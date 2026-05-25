# NOTE: The crucial assumption in the Filmer and Pritchett (2001) study is that a household's long-run wealth explains the maximum variance (and covariance) in the asset variables. https://doi.org/10.1016/j.heliyon.2021.e06163

# Select PCA variables, recode, and handle missing values
data_catpca <- data |>
  # Variable selection for the wealth index
  dplyr::select(P018:P021, P023:P027) |>
  
  # Replace specific values with NA
  mutate(
    across(everything(), ~ na_if(.x, 77)),
    P024 = na_if(P024, 3)
  ) |>
  
  # Recode, collapse, and reverse scales
  mutate(
    # Collapse categories with low frequency
    P026 = case_when(P026 <= 1 ~ 1, P026 >= 6 ~ 6, TRUE ~ P026),
    P027 = case_when(P027 >= 4 ~ 4, TRUE ~ P027),
    
    # Reverse ordinal scale safely (P018 to P025)
    across(any_of(paste0("P0", 18:25)), ~ (max(.x, na.rm = TRUE) + min(.x, na.rm = TRUE)) - .x)
  )

# Internal consistency check
psych::alpha(data_catpca)

# Remove P024 and P025 as planned
data_catpca <- data_catpca |>
  dplyr::select(-c(P024, P025))

# Oriented ordinal matrix
X_ord <- data.frame(lapply(data_catpca, as.numeric))
rownames(X_ord) <- rownames(data_catpca)

# Internal consistency check
psych::alpha(data_catpca)

# CATPCA (categorical PCA by Optimal Scaling)
catpca_model <- princals(data_catpca, ndim = 2, ordinal = TRUE, verbose = FALSE)
summary(catpca_model)

# Alfa de Cronbach
k <- ncol(data_catpca)
lambda1 <- catpca_model$evals[1]
cronbach_alpha <- (k / (k - 1)) * (1 - (1 / lambda1))
print(paste("Alfa de Cronbach para el índice de riqueza:", round(cronbach_alpha, 3)))

# Eigenvalues
catpca_model$evals

# Object scores: Coordinates of objects (individuals/observations) in principal component space
scores <- catpca_model$objectscores |>
  as.data.frame() |>
  rename(Dim1 = D1, Dim2 = D2)
scores$ID <- rownames(data_catpca)

# Loadings
catpca_model$loadings

# Category quantifications (counts)
catpca_model$quantifications

# Alignment of the first component

## Create Equal-Weight Index for alignment direction
X01 <- as.data.frame(lapply(X_ord, function(x) {
  min_val <- min(x, na.rm = TRUE)
  max_val <- max(x, na.rm = TRUE)
  if (max_val == min_val) return(rep(0, length(x)))
  
  return((x - min_val) / (max_val - min_val))
}))

indice_pesos_iguales <- rowMeans(X01, na.rm = TRUE) * 100

# Check directionality: if Dim1 is negatively correlated with the proxy index,
# multiply by -1 to invert its sign so that higher values represent higher wealth.
cor(scores$Dim1, indice_pesos_iguales, use = "complete.obs", method = "spearman")
 
# Apply inversion to scores
scores <- scores |>
  mutate(Dim1 = if (cor(Dim1, indice_pesos_iguales, use = "complete.obs") < 0) -Dim1 else Dim1)

# Calculate Wealth Index, Rankings and sort the final dataset
scores <- scores |>
  mutate(
    # Re-scale index to 0–100
    wealth_index_0_100 = (Dim1 - min(Dim1)) / (max(Dim1) - min(Dim1)) * 100,
    # Rankings (1 = Richest)
    ranking_CATPCA = rank(-round(wealth_index_0_100, 1), ties.method = "min")
  ) |>
  arrange(ranking_CATPCA, ID)

head(scores, 10)
tail(scores, 10)

# head(scores, 10)
# test <- X_ord |> mutate(ID = row.names(X_ord)) |> arrange(P018, ID)
# test[test$ID == 114, ]

# Object scores by variable (scoremat): Object scores calculated independently for each variable
catpca_model$scoremat

# Correlation between the P0181 score and the Equal-Weight Index
correlation_direction <- cor(catpca_model$scoremat[, 1], indice_pesos_iguales, use = "complete.obs")

# Loadings (contributions)
loadings <- catpca_model$loadings |> 
  as.data.frame() |> 
  rename(Load_Dim1 = 1, Load_Dim2 = 2) |> 
  mutate(
    Variable = rownames(catpca_model$loadings),
    # If the correlation was negative, multiply the Loading by -1
    Load_Dim1 = if(correlation_direction < 0) -Load_Dim1 else Load_Dim1,
    # Square to obtain the variance
    Var_Dim1 = Load_Dim1^2,
    Var_Dim2 = Load_Dim2^2,
    # Relative percentage with respect to the total of the dimension
    Contrib_Dim1 = round((Var_Dim1 / sum(Var_Dim1)) * 100, 2),
    Contrib_Dim2 = round((Var_Dim2 / sum(Var_Dim2)) * 100, 2)
  ) |>
  select(Variable, Load_Dim1, Load_Dim2, Contrib_Dim1, Contrib_Dim2)

# Loading plot without sign inversion
plot(catpca_model, plot.type = "loadplot", main = "Cargas de variables en las dos primeras dimensiones")

# Loading plot with sign inversion
inv_factor <- if(correlation_direction < 0) -1 else 1

ggplot(data = loadings, aes(x = 0, y = 0, xend = Load_Dim1, yend = Load_Dim2)) +
  # Loadings (flechas)
  geom_segment(
    arrow = arrow(length = unit(0.2, "cm")),
    color = "dodgerblue",
    linewidth = 0.8
  ) +
  # Variable labels (loadings)
  geom_text(
    aes(x = Load_Dim1, y = Load_Dim2, label = Variable),
    vjust = -0.5,
    hjust = 0.5,
    fontface = "bold",
    size = 4
  ) +
  # Reference lines at the origin (0,0)
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray60") +
  labs(
    title = "Cargas de variables en las dos primeras dimensiones (Invertido)",
    x = paste0("Dimensión 1 (", if(inv_factor == -1) "Invertida - " else "", "Riqueza)"),
    y = "Dimensión 2"
  ) +
  # Force a 1:1 ratio
  coord_fixed() +
  theme_minimal()

# Biplot without sign inversion
plot(catpca_model, plot.type = "biplot", col.loadings = "coral3", col.scores = "lightgrey")
abline(h = 0, v = 0, lty = 2)

# Biplot with sign inversion
max_score <- max(abs(scores$Dim1), na.rm = TRUE) # Expansion Factor
scaler <- max_score * 0.75 # A good standard is that arrows occupy approximately 70-80% of the score range

df_loadings <- loadings |> 
  mutate(
    Dim1_scaled = Load_Dim1 * scaler,
    Dim2_scaled = Load_Dim2 * scaler
  )

ggplot() +
  # Scores
  geom_point(data = scores, aes(x = Dim1, y = Dim2), color = "lightgrey", alpha = 0.6, size = 1.2) +
  
  # Loadings
  geom_segment(
    data = df_loadings,
    aes(x = 0, y = 0, xend = Dim1_scaled, yend = Dim2_scaled),
    arrow = arrow(length = unit(0.25, "cm")),
    color = "tomato",
    linewidth = 0.8
  ) +
  
  # Variable labels (loadings)
  geom_text(
    data = df_loadings,
    aes(x = Dim1_scaled * 1.1, y = Dim2_scaled * 1.1, label = Variable),
    color = "tomato",
    fontface = "bold",
    size = 4
  )+
  
  # Reference lines at the origin (0,0)
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray60") +
  
  # Plot labels
  labs(
    title = "Biplot CATPCA",
    x = paste0("Dimensión 1 (", if(inv_factor == -1) "Invertida - " else "", "Riqueza)"),
    y = "Dimensión 2"
  ) +
  
  # Force a 1:1 ratio
  coord_fixed() +
  theme_minimal() +
  
  # Theme
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 14)
  )

# Category quantifications
quantification_table <- bind_rows(
  lapply(catpca_model$quantifications, function(x) {
    as.data.frame(x) |> rownames_to_column("Categoria")
  }), 
  .id = "Variable"
) |>
  mutate(
    reference_quantification = round(D1 * inv_factor, 3),
    order_oriented = as.numeric(Categoria)
  ) |>
  select(Variable, Categoria, order_oriented, reference_quantification) |>
  arrange(Variable, order_oriented)

quantification_table |> dplyr::filter(Variable == "P018")

# Plot of Oriented Ordinal Quantifications
ggplot(
  data = quantification_table, 
  aes(x = factor(order_oriented), y = reference_quantification, color = Variable, group = Variable)
  ) +
  geom_line(linewidth = 0.8, alpha = 0.8) +
  geom_point(size = 2.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
  labs(
    title = "Cuantificaciones ordinales orientadas",
    subtitle = "Comportamiento no lineal de las categorías por variable",
    x = "Categoría (Orden Orientado)",
    y = "Cuantificación de referencia (Dim 1 Invertida)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 10, face = "bold"),
    axis.text.y = element_text(size = 10),
    axis.title.y = element_text(size = 11, face = "bold"),
    plot.title = element_text(face = "bold", size = 13, hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "gray90")
  )

# Comparison with Equal Weights (EW)
df_ew <- data.frame(ID = rownames(X_ord), Indice_EW = indice_pesos_iguales) |>
  mutate(
    # Ranking EW
    Rank_EW = rank(-round(Indice_EW, 1), ties.method = "min")
  )

tabla_sensibilidad <- df_ew |>
  # Retrieve only the necessary columns from the 'scores' dataframe
  left_join(
    scores |> dplyr::select(ID, Indice_CATPCA = wealth_index_0_100, Rank_CATPCA = ranking_CATPCA),
    by = "ID"
  ) |>
  mutate(
    Indice_EW = round(Indice_EW, 1),
    Indice_CATPCA = round(Indice_CATPCA, 1),
    Cambio_rank = Rank_EW - Rank_CATPCA
  ) |>
  arrange(Rank_CATPCA, ID) |>
  dplyr::select(ID, Indice_EW, Rank_EW, Indice_CATPCA, Rank_CATPCA, Cambio_rank)

head(tabla_sensibilidad, 15)

# Comparison between EW index and CATPCA index
ggplot(tabla_sensibilidad, aes(x = Indice_EW, y = Indice_CATPCA)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray50", linewidth = 0.8) +
  geom_point(color = "tomato", size = 2, alpha = 0.7) +
  labs(
    title = "Sensibilidad: pesos iguales vs CATPCA",
    x = "Índice con pesos iguales",
    y = "Índice CATPCA") +
  coord_fixed(ratio = 1, xlim = c(0, 100), ylim = c(0, 100)) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.grid.minor = element_blank())

#  Wealth index categorization and merge
data <- data |>
  mutate(
    # Quintiles (0%, 20%, 40%, 60%, 80%, 100%)
    wealth_5 = cut(
      scores$Dim1,
      breaks = quantile(scores$Dim1, probs = seq(0, 1, by = 0.2), na.rm = TRUE),
      include.lowest = TRUE,
      labels = FALSE
    ),
    wealth_5 = factor(
      wealth_5,
      levels = 1:5,
      labels = c("Muy pobre", "Pobre", "Medio", "Rico", "Muy rico")
    ),
    # Terciles (0%, 33%, 66%, 100%)
    wealth_3 = cut(
      scores$Dim1,
      breaks = quantile(scores$Dim1, probs = seq(0, 1, by = 1/3), na.rm = TRUE),
      include.lowest = TRUE,
      labels = FALSE
    ),
    wealth_3 = factor(
      wealth_3,
      levels = 1:3,
      labels = c("Pobre", "Medio", "Rico")
    )
  )
