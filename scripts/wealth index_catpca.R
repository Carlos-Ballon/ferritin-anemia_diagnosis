# ==============================================================================
# DATA PREPARATION FOR CATPCA
# ==============================================================================

# Select, recode, and handle missing values
data_catpca <- data |>
  # Variable selection
  dplyr::select(ID1, P018:P021, P023:P027) |>
  mutate(
    # Replace specific values with NA
    across(P018:P027, ~ na_if(.x, 77)),
    P024 = na_if(P024, 3),
    # Adjust to the maximum observed value
    P023 = case_match(P023, 7 ~ 6, .default = P023),
    P021 = case_match(P021, 9 ~ 8, 10 ~ 9, .default = P021),
    # Collapse categories with low frequency
    P026 = case_when(P026 <= 1 ~ 1, P026 >= 6 ~ 6, TRUE ~ P026),
    P027 = case_when(P027 >= 4 ~ 4, TRUE ~ P027),
    # Reverse ordinal scale safely (P018 to P025)
    across(any_of(paste0("P0", 18:25)), ~ (max(.x, na.rm = TRUE) + min(.x, na.rm = TRUE)) - .x),
    # Impute missing values with the median of each variable
    across(P018:P027, ~ if_else(is.na(.x), as.integer(median(.x, na.rm = TRUE)), as.integer(.x)))
  )

# Save the IDs in a separate vector
vector_ids <- data_catpca$ID1

# Select only the variables to be included in the CATPCA model
data_catpca <- data_catpca |> dplyr::select(-ID1)

# ==============================================================================
# CHECK INTERNAL CONSISTENCY
# ==============================================================================
psych::alpha(data_catpca)

# Remove P024 and P025 as planned
data_catpca <- data_catpca |> 
  dplyr::select(-c(P024, P025))

# Check internal consistency measures without P024 and P025
internal_consistency <- psych::alpha(data_catpca)

# Reliability if an item is removed
table_S5 <- internal_consistency$alpha.drop |> 
  as.data.frame() |>
  rownames_to_column(var = "ID1") |>
  mutate(across(where(is.numeric), \(x) round(x, digits = 3))) |>
  flextable() |>
  fontsize(size = 10, part = "all") |>
  align(align = "center", part = "all") |>
  align(j = 1, align = "left", part = "body") |>
  padding(padding.top = 1, padding.bottom = 1, part = "all") |>
  set_table_properties(layout = "autofit")

# Item statistics
table_S6 <- internal_consistency$item.stats |>
  as.data.frame() |>
  rownames_to_column(var = "ID1") |>
  mutate(across(where(is.numeric), \(x) round(x, digits = 3))) |>
  flextable() |>
  fontsize(size = 10, part = "all") |>
  align(align = "center", part = "all") |>
  align(j = 1, align = "left", part = "body") |>
  padding(padding.top = 1, padding.bottom = 1, part = "all") |>
  set_table_properties(layout = "autofit")

# ==============================================================================
# ALIGNMENT OF THE FIRST COMPONENT (EQUAL-WEIGHT INDEX)
# ==============================================================================
indice_pesos_iguales <- data_catpca |>
  # Minimum-maximum scaling (0 to 1) and calculation of the Equal-Weight Index
  mutate(across(everything(), ~ {
    min_val <- min(.x, na.rm = TRUE)
    max_val <- max(.x, na.rm = TRUE)
    if (max_val == min_val) 0 else (.x - min_val) / (max_val - min_val)
  })) |>
  # Calculate the average of each row
  rowMeans(na.rm = TRUE) * 100

# ==============================================================================
# CATPCA (CATEGORICAL PCA BY OPTIMAL SCALING)
# ==============================================================================

# Ordinal CATPCA
catpca_model <- princals(data_catpca, ndim = 2, ordinal = TRUE, verbose = FALSE)
summary(catpca_model)

# Cronbach’s Alpha
k <- ncol(data_catpca)
lambda1 <- catpca_model$evals[1]
cronbach_alpha <- (k / (k - 1)) * (1 - (1 / lambda1))
print(paste("Alfa de Cronbach para el índice de riqueza:", round(cronbach_alpha, 3)))

# ==============================================================================
# OBJECT SCORES OF INDIVIDUALS IN DIM 1
# ==============================================================================
# If Dim1 is negatively correlated with the Equal-Weight Index, multiply by -1 to invert its sign so that higher values represent higher wealth index.

# Object scores: Coordinates of objects (individuals/observations) in principal component space
scores <- catpca_model$objectscores |>
  as.data.frame() |>
  rename(Dim1 = D1, Dim2 = D2) |>
  mutate(
    # ID assignment
    ID1 = vector_ids,
    # If the correlation is negative, multiply the object score by -1
    Dim1 = if (cor(Dim1, indice_pesos_iguales, use = "complete.obs") < 0) -Dim1 else Dim1,
    # Re-scale index to 0–100
    wealth_index_0_100 = (Dim1 - min(Dim1)) / (max(Dim1) - min(Dim1)) * 100,
    # Rankings (1 = Richest)
    ranking_CATPCA = rank(-round(wealth_index_0_100, 1), ties.method = "min")
  )

# Check directionality (It was negative)
cor(scores$Dim1, indice_pesos_iguales, use = "complete.obs", method = "spearman")

# Sort the rankings
score_rankings <- scores |> arrange(ranking_CATPCA, ID1)
head(score_rankings, 10)
tail(score_rankings, 10)

# test[score_rankings$ID1 == 114, ]

# ==============================================================================
# EIGENVALUES
# ==============================================================================
catpca_model$evals

# Plot
plot(catpca_model, plot.type = "screeplot")

# ==============================================================================
# OBJECT SCORES OF INDIVIDUALS FOR EACH VARIABLE IN DIM 1
# ==============================================================================
catpca_model$scoremat

# Correlation between the P018 object scores and the Equal-Weight Index
correlation_direction <- cor(catpca_model$scoremat[, 1], indice_pesos_iguales, use = "complete.obs")

# ==============================================================================
# LOADINGS
# ==============================================================================
catpca_model$loadings

# Loadings and contributions
loadings <- catpca_model$loadings |> 
  as.data.frame() |> 
  rename(Load_Dim1 = 1, Load_Dim2 = 2) |> 
  mutate(
    Variable = rownames(catpca_model$loadings),
    # If the correlation is negative, multiply the Loading by -1
    Load_Dim1 = if(correlation_direction < 0) -Load_Dim1 else Load_Dim1,
    # Square to obtain the explained variance (Least squares captured by variable)
    Var_Dim1 = Load_Dim1^2,
    Var_Dim2 = Load_Dim2^2,
    # Relative percentage with respect to the total of the dimension
    Contrib_Dim1 = round((Var_Dim1 / sum(Var_Dim1)) * 100, 2),
    Contrib_Dim2 = round((Var_Dim2 / sum(Var_Dim2)) * 100, 2),
    # Residual loss by variable in Dim1 (1 - Variance)
    Perdida_Dim1 = 1 - Var_Dim1
  )

# Table
table_S7 <- loadings |>
  mutate(across(where(is.numeric), ~ round(.x, 3))) |>
  select(Variable, Load_Dim1, Load_Dim2, Var_Dim1, Contrib_Dim1, Perdida_Dim1) |>
  flextable() |> 
  set_header_labels(
    Variable = "Variable",
    Load_Dim1 = "Loading Dim 1",
    Load_Dim2 = "Loading Dim 2",
    Var_Dim1 = "Explained variance\n(R-Squared)",
    Contrib_Dim1 = "Contribution to Dim 1",
    Perdida_Dim1 = "ALS-related loss"
  ) |> 
  fontsize(size = 10, part = "all") |>
  align(align = "center", part = "all") |>
  align(j = 1, align = "left", part = "body") |>
  padding(padding.top = 1, padding.bottom = 1, part = "all") |>
  set_table_properties(layout = "autofit")

# Loading plot without sign inversion
plot(catpca_model, plot.type = "loadplot", main = "Cargas de variables en las dos primeras dimensiones")

# Loading plot with sign inversion
inv_factor <- if(correlation_direction < 0) -1 else 1

# Plot
FS1 <- ggplot(data = loadings, aes(x = 0, y = 0, xend = Load_Dim1, yend = Load_Dim2)) +
  # Loadings (flechas)
  geom_segment(
    arrow = arrow(length = unit(0.2, "cm")),
    color = "tomato",
    linewidth = 0.8
  ) +
  # Variable labels (loadings)
  ggrepel::geom_text_repel(
    aes(x = Load_Dim1, y = Load_Dim2, label = Variable),
    size = 4
  ) +
  # Reference lines at the origin (0,0)
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray60") +
  labs(
    title = "CATPCA Loading Plot",
    x = paste0("PC1 (", if(inv_factor == -1) "Inverted - " else "", "Wealth Index)"),
    y = "PC2"
  ) +
  # Force a 1:1 ratio
  coord_fixed() +
  theme_minimal() +
  theme(axis.text = element_text(colour = "black"), axis.title = element_text(colour = "black")) 

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

# Plot
biplot_catpca <- ggplot() +
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
    title = "CATPCA Biplot ",
    x = paste0("PC1 (", if(inv_factor == -1) "Inverted - " else "", "Wealth Index)"),
    y = "PC2"
  ) +
  
  # Force a 1:1 ratio
  coord_fixed() +
  theme_minimal() +
  
  # Theme
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 14)
  )

# ==============================================================================
# CATEGORY QUANTIFICATIONS (COUNTS)
# ==============================================================================
catpca_model$quantifications

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
FS2 <- ggplot(
  data = quantification_table, 
  aes(x = factor(order_oriented), y = reference_quantification, color = Variable, group = Variable)
  ) +
  geom_line(linewidth = 0.8, alpha = 0.8) +
  geom_point(size = 2.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
  labs(
    x = "Category (Ordered)",
    y = "Reference Quantification (Inverted PC1)"
  ) +
  guides(col = guide_legend(ncol = 7)) +
  theme_classic() +
  scale_color_bmj() +
  theme(
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 11, face = "bold"),
    legend.position = "top",
    legend.title = element_blank(),
  )

# ==============================================================================
# COMPARISON WITH EQUAL WEIGHTS (EW)
# ==============================================================================
df_ew <- data.frame(ID1 = rownames(data_catpca), Indice_EW = indice_pesos_iguales) |>
  mutate(
    # Ranking EW
    Rank_EW = rank(-round(Indice_EW, 1), ties.method = "min")
  )

# Table
tabla_sensibilidad <- df_ew |>
  # Retrieve only the necessary columns from the 'scores' dataframe
  left_join(
    scores |> dplyr::select(ID1, Indice_CATPCA = wealth_index_0_100, Rank_CATPCA = ranking_CATPCA),
    by = "ID1"
  ) |>
  mutate(
    Indice_EW = round(Indice_EW, 1),
    Indice_CATPCA = round(Indice_CATPCA, 1),
    Cambio_rank = Rank_EW - Rank_CATPCA
  ) |>
  arrange(Rank_CATPCA, ID1) |>
  dplyr::select(ID1, Indice_EW, Rank_EW, Indice_CATPCA, Rank_CATPCA, Cambio_rank)

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

# ==============================================================================
# OPTIMAL SCALING TRANSFORMATIONS
# ==============================================================================

# Transformation plot
png(
  filename = here("outputs", "Figure_S3_transformation plot.png"),
  width = 7, 
  height = 7, 
  units = "in", 
  res = 600
)
par(mar = c(2, 2, 2, 2))
plot(catpca_model, plot.type = "transplot")
dev.off()

# ==============================================================================
# MERGE
# ==============================================================================

data <- data |>
  # Cruce relacional 100% seguro por ID1
  left_join(
    scores |> dplyr::select(ID1, Dim1), 
    by = "ID1"
  ) |>  
  mutate(
    # Quintiles basados en la Dim1 localmente acoplada
    wealth_5 = cut(
      Dim1,
      breaks = quantile(Dim1, probs = seq(0, 1, by = 0.2), na.rm = TRUE),
      include.lowest = TRUE,
      labels = FALSE
    ),
    wealth_5 = factor(
      wealth_5,
      levels = 1:5,
      labels = c("Poorest", "Poor", "Middle", "Rich", "Richest")
    ),
    
    # Terciles
    wealth_3 = cut(
      Dim1,
      breaks = quantile(Dim1, probs = seq(0, 1, by = 1/3), na.rm = TRUE),
      include.lowest = TRUE,
      labels = FALSE
    ),
    wealth_3 = factor(
      wealth_3,
      levels = 1:3,
      labels = c("Poor", "Middle", "Richest")
    )
  )

# NOTE: The crucial assumption in the Filmer and Pritchett (2001) study is that a household's long-run wealth explains the maximum variance (and covariance) in the asset variables. From: Tareq M, Abdel-Razzaq AI, Rahman MA, Choudhury T. Comparison of weighted and unweighted methods of wealth indices for assessing SOCIO-ECONOMIC status. Heliyon. 2021 Feb 26;7(2):e06163. doi: 10.1016/j.heliyon.2021.e06163.

# (alpha, estabilidad de la matriz de correlacion, regresion con minimos cuadrados alternantes, componentes rotados o principales)