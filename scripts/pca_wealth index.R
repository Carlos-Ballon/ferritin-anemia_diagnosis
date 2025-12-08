# PCA - Wealth Index

# Loading packages
pacman::p_load(
  tidyverse,
  rio,
  here,
  factoextra
)

# Drop missing values
data <- data |>
  drop_na(P018, P019, P020, P021, P023, P024, P025, P026, P027)

# Reemplazar los valores "77" por NA
vars77 <- c("P018", "P019", "P020", "P021", "P023", "P024", "P025", "P026", "P027")
data <- data |>
  mutate(across(all_of(vars77), ~ na_if(.x, 77))) |>
  drop_na(all_of(vars77))

# Paredes
data <- data |>
  mutate(walls_ord = case_when(
    P018 == 1 ~ 0,
    P018 %in% c(2, 5, 7) ~ 1,
    P018 == 3 ~ 2,
    P018 %in% c(4, 6, 8, 9) ~ 3
  ))

# Techos
data <- data |>
  mutate(roof_ord = case_when(
    P019 %in% c(1, 3) ~ 0,
    P019 == 4 ~ 1,
    P019 == 2 ~ 2,
    P019 %in% c(5, 6) ~ 3
  ))

# Pisos
data <- data |>
  mutate(floor_ord = case_when(
    P020 %in% c(1, 2, 3) ~ 0,
    P020 == 4 ~ 1,
    P020 == 5 ~ 2,
    P020 %in% c(6, 7) ~ 3
  ))

# Agua
data <- data |>
  mutate(water_ord = case_when(
    P021 %in% c(1, 2) ~ 0,
    P021 %in% c(3, 9, 10) ~ 1,
    P021 %in% c(4, 5, 6, 7) ~ 2
  ))

# Baño
data <- data |>
  mutate(toilet_ord = case_when(
    P023 == 1 ~ 0,
    P023 == 2 ~ 1,
    P023 %in% c(3, 4, 5) ~ 2,
    P023 %in% c(6, 7) ~ 3
  ))

# Alumbrado
data <- data |>
  mutate(light_ord = case_when(
    P024 == 1 ~ 0,
    P024 %in% c(0, 2, 3) ~ 1,
    P024 == 4 ~ 2
  ))

# Combustible
data <- data |>
  mutate(fuel_ord = case_when(
    P025 %in% c(1, 2) ~ 0,
    P025 == 3 ~ 1,
    P025 %in% c(4, 5) ~ 2
  ))

# Select PCA variables and remove missing values
assets_pca <- data |>
  dplyr::select(walls_ord, roof_ord, floor_ord, water_ord, toilet_ord, light_ord, fuel_ord, P026, P027) |>
  drop_na()

# PCA with prcomp
pca_result <- prcomp(assets_pca, center = TRUE, scale. = TRUE)

# Puntaje del primer componente
data <- data |>
  mutate(
    wealth_pc1_raw = predict(pca_result)[, 1:2], # Puntaje bruto del primer componente
    wealth_pc1 = -1 * wealth_pc1_raw, # Puntaje invertido (mayor valor = mayor riqueza)
    wealth_std = scale(wealth_pc1)[, 1] # Puntaje estandarizado (scaled)
  )

# Quintiles (0%, 20%, 40%, 60%, 80%, 100%)
data <- data |>
  mutate(
    wealth_5 = cut(
      wealth_std,
      breaks = quantile(wealth_std, probs = seq(0, 1, by = 0.2), na.rm = TRUE),
      include.lowest = TRUE,
      labels = FALSE
    ),
    wealth_5 = factor(
      wealth_5,
      levels = 1:5,
      labels = c("Muy pobre", "Pobre", "Medio", "Rico", "Muy rico")
    )
  )

# Terciles (0%, 40%, 60%, 100%)
data <- data |>
  mutate(
    wealth_3 = case_when(
      wealth_5 %in% c("Muy pobre", "Pobre") ~ 1,
      wealth_5 == "Medio" ~ 2,
      wealth_5 %in% c("Rico", "Muy rico") ~ 3
    ),
    wealth_3 = factor(
      wealth_3,
      levels = 1:3,
      labels = c("Pobre", "Medio", "Rico")
    )
  )

# Visualize the proportion of variance explained by the principal dimensions
fviz_eig(
  pca_result,
  addlabels = TRUE,
  choice = "variance",
  barfill = "#99CC00FF",
  barcolor = "#99CC00FF",
  xlab = "New dimensions"
)

var <- get_pca_var(pca_result)

# Coordinates for the variables
head(var$coor[order(var$cor[, 1], decreasing = TRUE),], 5)

# Correlations between variables and dimensions
head(var$cor[order(var$cor[, 1], decreasing = TRUE),], 5)

# Quality of representation (Cos2) for the variables on the dimensions
head(var$cos2[order(var$cor[, 1], decreasing = TRUE),], 5)

# Tablas cruzadas para validación
table(data$wealth_5, data$water_ord)
table(data$wealth_5, data$walls_ord)
table(data$wealth_5, data$toilet_ord)
table(data$wealth_5, data$fuel_ord)

# Promedios por quintil
data |>
  group_by(wealth_5) |>
  summarise(
    mean_habs_total = mean(P026, na.rm = TRUE),
    mean_habs_dormir = mean(P027, na.rm = TRUE)
  )