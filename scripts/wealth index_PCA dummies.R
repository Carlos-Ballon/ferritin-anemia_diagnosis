# Select PCA variables, recode, and handle missing values
vars77 <- c("P018", "P019", "P020", "P021", "P023", "P024", "P025", "P026", "P027")

data <- data |>
  # Replace specific values with NA
  mutate(
    across(all_of(vars77), ~ na_if(.x, 77)),
    P026 = if_else(P026 == 0, 1, P026, missing = P026),
    P024 = na_if(P024, 3)
  ) |>
  # Drop missing values
  # tidyr::drop_na(all_of(vars77))

# Wealth index estimate using dichotomous variables (Dummies)
data <- data |>
  mutate(
    # Paredes: Noble vs Tradicional vs Precario
    wall_noble = if_else(P018 %in% c(1, 2), 1, 0),
    wall_traditional = if_else(P018 %in% c(3, 4, 6), 1, 0), 
    wall_poor = if_else(P018 %in% c(5, 7, 8), 1, 0),
    
    # Techos: Duraderos vs Semi-duraderos vs Vulnerable
    roof_noble = if_else(P019 == 1, 1, 0),
    roof_basic = if_else(P019 %in% c(2, 3, 4), 1, 0),
    roof_poor  = if_else(P019 %in% c(5, 6), 1, 0),
    
    # Pisos: Acabados vs Pisos firmes vs Precarios/Naturales
    floor_noble = if_else(P020 %in% c(1, 2, 3), 1, 0),
    floor_basic = if_else(P020 %in% c(4, 5), 1, 0),
    floor_poor  = if_else(P020 %in% c(6, 7), 1, 0),
    
    # Aagua: Máxima inversión vs Acceso con esfuerzo/pago vs Sin tratamiento
    water_safe   = if_else(P021 %in% c(1, 2, 10), 1, 0), 
    water_access = if_else(P021 %in% c(3, 4, 5, 9), 1, 0), 
    water_unsafe = if_else(P021 %in% c(6, 7, 8), 1, 0),
    
    # Saneamiento: Seguro vs Compartido/Pasillo vs No seguro
    toilet_safe = if_else(P023 == 1, 1, 0),
    toilet_pub  = if_else(P023 == 2, 1, 0),
    toilet_unsafe = if_else(P023 %in% c(3, 4, 5, 6, 7), 1, 0),
    
    # Electricidad
    light_electric = if_else(P024 == 1, 1, 0),
    
    # Combustible: Limpio vs Biomasa
    fuel_clean = if_else(P025 %in% c(1, 2), 1, 0), # Electricidad, Gas
    fuel_wood  = if_else(P025 %in% c(4, 5), 1, 0), # Leña, Bosta
    
    # Habitaciones para dormir (privacidad vs hacinamiento).
    # Pasar de 0 a 1 dormitorio es un salto enorme en la calidad de vida
    # 0 dormitorios (Hogares con solo un ambiente multiusos) o 1 dormitorio exclusivo
    room_min = if_else(P027 %in% c(0, 1), 1, 0),
    
    # 2 o más dormitorios (Indicador de mayor riqueza)
    room_two_plus = if_else(P027 >= 2, 1, 0),
  )

# Select dummies
assets_pca <- data |>
  dplyr::select(
    wall_noble, roof_noble, roof_basic, floor_noble, floor_basic, water_safe,
    toilet_safe, light_electric, fuel_clean, room_min, room_two_plus) |>
  
  # Selecciona solo columnas donde la desviación estándar sea mayor a 0
  dplyr::select(where(~ sd(.x, na.rm = TRUE) > 0))

# PCA with prcomp
pca_result <- prcomp(assets_pca, center = TRUE, scale. = TRUE)

# Método de Filmer and Pritchett
# En la literatura económica, se asume que existe una variable latente llamada "Nivel Socioeconómico" que explica por qué un hogar tiene simultáneamente piso de parquet, agua por red y cocina a gas.

# Regla de oro: Usar el puntaje del PC1
# Si usas el PC2 para medir riqueza, estarías mezclando dimensiones distintas y tu índice perdería validez.
data <- data |>
  mutate(
    # PC1: Captura la mayor varianza común de todos los activos. Se interpreta como el bienestar o riqueza.
    wealth_index_raw = predict(pca_result)[, 1], 
    
    # PC2: Captura una dimensión de varianza que es independiente a la riqueza, como diferencias geográficas 
    urban_rural_dim = predict(pca_result)[, 2],
    
    wealth_pc1 = -1 * wealth_index_raw, # Puntaje invertido (mayor valor = mayor riqueza)
    wealth_std = scale(wealth_pc1)[, 1] # Puntaje estandarizado (scaled)
  )

# Wealth index categorization
data <- data |>
  mutate(
    # Quintiles (0%, 20%, 40%, 60%, 80%, 100%)
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
    ),
    
    # Opcional: Normalizar de 0 a 100
    wealth_index_0_100 = (wealth_index_raw - min(wealth_index_raw)) /
      (max(wealth_index_raw) - min(wealth_index_raw)) * 100,
    
    # Terciles (0%, 33%, 66%, 100%)
    wealth_3 = cut(
      wealth_std,
      breaks = quantile(wealth_std, probs = seq(0, 1, by = 1/3), na.rm = TRUE),
      include.lowest = TRUE,
      labels = FALSE
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
  barcolor = "#99CC00FF") +
  labs(
    title = "Varianza explicada por cada componente",
    x = "Componentes Principales",
    y = "Porcentaje de varianza explicada")

# Gráfico de contribuciones de las variables al PC1
fviz_contrib(
  pca_result, 
  choice = "var", 
  axes = 1, 
  fill = "steelblue", 
  color = "black",
  top = 15) + # Muestra las 15 variables más importantes
  labs(
    title = "Contribución de los activos al Índice de Riqueza (PC1)",
    x = "Activos / Variables", 
    y = "Porcentaje de contribución")

# Gráfico de las variables en el espacio de los componentes
fviz_pca_var(
  pca_result,
  col.var = "contrib", # El color indica la contribución
  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
  repel = TRUE) + # Evita que los textos se traslapen
  labs(
    title = "Círculo de Correlación: Dirección de los activos",
    color = "Contribución")

var <- get_pca_var(pca_result)

# Loadings / weights / eigenvectors
# Las variables con valores más altos son las que más "pesan" en tu índice de riqueza
pca_result$rotation[, 1]

# Eigenvalues
get_eigenvalue(pca_result)

# library(xlsx)
# df_para_excel <- as.data.frame(get_eigenvalue(pca_result))
# write.xlsx(df_para_excel, here("outputs", "pca_eigenvalues.xlsx"))

# Coordinates for the variables
head(var$coor[order(var$coor[, 1], decreasing = TRUE),], 5)

# Correlations between variables and dimensions
head(var$cor[order(var$cor[, 1], decreasing = TRUE),], 5)

# Quality of representation (Cos2) for the variables on the dimensions
head(var$cos2[order(var$cos2[, 1], decreasing = TRUE),], 5)

# Contributions of the variables on the dimensions
head(var$contrib[order(var$contrib[, 1], decreasing = TRUE),], 5)

# df_para_excel <- as.data.frame(var$contrib[order(var$contrib[, 1], decreasing = TRUE),])
# write.xlsx(df_para_excel, here("outputs", "pca_contributions.xlsx"))

# ¿Cómo interpretar tus resultados?
# En el gráfico de contribución: Si floor_earth o light_electric están muy arriba, significa que en tu muestra de datos, tener o no tener esos servicios es lo que más diferencia a un hogar pobre de uno rico.

# En el círculo de correlación: Si el vector de floor_earth apunta hacia la izquierda y el de floor_noble hacia la derecha, el eje X es tu gradiente de riqueza.

# Inversión de signos: Si ves que todas las variables "pobres" están a la derecha y las "ricas" a la izquierda, recuerda multiplicar tu índice por −1 como hicimos en el paso anterior para que los valores más altos representen mayor riqueza.

# La correlación entre P026 (habitaciones totales) y P027 (dormitorios) es de 0.88. 
# En estadística, esto significa que ambas variables están "contando la misma historia".
# No vale la pena incluir ambas, y es mejor usar las dummies que creaste para la P027.
# El PCA le dará un peso doble al tamaño de la casa si incluyes las dummies de P027 y P026.
# El índice estará sesgado: parecerá que el tamaño de la vivienda es mucho más importante que tener agua, luz o servicios higiénicos, simplemente porque el PCA encontró mucha varianza repetida ahí.
