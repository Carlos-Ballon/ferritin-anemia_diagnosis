# ------------------------------------------------------------
# Unidad 2: Analisis de Componentes Principales para indices
# Guia de laboratorio en R
# Caso: indice ACP de desempeno comercial de sucursales
# Autor: Andres Galvis
# ------------------------------------------------------------

# 1. Lectura de datos ------------------------------------------------------
datos <- read.csv("datos_sucursales_acp_u2.csv", stringsAsFactors = FALSE)

# Inspeccion inicial
str(datos)
summary(datos)
head(datos)

# 2. Definicion de variables activas --------------------------------------
vars_activas <- c(
  "ventas_mensuales_usd",
  "margen_operativo_pct",
  "clientes_activos",
  "conversion_pct",
  "retencion_pct",
  "satisfaccion_nps",
  "reclamos_por_1000",
  "devoluciones_pct",
  "tiempo_entrega_dias",
  "horas_capacitacion"
)

vars_negativas <- c(
  "reclamos_por_1000",
  "devoluciones_pct",
  "tiempo_entrega_dias"
)

X <- datos[, vars_activas]

# 3. Orientacion y normalizacion min-max ----------------------------------
# Todas las variables quedan orientadas de forma que valores altos signifiquen
# mejor desempeno para el futuro indice.

normaliza_orientada <- function(x, negativa = FALSE) {
  xmin <- min(x, na.rm = TRUE)
  xmax <- max(x, na.rm = TRUE)
  if (xmax == xmin) stop("La variable es constante; no puede normalizarse.")
  if (negativa) {
    return((xmax - x) / (xmax - xmin))
  } else {
    return((x - xmin) / (xmax - xmin))
  }
}

Z <- as.data.frame(
  lapply(names(X), function(v) {
    normaliza_orientada(X[[v]], negativa = v %in% vars_negativas)
  })
)
names(Z) <- names(X)

# Verificacion: rango 0-1
sapply(Z, range)

# Guardar matriz Z
matriz_Z <- cbind(id_sucursal = datos$id_sucursal, Z)
write.csv(matriz_Z, "matriz_Z_orientada_u2_acp.csv", row.names = FALSE)

# 4. Analisis exploratorio de correlaciones -------------------------------
R <- cor(Z)
round(R, 2)

# Promedio de correlaciones absolutas fuera de la diagonal
R_abs <- abs(R)
mean(R_abs[upper.tri(R_abs)])

# 5. ACP con prcomp --------------------------------------------------------
# center=TRUE y scale.=TRUE implican ACP sobre la matriz de correlaciones
# de los indicadores orientados y normalizados.

acp <- prcomp(Z, center = TRUE, scale. = TRUE)

valores_propios <- acp$sdev^2
prop_var <- valores_propios / sum(valores_propios)
var_acum <- cumsum(prop_var)

tabla_varianza <- data.frame(
  componente = paste0("CP", seq_along(valores_propios)),
  valor_propio = round(valores_propios, 3),
  varianza_pct = round(100 * prop_var, 2),
  acumulada_pct = round(100 * var_acum, 2)
)
print(tabla_varianza)

# 6. Cargas factoriales/correlaciones variable-componente -----------------
# Para prcomp, las correlaciones entre variables estandarizadas y componentes
# se obtienen como rotation * sdev.

cargas <- sweep(acp$rotation, 2, acp$sdev, FUN = "*")

# Orientar CP1 para que cargas positivas indiquen mejor desempeno
if (sum(cargas[, 1]) < 0) {
  acp$x[, 1] <- -acp$x[, 1]
  acp$rotation[, 1] <- -acp$rotation[, 1]
  cargas[, 1] <- -cargas[, 1]
}

round(cargas[, 1:2], 3)

# 7. Construccion del indice ACP ------------------------------------------
score_cp1 <- acp$x[, 1]
indice_acp_0_100 <- 100 * (score_cp1 - min(score_cp1)) / (max(score_cp1) - min(score_cp1))

resultados <- data.frame(
  id_sucursal = datos$id_sucursal,
  region = datos$region,
  ciudad = datos$ciudad,
  canal = datos$canal,
  score_cp1 = round(score_cp1, 3),
  indice_acp_0_100 = round(indice_acp_0_100, 1),
  ranking = rank(-indice_acp_0_100, ties.method = "first")
)

resultados <- resultados[order(resultados$ranking), ]
print(head(resultados, 10))
print(tail(resultados, 10))

write.csv(resultados, "resultados_indice_acp_u2.csv", row.names = FALSE)

# 8. Graficos --------------------------------------------------------------

# Scree plot
png("u2_screeplot_acp.png", width = 900, height = 650, res = 120)
plot(seq_along(valores_propios), valores_propios, type = "b",
     xlab = "Componente principal", ylab = "Valor propio",
     main = "Scree plot del ACP")
abline(h = 1, lty = 2)
dev.off()

# Ranking del indice
png("u2_ranking_indice_acp.png", width = 1000, height = 700, res = 120)
barplot(rev(resultados$indice_acp_0_100[1:15]),
        names.arg = rev(resultados$id_sucursal[1:15]),
        horiz = TRUE, las = 1,
        xlab = "Indice ACP reescalado 0-100",
        main = "Top 15 sucursales segun indice ACP")
dev.off()

# Mapa de individuos CP1-CP2
png("u2_mapa_individuos_acp.png", width = 900, height = 700, res = 120)
plot(acp$x[,1], acp$x[,2],
     xlab = paste0("CP1 (", round(100*prop_var[1], 1), "%)"),
     ylab = paste0("CP2 (", round(100*prop_var[2], 1), "%)"),
     main = "Mapa de sucursales en el plano CP1-CP2",
     pch = 19)
text(acp$x[,1], acp$x[,2], labels = datos$id_sucursal, pos = 3, cex = 0.6)
abline(h = 0, v = 0, lty = 2)
dev.off()

# 9. Analisis de sensibilidad ---------------------------------------------
# Comparar indice ACP con indice de pesos iguales sobre Z.
indice_igual <- rowMeans(Z)
indice_igual_0_100 <- 100 * (indice_igual - min(indice_igual)) / (max(indice_igual) - min(indice_igual))
cor(indice_acp_0_100, indice_igual_0_100, method = "spearman")

sensibilidad <- data.frame(
  id_sucursal = datos$id_sucursal,
  indice_acp = round(indice_acp_0_100, 1),
  ranking_acp = rank(-indice_acp_0_100, ties.method = "first"),
  indice_igual = round(indice_igual_0_100, 1),
  ranking_igual = rank(-indice_igual_0_100, ties.method = "first")
)
sensibilidad$dif_ranking <- sensibilidad$ranking_igual - sensibilidad$ranking_acp
sensibilidad <- sensibilidad[order(abs(sensibilidad$dif_ranking), decreasing = TRUE), ]
head(sensibilidad, 10)
write.csv(sensibilidad, "sensibilidad_u2_acp_vs_pesos_iguales.csv", row.names = FALSE)

# 10. Cierre ---------------------------------------------------------------
# El indice ACP no es automaticamente mejor que un indice normativo.
# Debe justificarse por su capacidad de resumir informacion correlacionada,
# su coherencia conceptual y su estabilidad ante decisiones metodologicas.

