# Univariable Log-Binomial regression models
tbl_crude <- function(data, x_vars, outcome) {
  data |>
    dplyr::select(all_of(c(x_vars, outcome))) |>
    tbl_uvregression(
      include = all_of(x_vars),
      method = glm,
      method.args = list(family = binomial(link = "log")),
      y = !!outcome,
      hide_n = TRUE,
      exponentiate = TRUE,
      conf.int = TRUE,
      conf.level = 0.95,
      pvalue_fun = ~ style_pvalue(.x, digits = 3),
      estimate_fun = ~ style_number(.x, digits = 2)
    ) |>
    bold_p(t = 0.05)
}

# Convergence models
fit_logbinomial_safe <- function(data, formula) {
  # Poisson
  m_pois <- glm(formula, family = poisson(link = "log"), data = data)
  
  # Intento de Log Binomial usando coeficientes de Poisson como inicio
  m_bin  <- try(glm(
    formula,
    family = binomial(link = "log"),
    data = data,
    start = coef(m_pois)
  ),
  silent = TRUE)
  # Evaluación y mensaje
  if (inherits(m_bin, "try-error")) {
    # Si falla, avisa que usó el modelo de Poisson
    message("NO CONVERGIÓ: El modelo Log-Binomial falló. Se devolvió el modelo Poisson.")
    return(m_pois)
  } else {
    # Si funciona, confirma el éxito del modelo Log-Binomial
    message("CONVERGIÓ: El modelo Log-Binomial se ajustó correctamente.")
    return(m_bin)
  }
}

# Varianza robusta "HC1"
tbl_robust <- function(model) {
  model |>
    tbl_regression(
      exponentiate = TRUE,
      conf.int = TRUE,
      conf.level = 0.95,
      tidy_fun = function(x, ...) broom.helpers::tidy_parameters(x, vcov = "HC1", ...),
      pvalue_fun = ~style_pvalue(.x, digits = 3),
      estimate_fun = ~style_number(.x, digits = 2)
    ) |>
    bold_labels() |> 
    bold_p(t = 0.05) |>
    modify_header(label = "**Caraterísticas**")
}

# Función para extraer datos de un objeto gtsummary
extract_forest_data <- function(table_obj, outcome_name, group_name) {
  table_obj$table_body |>
    filter(!is.na(conf.low)) |> # Eliminar filas de referencia y etiquetas vacías
    dplyr::select(label, estimate, conf.low, conf.high, p.value) |>
    mutate(Outcome = outcome_name, Etnia = group_name)
}