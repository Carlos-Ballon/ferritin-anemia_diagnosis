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
  # Poisson regression
  m_pois <- glm(formula, family = poisson(link = "log"), data = data)
  
  # Try using Poisson coefficients as a starting point for log-binomial regression
  m_bin  <- try(glm(
    formula,
    family = binomial(link = "log"),
    data = data,
    start = coef(m_pois)
  ),
  silent = TRUE)
  
  # Message
  if (inherits(m_bin, "try-error")) {
    # If it fails, it notifies that it used the Poisson model
    message("NO CONVERGIÓ: El modelo Log-Binomial falló. Se devolvió el modelo Poisson.")
    return(m_pois)
  } else {
    # If it works, it confirms the success of the log-binomial model
    message("CONVERGIÓ: El modelo Log-Binomial se ajustó correctamente.")
    return(m_bin)
  }
}

# Robust variance "HC1"
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
    bold_p(t = 0.05)
}

# Function to extract data from a `gtsummary` object
extract_forest_data <- function(table_obj, outcome_name, group_name) {
  table_obj$table_body |>
    filter(!is.na(conf.low)) |> # Eliminar filas de referencia y etiquetas vacías
    dplyr::select(label, estimate, conf.low, conf.high, p.value) |>
    mutate(Outcome = outcome_name, Etnia = group_name)
}

# Cochran-Mantel-Haenszel Test function for tbl_summary
test_tendencia_ordinal <- function(data, variable, by, ...) {
  # 'by' is hematological_states (nominal), 'variable' is ordinal (wealth_3 / education)
  df_temp <- data |>
    mutate(
      v_nominal = as.factor(.data[[by]]),
      v_ordinal = as.factor(.data[[variable]]) # Debe venir como ordered de afuera
    )
  res <- coin::cmh_test(v_nominal ~ v_ordinal, data = df_temp)
  # Return of a structured tibble
  tibble(
    p.value = coin::pvalue(res),
    method = "Cochran-Mantel-Haenszel Test"
  )
}