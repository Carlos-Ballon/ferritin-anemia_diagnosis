#### Efectos Marginales ####

margins_empirical_ci <- function(model, data, x_var, group_var, ci_level = 0.95) {
  
  data[[x_var]]     <- droplevels(as.factor(data[[x_var]]))
  data[[group_var]] <- droplevels(as.factor(data[[group_var]]))
  
  lv_x <- levels(data[[x_var]])
  lv_g <- levels(data[[group_var]])
  
  V <- vcov(model)
  b <- coef(model)
  z <- qnorm(1 - (1 - ci_level) / 2)
  
  grid <- tidyr::expand_grid(
    !!rlang::sym(x_var) := lv_x,
    !!rlang::sym(group_var) := lv_g
  )
  
  tt <- delete.response(terms(model))
  
  purrr::pmap_dfr(grid, function(...) {
    vals <- list(...)
    
    dnew <- data
    dnew[[x_var]]     <- factor(vals[[x_var]], levels = lv_x)
    dnew[[group_var]] <- factor(vals[[group_var]], levels = lv_g)
    
    X   <- model.matrix(tt, dnew)
    eta <- as.numeric(X %*% b)
    mu  <- exp(eta)
    
    est <- mean(mu, na.rm = TRUE)
    
    grad    <- colMeans(X * mu, na.rm = TRUE)
    var_est <- as.numeric(t(grad) %*% V %*% grad)
    se_est  <- sqrt(pmax(var_est, 0))
    
    low  <- pmax(est - z * se_est, 0)
    high <- pmin(est + z * se_est, 1)
    
    tibble(
      !!x_var := vals[[x_var]],
      !!group_var := vals[[group_var]],
      Estimate = est, Low = low, High = high
    )
  }) %>%
    mutate(
      !!x_var := factor(.data[[x_var]], levels = lv_x),
      !!group_var := factor(.data[[group_var]], levels = lv_g)
    )
}

#### Función para Gráfico ####

plot_margins_logbin <- function(df, outcome, letter, x_var, group_var = "etnia", covars,  ymax, x_lab) {
  
  needed <- c(outcome, x_var, group_var, covars)
  d <- df %>%
    select(all_of(needed)) %>%
    filter(if_all(all_of(needed), ~ !is.na(.x))) %>%
    mutate(
      across(all_of(c(x_var, group_var, covars)), ~ droplevels(as.factor(.x))),
      "{outcome}" := as.integer(.data[[outcome]])
    )
  
  if (x_var == "educacion") d[[x_var]] <- fct_relevel(d[[x_var]], "Primaria o ninguno", "Secundaria", "Superior")
  if (x_var == "wealth_3")  d[[x_var]] <- fct_relevel(d[[x_var]], "Pobre", "Medio", "Rico")
  
  fml <- as.formula(paste0(outcome, " ~ ", group_var, " * ", x_var, " + ", paste(covars, collapse = " + ")))
  mod <- fit_logbinomial_safe(fml, d)
  
  df_pred <- margins_empirical_ci(mod, d, x_var, group_var)
  
  pd <- position_dodge(width = 0.35)
  y_breaks <- seq(0, ymax, by = 0.05)
  
  ggplot(df_pred, aes(x = .data[[x_var]], y = Estimate,
                      color = .data[[group_var]], group = .data[[group_var]])) +
    geom_hline(yintercept = y_breaks, linetype = 2, color = "gray85", linewidth = 0.4) +
    geom_line(position = pd, linewidth = 0.5, linetype = "dashed") +
    geom_errorbar(aes(ymin = Low, ymax = High), position = pd, width = 0,
                  linewidth = 0.7, color = "gray75") +
    geom_point(position = pd, size = 3.5, shape = 19) +
    scale_y_continuous(limits = c(0, ymax), breaks = y_breaks,
                       labels = scales::percent_format(accuracy = 1),
                       expand = c(0, 0)) +
    scale_color_manual(values = c("Mestizo" = "dodgerblue", "Quechua/Aymara" = "red2")) +
    labs(x = x_lab,
         y = "Prevalencia predicha del desenlace\n",
         color = NULL,
         title = paste0(letter, ". ", tools::toTitleCase(gsub("_", " ", outcome)))) +
    theme_classic(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold"),
      axis.title = element_text(size = 12, face = "bold"),
      axis.text.x = element_text(size = 12, colour = "black"),
      axis.text.y = element_text(size = 12, colour = "black"),
      legend.position = "bottom"
    )
}

#### Generación de Figuras para Educación ####
pA_edu <- plot_margins_logbin(
  df = data,
  outcome = "anemia",
  letter = "A",
  x_var = "educacion",
  covars = c("sexo", "wealth_3", "DEP"),
  ymax = 0.75,
  x_lab = "\nNivel de Educación"
)

pB_edu <- plot_margins_logbin(
  df = data,
  outcome = "ferropenia",
  letter = "B",
  x_var = "educacion",
  covars = c("sexo", "wealth_3", "DEP"),
  ymax = 0.50,
  x_lab = "\nNivel de Educación"
)

pC_edu <- plot_margins_logbin(
  df = data,
  outcome = "anemia_no_ferropenica",
  letter = "C",
  x_var = "educacion",
  covars = c("sexo", "wealth_3", "DEP"),
  ymax = 0.60,
  x_lab = "\nNivel de Educación"
)

pD_edu <- plot_margins_logbin(
  df = data,
  outcome = "anemia_ferropenica",
  letter = "D",
  x_var = "educacion",
  covars = c("sexo", "wealth_3", "DEP"),
  ymax = 0.50,
  x_lab = "\nNivel de Educación"
)

# Merge plots
Fig_Edu_ABCD <- ggarrange(
  pA_edu, pB_edu, pC_edu, pD_edu, 
  ncol = 2, nrow = 2, common.legend = TRUE, legend = "bottom")

ggsave(
  here::here("outputs", "Figure_Education.png"),
  Fig_Edu_ABCD,
  width = 10.73,
  height = 10.23,
  dpi = 1200
)

#### Generación de Figuras para Riqueza ####
pA_w <- plot_margins_logbin(
  df = data,
  outcome = "anemia",
  letter = "A",
  x_var = "wealth_3",
  covars = c("sexo", "educacion", "DEP"),
  ymax = 0.75,
  x_lab = "\nÍndice de Riqueza"
)

pB_w <- plot_margins_logbin(
  df = data,
  outcome = "ferropenia",
  letter = "B",
  x_var = "wealth_3",
  covars = c("sexo", "educacion", "DEP"),
  ymax = 0.50,
  x_lab = "\nÍndice de Riqueza"
)

pC_w <- plot_margins_logbin(
  df = data,
  outcome = "anemia_no_ferropenica",
  letter = "C",
  x_var = "wealth_3",
  covars = c("sexo", "educacion", "DEP"),
  ymax = 0.50,
  x_lab = "\nÍndice de Riqueza"
)

pD_w <- plot_margins_logbin(
  df = data,
  outcome = "anemia_ferropenica",
  letter = "D",
  x_var = "wealth_3",
  covars = c("sexo", "educacion", "DEP"),
  ymax = 0.50,
  x_lab = "\nÍndice de Riqueza"
)

# Merge plots
Fig_Wealth_ABCD <- ggarrange(
  pA_w, pB_w, pC_w, pD_w, 
  ncol = 2, nrow = 2, common.legend = TRUE, legend = "bottom")

ggsave(
  here::here("outputs", "Figure_Wealth.png"),
  Fig_Wealth_ABCD,
  width = 10.73,
  height = 10.23,
  dpi = 1200
)
