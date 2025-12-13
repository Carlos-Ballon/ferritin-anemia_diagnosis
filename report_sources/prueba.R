library(rio)
library(tidyverse)
library(here)
library(janitor)
library(ggeffects)
library(broom)
library(rms)
library(mgcv)

df <- import(here::here("Base de Curvas.dta"))

df_tidy <- data %>% 
  mutate(
    anemia = if_else(P037 >= 10.5, 0L, 1L),
    ferropenia = if_else(ferr_adj >= 12, 0L, 1L)
  )

### Ferritina
# Modelo naive para forma de relacion
mod <- glm(anemia ~ ferr_adj, 
           data = df_tidy, 
           family = binomial(link = "logit"))
summary(mod)

fited_lin <- predict_response(mod, terms = "ferr_adj")
plot(fited_lin)

# Modelo naive para forma de relacio
mod <- glm(anemia ~ rcs(ferr_adj, 3), 
           data = df_tidy, 
           family = binomial(link = "logit"))
summary(mod)

fited_sp <- predict_response(mod, terms = "ferr_adj")
plot(fited_sp)

# Modelo gam para forma de relacion
mod <- gam(anemia ~ s(ferr_adj, k = 4, bs = "cr"), 
           data = df_tidy, 
           family = binomial(link = "logit"), 
           method = "REML", 
           select = TRUE)
summary(mod)

fited_sp <- predict_response(mod, terms = "ferr_adj")
plot(fited_sp)


### Transportador de Ferritina
# Modelo naive para forma de relacion
mod <- glm(anemia ~ P032, 
           data = df_tidy, 
           family = binomial(link = "logit"))
summary(mod)

fited_lin <- predict_response(mod, terms = "P032")
plot(fited_lin)

# Modelo naive para forma de relacio
mod <- glm(anemia ~ rcs(P032, 3), 
           data = df_tidy, 
           family = binomial(link = "logit"))
summary(mod)

fited_sp <- predict_response(mod, terms = "P032")
plot(fited_sp)

# Modelo gam para forma de relacion
mod <- gam(anemia ~ s(P032, k = 4, bs = "cr"), 
           data = df_tidy, 
           family = binomial(link = "logit"), 
           method = "REML", 
           select = TRUE)
summary(mod)

fited_sp <- predict_response(mod, terms = "P032")
plot(fited_sp)

### Transportador de Ferritina
# Modelo naive para forma de relacion
mod <- glm(anemia ~ P017, 
           data = df_tidy, 
           family = binomial(link = "logit"))
summary(mod)

fited_lin <- predict_response(mod, terms = "P017")
plot(fited_lin)

# Modelo naive para forma de relacio
mod <- glm(anemia ~ rcs(P017, 3), 
           data = df_tidy, 
           family = binomial(link = "logit"))
summary(mod)

fited_sp <- predict_response(mod, terms = "P017")
plot(fited_sp)

# Modelo gam para forma de relacion
mod <- gam(anemia ~ s(P017, k = 4, bs = "cr"), 
           data = df_tidy, 
           family = binomial(link = "logit"), 
           method = "REML", 
           select = TRUE)
summary(mod)

fited_sp <- predict_response(mod, terms = "P017")
plot(fited_sp)




### Transportador de Ferritina ajustada por transportador de ferritina
# Modelo naive para forma de relacion
mod <- glm(anemia ~ ferr_adj + P017, 
           data = df_tidy, 
           family = binomial(link = "logit"))
summary(mod)

fited_lin <- predict_response(mod, terms = "ferr_adj")
plot(fited_lin)

# Modelo naive para forma de relacio
mod <- glm(anemia ~ rcs(ferr_adj, 3) + rcs(P017, 3), 
           data = df_tidy, 
           family = binomial(link = "logit"))
summary(mod)

fited_sp <- predict_response(mod, terms = "ferr_adj")
plot(fited_sp)

# Modelo gam para forma de relacion
mod <- gam(anemia ~ s(ferr_adj, k = 4, bs = "cr") + 
             s(P017, k = 4, bs = "cr"), 
           data = df_tidy, 
           family = binomial(link = "logit"), 
           method = "REML", 
           select = TRUE)
summary(mod)

fited_sp <- predict_response(mod, terms = "ferr_adj")
plot(fited_sp)







### Transportador d
# 
mod <- glm(ferropenia ~ P037,
           family = binomial(link = "logit"),
           data = df_tidy)
summary(mod)

fited_lin <- predict_response(mod, terms = "P037")
plot(fited_lin)

# Modelo naive para forma de relacio
mod <- glm(ferropenia ~ rcs(P037, 3), 
           family = binomial(link = "logit"),
           data = df_tidy)
summary(mod)

fited_sp <- predict_response(mod, terms = "P037")
plot(fited_sp)

# Modelo gam para forma de relacion
mod <- gam(anemia ~ s(ferr_adj, k = 4, bs = "cr") + 
             s(P017, k = 4, bs = "cr"), 
           data = df_tidy, 
           family = binomial(link = "logit"), 
           method = "REML", 
           select = TRUE)
summary(mod)

fited_sp <- predict_response(mod, terms = "ferr_adj")
plot(fited_sp)


# Check for multicollinearity
performance::check_collinearity(mod_anemia, ci = NULL)
