#
#
# MA thesis R script
# 
# David Klug
#
#
#



# Packages ----------------------------------------------------------------
library(renv)
library(here)
library(rio)
library(MASS)
library(stargazer) # for R >= 4.2.2 run this bugfix https://gist.github.com/alexeyknorre/b0780836f4cec04d41a863a683f91b53
library(tidyverse)
library(AER)
library(readtext)
library(quanteda)
library(ggplot2)
library(stringr)
library(scales)
library(lemon)
library(ggpattern)
library(flextable)
library(ggcorrplot)
library(devtools)
#devtools::install_github("caijun/ggcorrplot2")
library(ggcorrplot2)
library(ggforce) #implicit dependency of ggcorrplot2
library(performance)
library(mice)
library(pscl)
library(poissonreg)
library(conflicted)
conflicted::conflict_prefer("filter", "dplyr")
conflicted::conflict_prefer("select", "dplyr")


# Setting up the environment and data import-------------------------------
# Clear the R environment
rm(list=ls())

# Set the working directory (should be redundant thanks to .Rproj)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Import data
base::load("regression_input_data.RData")



# Regression analysis ---------------------------------------------------

## Preparing the data ------------------------------------------------------

# a separate set of regression results was created based on data that excludes
# the somewhat extreme values of below -40
# d <- filter(d, HRP > -40)

# Formulas
fhd_max <- as.formula("hD ~ HRP + RL + ASSUR + HRR + lnA + ROA + mREG + sLAW")
fsd_max <- as.formula("sD ~ HRP + RL + ASSUR + HRR + lnA + ROA + mREG + sLAW")

# ####################### #
# !!! change input data !!!
# ####################### #
d <- d %>% filter(HRP > -40) # this excludes the somewhat extreme HRP values of below -40
m <- 30
d_imp <- mice::mice(data = d, m = m, maxit = 10, seed = 1, print = F)


## Soft disclosure models --------------------------------------------------

# Note: MICE imputation is only necessary for those models that contain report length (RL)

# create minimal sD model
model_sd_min <- glm(sD ~ HRP, data = d, family = "poisson")

# create sD model with firm-level control variables
model_sd_firmlvl <- glm(sD ~ HRP + lnA + ROA, data = d, family = "poisson")

# create sD model with country-level control variables
model_sd_cntrylvl <- glm(sD ~ HRP + mREG + sLAW, data = d, family = "poisson")

# create sD model with document-level control variables
models_sd_documentlvl <- lapply(1:m, function(i) {
  glm(sD ~ HRP + RL + ASSUR + HRR,
      data = mice::complete(data = d_imp, i), family = "poisson")
})

# create maximal sD model with imputed datasets
models_sd_max <- lapply(1:m, function(i) {
  glm(sD ~ HRP + RL + ASSUR + HRR + lnA + ROA + mREG + sLAW,
      data = mice::complete(data = d_imp, i), family = "poisson")
})

# create stepwise sD model with imputed datasets
stepwise_regression_sd <- function(data) {
  step(glm(sD ~ HRP, data = data, family = "poisson"),
       direction = "forward",
       scope = fsd_max) }
models_sd_stepwise <- lapply(1:m, function(i) {
  stepwise_regression_sd(mice::complete(data = d_imp, i))
})



## Hard disclosure models --------------------------------------------------

# create minimal hD model
model_hd_min <- glm.nb(hD ~ HRP, data = d)

# create hD model with firm-level control variables
model_hd_firmlvl <- glm.nb(hD ~ HRP + lnA + ROA, data = d)

# create hD model with country-level control variables
model_hd_cntrylvl <- glm.nb(hD ~ HRP + mREG + sLAW, data = d)

# create hD model with document-level control variables
models_hd_documentlvl <- lapply(1:m, function(i) {
  glm.nb(hD ~ HRP + RL + ASSUR + HRR,
         data = mice::complete(data = d_imp, i))
})

# create maximal hD model with imputed datasets
models_hd_max <- lapply(1:m, function(i) {
  glm.nb(hD ~ HRP + RL + ASSUR + HRR + lnA + ROA + mREG + sLAW,
         data = mice::complete(data = d_imp, i))
})

# create stepwise hD model with imputed datasets
stepwise_regression_hd <- function(data) {
  step(glm.nb(hD ~ HRP, data = data),
       direction = "forward",
       scope = fhd_max) }
models_hd_stepwise <- lapply(1:m, function(i) {
  stepwise_regression_hd(mice::complete(data = d_imp, i))
})


## Pooling the MICE regression results -------------------------------------

# extract MICE pooled regression results for soft disclosure models
models_sd_max_pooled <- mice::pool(models_sd_max)
models_sd_stepwise_pooled <- mice::pool(models_sd_stepwise)
models_sd_documentlvl_pooled <- mice::pool(models_sd_documentlvl)

# extract MICE pooled regression results for hard disclosure models
models_hd_max_pooled <- mice::pool(models_hd_max)
models_hd_stepwise_pooled <- mice::pool(models_hd_stepwise)
models_hd_documentlvl_pooled <- mice::pool(models_hd_documentlvl)


## Exporting regression model results -------------------------------------

# for R >= 4.2.2 run this bugfix https://gist.github.com/alexeyknorre/b0780836f4cec04d41a863a683f91b53

# create big regression table with soft and hard disclosure regression results
stargazer(
  # specify models (these mostly serve as placeholders because the coefficents etc. need to be extracted manually for pooled regression)
  model_sd_min, models_sd_documentlvl[[1]], model_sd_firmlvl, model_sd_cntrylvl, models_sd_max[[1]], models_sd_stepwise[[1]], 
  model_hd_min, models_hd_documentlvl[[1]], model_hd_firmlvl, model_hd_cntrylvl, models_hd_max[[1]], models_hd_stepwise[[1]],
  
  dep.var.caption = "Dependent variable: human rights disclosure score",
  dep.var.labels = c("soft disclosure", "hard disclosure"),
  title = "Poisson and negative binomial GLM regression about the relationship between human rights disclosure and performance",
  type = "html",
  out = "Outputs/reg_table_main_HRP-40.html",
  omit.stat = "theta",
  
  coef = list(coef(model_sd_min),
              summary(models_sd_documentlvl_pooled)$estimate,
              coef(model_sd_firmlvl),
              coef(model_sd_cntrylvl),
              summary(models_sd_max_pooled)$estimate,
              summary(models_sd_stepwise_pooled)$estimate,
              coef(model_hd_min),
              summary(models_hd_documentlvl_pooled)$estimate,
              coef(model_hd_firmlvl),
              coef(model_hd_cntrylvl),
              summary(models_hd_max_pooled)$estimate,
              summary(models_hd_stepwise_pooled)$estimate),
  
  se = list(summary(model_sd_min)$coefficients[, "Std. Error"],
            summary(models_sd_documentlvl_pooled)$std.error,
            summary(model_sd_firmlvl)$coefficients[, "Std. Error"],
            summary(model_sd_cntrylvl)$coefficients[, "Std. Error"],
            summary(models_sd_max_pooled)$std.error,
            summary(models_sd_stepwise_pooled)$std.error,
            summary(model_hd_min)$coefficients[, "Std. Error"],
            summary(models_hd_documentlvl_pooled)$std.error,
            summary(model_hd_firmlvl)$coefficients[, "Std. Error"],
            summary(model_hd_cntrylvl)$coefficients[, "Std. Error"],
            summary(models_hd_max_pooled)$std.error,
            summary(models_hd_stepwise_pooled)$std.error),
  
  t = list(coef(model_sd_min)/summary(model_sd_min)$coefficients[, "Std. Error"],
           summary(models_sd_documentlvl_pooled)$statistic,
           coef(model_sd_firmlvl)/summary(model_sd_firmlvl)$coefficients[, "Std. Error"],
           coef(model_sd_cntrylvl)/summary(model_sd_cntrylvl)$coefficients[, "Std. Error"],
           summary(models_sd_max_pooled)$statistic,
           summary(models_sd_stepwise_pooled)$statistic,
           coef(model_hd_min)/summary(model_hd_min)$coefficients[, "Std. Error"],
           summary(models_hd_documentlvl_pooled)$statistic,
           coef(model_hd_firmlvl)/summary(model_hd_firmlvl)$coefficients[, "Std. Error"],
           coef(model_hd_cntrylvl)/summary(model_hd_cntrylvl)$coefficients[, "Std. Error"],
           summary(models_hd_max_pooled)$statistic,
           summary(models_hd_stepwise_pooled)$statistic),
  
  p = list(coef(summary(model_sd_min))[,4],
           summary(models_sd_documentlvl_pooled)$p.value,
           coef(summary(model_sd_firmlvl))[,4],
           coef(summary(model_sd_cntrylvl))[,4],
           summary(models_sd_max_pooled)$p.value,
           summary(models_sd_stepwise_pooled)$p.value,
           coef(summary(model_hd_min))[,4],
           summary(models_hd_documentlvl_pooled)$p.value,
           coef(summary(model_hd_firmlvl))[,4],
           coef(summary(model_hd_cntrylvl))[,4],
           summary(models_hd_max_pooled)$p.value,
           summary(models_hd_stepwise_pooled)$p.value)
)



# Robustness tests --------------------------------------------------------


## Hurdle models for hard disclosure ---------------------------------------

# create minimal hD hurdle model
model_hurdle_min <- hurdle(hD ~ HRP, data = d, dist = "negbin", zero.dist = "negbin")
hurdletest(model_hurdle_min)

# create hD hurdle model with firm-level control variables
model_hurdle_firmlvl <- hurdle(hD ~ HRP + lnA + ROA, data = d, dist = "negbin", zero.dist = "negbin")

# create hD hurdle model with country-level control variables
model_hurdle_cntrylvl <- hurdle(hD ~ HRP + mREG + sLAW, data = d, dist = "negbin", zero.dist = "negbin")

# create hD hurdle model with document-level control variables
models_hurdle_documentlvl <- lapply(1:m, function(i) {
  hurdle(hD ~ HRP + RL + ASSUR + HRR,
         data = mice::complete(data = d_imp, i), dist = "negbin", zero.dist = "negbin")
})

# create maximal hD hurdle model with imputed datasets
models_hurdle_max <- lapply(1:m, function(i) {
  hurdle(hD ~ HRP + RL + ASSUR + HRR + lnA + ROA + mREG + sLAW,
         data = mice::complete(data = d_imp, i), dist = "negbin", zero.dist = "negbin")
})

# create stepwise hD hurdle model with imputed datasets
stepwise_regression_hurdle <- function(data) {
  step(hurdle(hD ~ HRP, data = data),
       direction = "forward",
       scope = fhd_max) }
models_hurdle_stepwise <- lapply(1:m, function(i) {
  stepwise_regression_hurdle(mice::complete(data = d_imp, i))
})


## Pooling the hurdle MICE models --------------------------------------------

# The approach for extracting hurdle model information is based on an SO post here:
# https://stackoverflow.com/questions/75717211/problem-with-pooling-estimates-from-multiple-imputed-datasets-using-mice-in-r-z

# Step 1
# Convert to MICE models to tidy format
models_hurdle_max_tidy <- mice::getfit(models_hurdle_max) %>%
  lapply(function(fit) poissonreg::tidy(fit, type="all")) %>% 
  bind_rows()
models_hurdle_stepwise_tidy <- mice::getfit(models_hurdle_stepwise) %>%
  lapply(function(fit) poissonreg::tidy(fit, type="all")) %>% 
  bind_rows()
models_hurdle_documentlvl_tidy <- mice::getfit(models_hurdle_documentlvl) %>%
  lapply(function(fit) poissonreg::tidy(fit, type="all")) %>% 
  bind_rows()

# Step 2
# Convenience wrapper function around pool.scalar.
# pool.scalar also returns a "qhat" and "u" which are vectors, 
# and we don't need them. Those vectors mess up the format of
# the summary that we want to compute later.
wrap.pool.scalar <- function(estimates, variances, n, k) {
  pool_res <- mice::pool.scalar(estimates, variances, n = n, k = k)
  return(as_tibble(list(
    qbar = pool_res$qbar, 
    ubar = pool_res$ubar, 
    b = pool_res$b, 
    t = pool_res$t, 
    df = pool_res$df, 
    r = pool_res$r, 
    fmi = pool_res$fmi)))
}

# For each (term,type) pair, compute pooled univariate estimates using 
# wrap.pool.scalar 
models_hurdle_max_tidy_pooled <- models_hurdle_max_tidy %>% 
  group_by(term, type) %>% 
  reframe(wrap.pool.scalar(estimate, std.error^2, n=60, k=1)) %>% 
  mutate(estimate = qbar)
models_hurdle_stepwise_tidy_pooled <- models_hurdle_stepwise_tidy %>% 
  group_by(term, type) %>% 
  reframe(wrap.pool.scalar(estimate, std.error^2, n=60, k=1)) %>% 
  mutate(estimate = qbar)
models_hurdle_documentlvl_tidy_pooled <- models_hurdle_documentlvl_tidy %>% 
  group_by(term, type) %>% 
  reframe(wrap.pool.scalar(estimate, std.error^2, n=60, k=1)) %>% 
  mutate(estimate = qbar)

# Step 3
# Copy the pooled estimate calculations from the the MICE package
# https://github.com/amices/mice/blob/master/R/mipo.R#L69-L71
models_hurdle_max_tidy_smry <- models_hurdle_max_tidy_pooled %>% mutate(
  std.error = sqrt(t), 
  statistic = estimate / std.error,
  p.value = 2 * (pt(abs(statistic), pmax(df, 0.001), lower.tail = FALSE))) %>% 
  dplyr::select(term, type, estimate, std.error, statistic, df, p.value)
models_hurdle_stepwise_tidy_smry <- models_hurdle_stepwise_tidy_pooled %>% mutate(
  std.error = sqrt(t), 
  statistic = estimate / std.error,
  p.value = 2 * (pt(abs(statistic), pmax(df, 0.001), lower.tail = FALSE))) %>% 
  dplyr::select(term, type, estimate, std.error, statistic, df, p.value)
models_hurdle_documentlvl_tidy_smry <- models_hurdle_documentlvl_tidy_pooled %>% mutate(
  std.error = sqrt(t), 
  statistic = estimate / std.error,
  p.value = 2 * (pt(abs(statistic), pmax(df, 0.001), lower.tail = FALSE))) %>% 
  dplyr::select(term, type, estimate, std.error, statistic, df, p.value)

# extract zero or count part of the tidy hurdle models
models_hurdle_max_tidy_smry_zero <- models_hurdle_max_tidy_smry[models_hurdle_max_tidy_smry$type=="zero",]
models_hurdle_max_tidy_smry_count <- models_hurdle_max_tidy_smry[models_hurdle_max_tidy_smry$type=="count",]
models_hurdle_stepwise_tidy_smry_zero <- models_hurdle_stepwise_tidy_smry[models_hurdle_stepwise_tidy_smry$type=="zero",]
models_hurdle_stepwise_tidy_smry_count <- models_hurdle_stepwise_tidy_smry[models_hurdle_stepwise_tidy_smry$type=="count",]
models_hurdle_documentlvl_tidy_smry_zero <- models_hurdle_documentlvl_tidy_smry[models_hurdle_documentlvl_tidy_smry$type=="zero",]
models_hurdle_documentlvl_tidy_smry_count <- models_hurdle_documentlvl_tidy_smry[models_hurdle_documentlvl_tidy_smry$type=="count",]

# also tidy up non MICE models for consistency
model_hurdle_min_zero <- poissonreg::tidy(model_hurdle_min, type="all") %>% filter(type=="zero")
model_hurdle_min_count <- poissonreg::tidy(model_hurdle_min, type="all") %>% filter(type=="count")
model_hurdle_firmlvl_zero <- poissonreg::tidy(model_hurdle_firmlvl, type="all") %>% filter(type=="zero")
model_hurdle_firmlvl_count <- poissonreg::tidy(model_hurdle_firmlvl, type="all") %>% filter(type=="count")
model_hurdle_cntrylvl_zero <- poissonreg::tidy(model_hurdle_cntrylvl, type="all") %>% filter(type=="zero")
model_hurdle_cntrylvl_count <- poissonreg::tidy(model_hurdle_cntrylvl, type="all") %>% filter(type=="count")



## Exporting hurdle model results ------------------------------------------

# for R >= 4.2.2 run this bugfix https://gist.github.com/alexeyknorre/b0780836f4cec04d41a863a683f91b53

stargazer(
  # specify models (these mostly serve as placeholders because the coefficents etc. need to be extracted manually for pooled regression)
  model_hurdle_min, models_hurdle_documentlvl[[1]], model_hurdle_firmlvl,
  model_hurdle_cntrylvl, models_hurdle_max[[1]], models_hurdle_stepwise[[1]], 
  model_hurdle_min, models_hurdle_documentlvl[[1]], model_hurdle_firmlvl,
  model_hurdle_cntrylvl, models_hurdle_max[[1]], models_hurdle_stepwise[[1]],
  
  dep.var.caption = "Dependent variable: hard human rights disclosure score",
  dep.var.labels = "",
  title = "Neg. binomial hurdle regression that models the relationship between human rights disclosure and performance<br>1-6 are zero models, 7-12 are count models",
  type = "html",
  out = "Outputs/reg_table_hurdle_HRP-40.html",
  omit.stat = "theta",
  
  coef = list(model_hurdle_min_zero$estimate,
              models_hurdle_documentlvl_tidy_smry_zero$estimate,
              model_hurdle_firmlvl_zero$estimate,
              model_hurdle_cntrylvl_zero$estimate,
              models_hurdle_max_tidy_smry_zero$estimate,
              models_hurdle_stepwise_tidy_smry_zero$estimate,
              model_hurdle_min_count$estimate,
              models_hurdle_documentlvl_tidy_smry_count$estimate,
              model_hurdle_firmlvl_count$estimate,
              model_hurdle_cntrylvl_count$estimate,
              models_hurdle_max_tidy_smry_count$estimate,
              models_hurdle_stepwise_tidy_smry_count$estimate),
  
  se = list(model_hurdle_min_zero$std.error,
            models_hurdle_documentlvl_tidy_smry_zero$std.error,
            model_hurdle_firmlvl_zero$std.error,
            model_hurdle_cntrylvl_zero$std.error,
            models_hurdle_max_tidy_smry_zero$std.error,
            models_hurdle_stepwise_tidy_smry_zero$std.error,
            model_hurdle_min_count$std.error,
            models_hurdle_documentlvl_tidy_smry_count$std.error,
            model_hurdle_firmlvl_count$std.error,
            model_hurdle_cntrylvl_count$std.error,
            models_hurdle_max_tidy_smry_count$std.error,
            models_hurdle_stepwise_tidy_smry_count$std.error),
  
  t = list(model_hurdle_min_zero$statistic,
           models_hurdle_documentlvl_tidy_smry_zero$statistic,
           model_hurdle_firmlvl_zero$statistic,
           model_hurdle_cntrylvl_zero$statistic,
           models_hurdle_max_tidy_smry_zero$statistic,
           models_hurdle_stepwise_tidy_smry_zero$statistic,
           model_hurdle_min_count$statistic,
           models_hurdle_documentlvl_tidy_smry_count$statistic,
           model_hurdle_firmlvl_count$statistic,
           model_hurdle_cntrylvl_count$statistic,
           models_hurdle_max_tidy_smry_count$statistic,
           models_hurdle_stepwise_tidy_smry_count$statistic),
  
  p = list(model_hurdle_min_zero$p.value,
           models_hurdle_documentlvl_tidy_smry_zero$p.value,
           model_hurdle_firmlvl_zero$p.value,
           model_hurdle_cntrylvl_zero$p.value,
           models_hurdle_max_tidy_smry_zero$p.value,
           models_hurdle_stepwise_tidy_smry_zero$p.value,
           model_hurdle_min_count$p.value,
           models_hurdle_documentlvl_tidy_smry_count$p.value,
           model_hurdle_firmlvl_count$p.value,
           model_hurdle_cntrylvl_count$p.value,
           models_hurdle_max_tidy_smry_count$p.value,
           models_hurdle_stepwise_tidy_smry_count$p.value)
)
