## ----pkgload, include = FALSE-------------------------------------------------
library(ggplot2)
library(brms)
library(dplyr)
library(tidyverse) # needed for data manipulation
library(xlsx)

## ---- Set working directory as required -------------------------------------------------
setwd('')

## ----Load Datasets, Clean and Set Factor Levels -------------------------------------------------
#1. Load data
data <- read.csv("LZC_Chan_Master.csv")
outputFile <- "ModelOutputs.xlsx"

#2. Remove drop-outs
data <- data %>% 
  filter(!BSS == 999)

#3. Specify variables as factors and code levels
data$Timepoint <- factor(data$Timepoint, levels = c("ses-01", "ses-02", "ses-03"), labels = c("BAS", "POST", "FUP"))
data$Task <- factor(data$Task, labels = c("EC", "EO"))
data$Responder  <- factor(data$Responder, levels = c(0, 1), labels = c("NR", "RESP"))

#4. Check variable types to confirm factor coding
str(data)

### ANALYSIS SECTION ###
## ---- Set priors (add correct name to brm model)  -------------------------------------------------
prior_random <- c(set_prior("normal(0.5, 0.05)", class = "Intercept", resp = c('Fp1', 'AF3','FC1', 'F3')),
          set_prior("normal(0, 0.1)", class = "b", resp = c('Fp1', 'AF3','FC1', 'F3')),
          set_prior("student_t(3, 0, 0.05)", class = "sigma", resp = c('Fp1', 'AF3','FC1', 'F3')),
          set_prior("cauchy(0, 0.01)", class = "sd", resp = c('Fp1', 'AF3','FC1', 'F3'))
)

## ---- Specify model parameters (not required but advised)  -------------------------------------------------
iterations <- 15000      # No of iterations
warmup <- iterations / 2 # Warmup samples to be discarded
init <- 0
control <- list(
  adapt_engaged = TRUE,
  adapt_delta = 0.95, #increased from default of 0.8
  stepsize = 0.05, # 0.05 default
  max_treedepth = 15
)

## ---- Fit the Bayesian mixed-effects model -------------------------------------------------
# Specify multivariate syntax
MV <- 
  bf(mvbind(Fp1, AF3, FC1, F3) | resp_trunc(lb = 0) ~ 1 + Responder * Timepoint * Task + (1 + Responder + Timepoint + Task |p| Subject)) +
  set_rescor(FALSE)

# 2.  Full Interaction Model  (random intercept)
fullModel_prior <- brm(MV, 
             data = data, 
             family = gaussian(),  # Specify the appropriate likelihood for your data
             chains = 4,  # Number of chains for MCMC sampling
             cores = 4,   # Number of CPU cores to use
             iter = iterations,  # Number of MCMC iterations
             warmup = warmup,
             prior = prior_random,
             init = init,
             sample_prior = 'only',
             save_pars = save_pars(all = TRUE),
             file = "LZCchan_fullModel_prior.rda", # include to save model object
             control = control # adjust if divergence issues emerge
             )

fullModel <- brm(MV, 
             data = data, 
             family = gaussian(),  # Specify the appropriate likelihood for your data
             chains = 4,  # Number of chains for MCMC sampling
             cores = 4,   # Number of CPU cores to use
             iter = iterations,  # Number of MCMC iterations
             warmup = warmup,
             prior = prior_random,
             init = init,
             sample_prior = 'yes',
             save_pars = save_pars(all = TRUE),
             file = "LZCchan_fullModel.rda", # include to save model object
             control = control # adjust if divergence issues emerge
             )

## ---- Add information criterion to model object  -------------------------------------------------
fullModel <- add_criterion(fullModel, c("waic", "loo"))

## ---- Assess fit (observed vs. predicted) -------------------------------------------------
temp1 <- bayes_R2(fullModel) %>%as.data.frame()
BayesR2 <- round(temp1, 4)

row.names(BayesR2) <- paste("Model", 1:nrow(BayesR2))
# row.names(BayesR2)[row.names(BayesR2) == "Model 1"] <- fullModel$formula
loo <- fullModel$criteria$loo$estimates  %>% as.data.frame()
row.names(loo)[row.names(loo) == "model1"] <- fullModel$formula

## ----- Data Export --------------
# Export tables
write.xlsx(BayesR2, outputFile, sheetName="LZCChan_BayesR2",  append=TRUE)
write.xlsx(loo, outputFile, sheetName="LZCChan_loo",  append=TRUE)