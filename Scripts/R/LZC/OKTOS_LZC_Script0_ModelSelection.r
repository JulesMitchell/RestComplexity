## ----pkgload, include = FALSE-------------------------------------------------
library(ggplot2)
library(brms)
library(dplyr)
library(tidyverse) # needed for data manipulation
library(xlsx)

## ---- Set working directory and output file location as required -------------------------------------------------
setwd('')
outputFile <- "ModelOutputs.xlsx"

## ----Load Datasets, Clean and Set Factor Levels -------------------------------------------------
#1. Load data
data <- read.csv("analysis_master.csv")

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
prior = c(prior(normal(0.5, 0.05), class = 'Intercept', lb = 0), prior(normal(0, 0.1), class = 'b'), prior(student_t(3, 0.05, 0.05), class = 'sigma')) # Best Loo
prior_random = c(prior(normal(0.5, 0.05), class = 'Intercept', lb = 0), prior(normal(0, 0.1), class = 'b'), prior(student_t(3, 0.05, 0.05), class = 'sigma'), prior_(~cauchy(0, 0.01), class = ~sd)) # Best Loo

## ---- Specify model parameters (not required but advised)  -------------------------------------------------
iterations <- 2000      # No of iterations
warmup <- iterations / 2 # Warmup samples to be discarded
init <- 0
control <- list(
  adapt_engaged = TRUE,
  adapt_delta = 0.95, #increased from default of 0.8
  stepsize = 0.05, # 0.05 default
  max_treedepth = 15
)

## ---- Fit the Bayesian mixed-effects model -------------------------------------------------
# 1. Full Interaction Model (no random intercept)
model1 <- brm(LZC | resp_trunc(lb = 0) ~ 1 + Responder * Timepoint * Task, 
             data = data, 
             family = gaussian(),  # Specify the appropriate likelihood for your data
             chains = 4,  # Number of chains for MCMC sampling
             cores = 4,   # Number of CPU cores to use
             iter = iterations, # Number of MCMC iterations
             warmup = warmup,
             prior = prior,
             init = init,
             sample_prior = 'yes',
             save_pars = save_pars(all = TRUE),
             seed = 22,
             file = "LZC_model1.rda", # Uncomment to save model object
             control = control # adjust if divergence issues emerge
             )             

# 2. Full Interaction Model  (random intercept)
model2 <- brm(LZC | resp_trunc(lb = 0) ~ 1 + Responder * Timepoint * Task + (1| Subject), 
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
             seed = 22,
             file = "LZC_model2.rda", # Uncomment save model object
             control = control # adjust if divergence issues emerge
             )

# 3. Full Interaction Model  (random intercept + random timepoint slope by participant)
model3 <- brm(LZC | resp_trunc(lb = 0) ~ 1 + Responder * Timepoint * Task + (1 + Timepoint || Subject), 
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
             seed = 22,
             file = "LZC_model3.rda", # Uncomment save model object
             control = control # adjust if divergence issues emerge
             )

# 4. Full Interaction Model  (random intercept + random timepoint and Responder slope by participant)      
model4 <- brm(LZC | resp_trunc(lb = 0) ~ 1 + Responder * Timepoint * Task + (1 + Timepoint + Responder || Subject), 
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
             seed = 22,
             file = "LZC_model4.rda", # Uncomment save model object
             control = control # adjust if divergence issues emerge
             )

# 5. Full Interaction Model  (random intercept + random timepoint + responder + task slope by participant)
model5 <- brm(LZC | resp_trunc(lb = 0) ~ 1 + Responder * Timepoint * Task + (1 + Timepoint + Responder + Task|| Subject), 
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
             seed = 22,
             file = "LZC_model5.rda", # Uncomment save model object
             control = control # adjust if divergence issues emerge
             )

## ---- Add information criterion to model object  -------------------------------------------------
model1 <- add_criterion(model1, c("waic", "loo"))
model2 <- add_criterion(model2, c("waic", "loo"))
model3 <- add_criterion(model3, c("waic", "loo"))
model4 <- add_criterion(model4, c("waic", "loo"))
model5 <- add_criterion(model5, c("waic", "loo"))

## ---- Model  variance -------------------------------------------------
temp1 <- bayes_R2(model1) %>%as.data.frame()
temp2 <- bayes_R2(model2) %>%as.data.frame()
temp3 <- bayes_R2(model3) %>%as.data.frame()
temp4 <- bayes_R2(model4) %>%as.data.frame()
temp5 <- bayes_R2(model5) %>%as.data.frame() 

BayesR2 <- rbind(temp1, temp2, temp3, temp4, temp5)
BayesR2 <- round(BayesR2, 4)

# Modify row names to correspond to model formula
row.names(BayesR2) <- paste("Model", 1:nrow(BayesR2))
row.names(BayesR2)[row.names(BayesR2) == "Model 1"] <- model1$formula
row.names(BayesR2)[row.names(BayesR2) == "Model 2"] <- model2$formula
row.names(BayesR2)[row.names(BayesR2) == "Model 3"] <- model3$formula
row.names(BayesR2)[row.names(BayesR2) == "Model 4"] <- model4$formula
row.names(BayesR2)[row.names(BayesR2) == "Model 5"] <- model5$formula

## ---- Model comparison  -------------------------------------------------
loo <- loo(model1, model2, model3, model4, model5, compare = TRUE) # requires same numbers of observations
modelcomparison <- loo$diffs %>% as.data.frame()
row.names(modelcomparison)[row.names(modelcomparison) == "model1"] <- model1$formula
row.names(modelcomparison)[row.names(modelcomparison) == "model2"] <- model2$formula
row.names(modelcomparison)[row.names(modelcomparison) == "model3"] <- model3$formula
row.names(modelcomparison)[row.names(modelcomparison) == "model4"] <- model4$formula
row.names(modelcomparison)[row.names(modelcomparison) == "model5"] <- model5$formula

## ----- Data Export --------------
# Export tables
write.xlsx(BayesR2, outputFile, sheetName="LZC_ModelSelection_BayesR2",  append=TRUE)
write.xlsx(modelcomparison, outputFile, sheetName="LZC_ModelSelection",  append=TRUE)