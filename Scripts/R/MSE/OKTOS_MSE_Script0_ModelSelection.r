### MODEL SPECIFICATION AND VALIDATION ###
## ----pkgload, include = FALSE-------------------------------------------------
library(ggplot2)
library(brms)
library(dplyr)
library(tidyverse) # needed for data manipulation
library(xlsx)

## ---- Set working directory as required -------------------------------------------------
setwd('')
outputFile <- "ModelOutputs.xlsx"

## ----Load Datasets, Clean and Set Factor Levels -------------------------------------------------
#1. Load data
data <- read.csv("analysis_master.csv")

# Select participant IDs to drop
drop <- c('sub-04', 'sub-05','sub-09', 'sub-13','sub-17', 'sub-32','sub-36')
data <- data %>% 
  filter(!Subject %in% drop)

#2. Specify variables as factors and code levels
data$Timepoint <- factor(data$Timepoint, levels = c("ses-01", "ses-02", "ses-03"), labels = c("BAS", "POST", "FUP"))
data$Task <- factor(data$Task, labels = c("EC", "EO"))
data$Responder  <- factor(data$Responder, levels = c(0, 1), labels = c("NR", "RESP"))

#3. Rename MSE scale columns using index
for (i in 1:10){
  # colnames(data)[i] <- "new_col2"
  colnames(data)[which(names(data) == paste("MSE..scale.", as.character(i), ".", sep = ''))] <- paste("Scale_", as.character(i), sep = '')
}

#4. Check variable types to confirm factor coding
str(data)

### ANALYSIS SECTION ###
## ---- Set priors -------------------------------------------------
# Current best prior
# Tentatively Correct
prior <- c(set_prior("normal(1.2, 0.2)", class = "Intercept", resp = c('Scale1', 'Scale2','Scale3', 'Scale4','Scale5', 'Scale6','Scale7', 'Scale8', 'Scale9', 'Scale10')),
          set_prior("normal(0, 0.1)", class = "b", resp = c('Scale1', 'Scale2','Scale3', 'Scale4','Scale5', 'Scale6','Scale7', 'Scale8', 'Scale9', 'Scale10')),
          set_prior("student_t(3, 0.05, 0.05)", class = "sigma", resp = c('Scale1', 'Scale2','Scale3', 'Scale4','Scale5', 'Scale6','Scale7', 'Scale8', 'Scale9', 'Scale10'))
)

prior_random <- c(set_prior("normal(1.2, 0.2)", class = "Intercept", resp = c('Scale1', 'Scale2','Scale3', 'Scale4','Scale5', 'Scale6','Scale7', 'Scale8', 'Scale9', 'Scale10')),
          set_prior("normal(0, 0.1)", class = "b", resp = c('Scale1', 'Scale2','Scale3', 'Scale4','Scale5', 'Scale6','Scale7', 'Scale8', 'Scale9', 'Scale10')),
          set_prior("student_t(3, 0.05, 0.05)", class = "sigma", resp = c('Scale1', 'Scale2','Scale3', 'Scale4','Scale5', 'Scale6','Scale7', 'Scale8', 'Scale9', 'Scale10')),
          set_prior("cauchy(0, 0.01)", class = "sd", resp = c('Scale1', 'Scale2','Scale3', 'Scale4','Scale5', 'Scale6','Scale7', 'Scale8', 'Scale9', 'Scale10'))
)

## ---- Specify model parameters (iterations set low for model validation only)  -------------------------------------------------
iterations <- 2000      # No of iterations
warmup <- iterations / 2 # Warmup samples to be discarded
init <- 0
control <- list(
  adapt_engaged = TRUE,
  adapt_delta = 0.95, #increased from default of 0.8
  stepsize = 0.05, # 0.05 default
  max_treedepth = 15)

# Specify multivariate syntax
MV1 <- bf(mvbind(Scale_1, Scale_2, Scale_3, Scale_4, Scale_5, Scale_6, Scale_7, Scale_8, Scale_9, Scale_10) | resp_trunc(lb = 0) ~ 1 + Responder * Timepoint * Task ) +
  set_rescor(FALSE)

MV2 <- bf(mvbind(Scale_1, Scale_2, Scale_3, Scale_4, Scale_5, Scale_6, Scale_7, Scale_8, Scale_9, Scale_10) | resp_trunc(lb = 0) ~ 1 + Responder * Timepoint * Task + (1 |p| Subject)) +
  set_rescor(FALSE)

MV3 <- bf(mvbind(Scale_1, Scale_2, Scale_3, Scale_4, Scale_5, Scale_6, Scale_7, Scale_8, Scale_9, Scale_10) | resp_trunc(lb = 0) ~ 1 + Responder * Timepoint * Task + (1 + Responder |p| Subject)) +
  set_rescor(FALSE)

MV4 <- bf(mvbind(Scale_1, Scale_2, Scale_3, Scale_4, Scale_5, Scale_6, Scale_7, Scale_8, Scale_9, Scale_10) | resp_trunc(lb = 0) ~ 1 + Responder * Timepoint * Task + (1 + Responder + Timepoint |p| Subject)) +
  set_rescor(FALSE)

MV5 <- bf(mvbind(Scale_1, Scale_2, Scale_3, Scale_4, Scale_5, Scale_6, Scale_7, Scale_8, Scale_9, Scale_10) | resp_trunc(lb = 0) ~ 1 + Responder * Timepoint * Task + (1 + Responder + Timepoint + Task |p| Subject)) +
  set_rescor(FALSE)

## ---- Fit the Bayesian mixed-effects model -------------------------------------------------
# 1. Full interaction model (no random intercept)
model1 <- brm(MV1, # Done
             data = data, 
             family = gaussian(),  # Specify the appropriate likelihood for your data
             chains = 2,  # Number of chains for MCMC sampling
             cores = 4,   # Number of CPU cores to use
             iter = iterations, # Number of MCMC iterations
             warmup = warmup,
             prior = prior,
             init = init,
             sample_prior = 'yes',
             save_pars = save_pars(all = TRUE),
             seed = 22,
             file = "MSE_model1_MV.rda", # include to save model object
             control = control # adjust if divergence issues emerge
             )             

# 2. Full interaction model (random intercept for subject)
model2 <- brm(MV2, 
             data = data, 
             family = gaussian(),  # Specify the appropriate likelihood for your data
             chains = 2,  # Number of chains for MCMC sampling
             cores = 4,   # Number of CPU cores to use
             iter = iterations,  # Number of MCMC iterations
             warmup = warmup,
             prior = prior_random,
             init = init,
             sample_prior = 'yes',
             save_pars = save_pars(all = TRUE),
             seed = 22,
             file = "MSE_model2_MV.rda", # include to save model object
             control = control # adjust if divergence issues emerge
             )

# 3. Full interaction model (random intercept for scale)
model3 <- brm(MV3, 
             data = data, 
             family = gaussian(),  # Specify the appropriate likelihood for your data
             chains = 2,  # Number of chains for MCMC sampling
             cores = 4,   # Number of CPU cores to use
             iter = iterations,  # Number of MCMC iterations
             warmup = warmup,
             prior = prior_random,
             init = init,
             sample_prior = 'yes',
             save_pars = save_pars(all = TRUE),
             seed = 22,
             file = "MSE_model3_MV.rda", # include to save model object
             control = control # adjust if divergence issues emerge
             )

# 4. Full interaction model (random intercept for subject and scale)
model4 <- brm(MV4, # Done
             data = data, 
             family = gaussian(),  # Specify the appropriate likelihood for your data
             chains = 2,  # Number of chains for MCMC sampling
             cores = 4,   # Number of CPU cores to use
             iter = iterations,  # Number of MCMC iterations
             warmup = warmup,
             prior = prior_random,
             init = init,
             sample_prior = 'yes',
             save_pars = save_pars(all = TRUE),
             seed = 22,
             file = "MSE_model4_MV.rda", # include to save model object
             control = control # adjust if divergence issues emerge
             )

# 5. Full interaction model (random intercept for subject and scale, and random slopes for subject)
model5 <- brm(MV5, # Done
             data = data, 
             family = gaussian(),  # Specify the appropriate likelihood for your data
             chains = 2,  # Number of chains for MCMC sampling
             cores = 4,   # Number of CPU cores to use
             iter = iterations,  # Number of MCMC iterations
             warmup = warmup,
             prior = prior_random,
             init = init,
             sample_prior = 'yes',
             save_pars = save_pars(all = TRUE),
             seed = 22,
             file = "MSE_model5_MV.rda", # include to save model object
             control = control # adjust if divergence issues emerge
             )

## ---- Add information criterion to model object  -------------------------------------------------
model1 <- add_criterion(model1, c("waic", "loo"))
model2 <- add_criterion(model2, c("waic", "loo"))
model3 <- add_criterion(model3, c("waic", "loo"))
model4 <- add_criterion(model4, c("waic", "loo"))
model5 <- add_criterion(model5, c("waic", "loo"))

## ---- Assess fit (observed vs. predicted) -------------------------------------------------
temp1 <- bayes_R2(model1) %>%as.data.frame()
temp2 <- bayes_R2(model2) %>%as.data.frame()
temp3 <- bayes_R2(model3) %>%as.data.frame()
temp4 <- bayes_R2(model4) %>%as.data.frame()
temp5 <- bayes_R2(model5) %>%as.data.frame()

BayesR2 <- rbind(temp1, temp2, temp3, temp4, temp5)
BayesR2 <- round(BayesR2, 4)

row.names(BayesR2) <- paste("Model", 1:nrow(BayesR2))

## ---- Model comparison  -------------------------------------------------
loo <- loo(model1, model2, model3, model4, model5, compare = TRUE) # requires same numbers of observations
modelcomparison <- loo$diffs %>% as.data.frame()

## ----- Data Export --------------
# Export tables
write.xlsx(BayesR2, outputFile, sheetName="MSE_MVModelSelection_BayesR2",  append=TRUE)
write.xlsx(modelcomparison, outputFile, sheetName="MSE_MVModelSelection",  append=TRUE)