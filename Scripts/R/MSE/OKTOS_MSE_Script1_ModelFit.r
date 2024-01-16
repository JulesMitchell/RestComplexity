### MODEL SPECIFICATION AND FITTING###
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
## ---- Set priors ------------------------------------------------               
prior_random <- c(set_prior("normal(1.2, 0.2)", class = "Intercept", resp = c('Scale1', 'Scale2','Scale3', 'Scale4','Scale5', 'Scale6','Scale7', 'Scale8', 'Scale9', 'Scale10')),
          set_prior("normal(0, 0.1)", class = "b", resp = c('Scale1', 'Scale2','Scale3', 'Scale4','Scale5', 'Scale6','Scale7', 'Scale8', 'Scale9', 'Scale10')),
          set_prior("student_t(3, 0.05, 0.05)", class = "sigma", resp = c('Scale1', 'Scale2','Scale3', 'Scale4','Scale5', 'Scale6','Scale7', 'Scale8', 'Scale9', 'Scale10')),
          set_prior("cauchy(0, 0.01)", class = "sd", resp = c('Scale1', 'Scale2','Scale3', 'Scale4','Scale5', 'Scale6','Scale7', 'Scale8', 'Scale9', 'Scale10'))
)
## ---- Specify model parameters (not required but advised)  -------------------------------------------------
iterations <- 15000      # No of iterations
warmup <- iterations / 2 # Warmup samples to be discarded
init <- 0
control <- list(
  adapt_engaged = TRUE,
  adapt_delta = 0.95, #increased from default of 0.8
  stepsize = 0.05, # 0.05 default
  max_treedepth = 15)

# Specify multivariate syntax
MV <- bf(mvbind(Scale_1, Scale_2, Scale_3, Scale_4, Scale_5, Scale_6, Scale_7, Scale_8, Scale_9, Scale_10) | resp_trunc(lb = 0) ~ 1 + Responder * Timepoint * Task + (1 + Responder + Timepoint + Task |p| Subject)) +
  set_rescor(FALSE)

## ---- Fit the Bayesian mixed-effects model -------------------------------------------------
# 1. Prior Only Model
finalModel_prior <- brm(MV, 
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
             seed = 22,
             file = "MSE_MVfinalModel_prior", # include to save model object
             control = control # adjust if divergence issues emerge
             )

finalModel <- brm(MV, 
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
             file = "MSE_MVfinalModel", # include to save model object
             control = control # adjust if divergence issues emerge
             )

## ---- Add information criterion to model object  -------------------------------------------------
finalModel <- add_criterion(finalModel, c("waic", "loo"))

## ---- Model Variance -------------------------------------------------
temp1 <- bayes_R2(finalModel) %>%as.data.frame()

loo <- finalModel$criteria$loo$estimates  %>% as.data.frame()
row.names(loo)[row.names(loo) == "model1"] <- finalModel$formula

## ----- Data Export --------------
# Export tables
write.xlsx(temp1, outputFile, sheetName="MSE_MVfinalModel_BayesR2",  append=TRUE)
write.xlsx(loo, outputFile, sheetName="MSE_MVfinalModel_loo",  append=TRUE)