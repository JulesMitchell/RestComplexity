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
prior_random = c(prior(normal(0.5, 0.05), class = 'Intercept', lb = 0), prior(normal(0, 0.1), class = 'b'), prior(student_t(3, 0.05, 0.05), class = 'sigma'), prior_(~cauchy(0, 0.01), class = ~sd)) 

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
# Prior Only Model
finalModel_prior <- brm(LZC | resp_trunc(lb = 0) ~ 1 + Responder * Timepoint * Task + (1 + Timepoint + Responder + Task || Subject), 
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
             file = "LZC_finalModel_prior.rda", # Uncomment save model object
             control = control # adjust if divergence issues emerge
             )

finalModel <- brm(LZC | resp_trunc(lb = 0) ~ 1 + Responder * Timepoint * Task + (1 + Timepoint + Responder + Task|| Subject), 
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
             file = "LZC_finalModel.rda", # Uncomment save model object
             control = control # adjust if divergence issues emerge
             )

## ---- Add information criterion to model object  -------------------------------------------------
finalModel <- add_criterion(finalModel, c("waic", "loo"))

## ---- Model  variance -------------------------------------------------
bayes_R2(finalModel)
temp1 <- bayes_R2(finalModel) %>%as.data.frame()

loo <- finalModel$criteria$loo$estimates  %>% as.data.frame()
row.names(loo)[row.names(loo) == "model1"] <- finalModel$formula

## ----- Data Export --------------
# Export tables
write.xlsx(temp1, outputFile, sheetName="LZC_finalModel_BayesR2",  append=TRUE)
write.xlsx(loo, outputFile, sheetName="LZC_finalModel_loo",  append=TRUE)