## Analysis Overview ------------------------
# Post-Hoc exploratory evaluation of time of EEG recording to assess potential response status bias.

## ----pkgload, include = FALSE-------------------------------------------------
library(ggplot2)
library(brms)
library(dplyr)
library(tidyverse) # needed for data manipulation
library(emmeans)
library(xlsx)
library(bayestestR)
library(knitr)
library(kableExtra)
library(tidybayes)

## ---- Set working directory as required -------------------------------------------------
setwd('')
outputFile <- "ModelOutputs.xlsx"

## ----Load Datasets, Clean and Set Factor Levels -------------------------------------------------
#1. Load data
data <- read.csv("analysis_master.csv")

#2. Remove drop-outs
drop <- c('sub-04', 'sub-05','sub-09', 'sub-13','sub-17', 'sub-32','sub-36')
data <- data %>% 
  filter(!Subject %in% drop, Timepoint == 'ses-01')

#3. Specify variables as factors and code levels
# LZC
data$Timepoint <- factor(data$Timepoint, levels = c("ses-01"), labels = c("BAS"))
data$Task <- factor(data$Task, labels = c("EC", "EO"))
data$Responder  <- factor(data$Responder, levels = c(0, 1), labels = c("NR", "RESP"))
data$is_before_12 <- factor (data$is_before_12, levels = c(1,2), labels = c("Pre12", "Post12"))

#4. Rename MSE scale columns using index
for (i in 1:10){
  colnames(data)[which(names(data) == paste("MSE..scale.", as.character(i), ".", sep = ''))] <- paste("Scale_", as.character(i), sep = '')
}

#5. Check variable types to confirm factor coding
str(data)

### ANALYSIS SECTION ###
## ---- Set priors (add correct name to brm model)  -------------------------------------------------
prior_LZC = c(prior(normal(0.5, 0.1), class = 'Intercept', lb = 0), prior(normal(0, 0.1), class = 'b'), prior(student_t(3, 0.05, 0.05), class = 'sigma')) 
prior_MSE <- c(set_prior("normal(1.2, 0.2)", class = "Intercept", resp = c('Scale1', 'Scale2','Scale3', 'Scale4','Scale5', 'Scale6','Scale7', 'Scale8', 'Scale9', 'Scale10')),
          set_prior("normal(0, 0.1)", class = "b", resp = c('Scale1', 'Scale2','Scale3', 'Scale4','Scale5', 'Scale6','Scale7', 'Scale8', 'Scale9', 'Scale10')),
          set_prior("student_t(3, 0.05, 0.05)", class = "sigma", resp = c('Scale1', 'Scale2','Scale3', 'Scale4','Scale5', 'Scale6','Scale7', 'Scale8', 'Scale9', 'Scale10'))
)

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
timeofday_LZC <- brm(LZC | resp_trunc(lb = 0) ~ 1 + Responder * Task * is_before_12, 
             data = data, 
             family = gaussian(),  # Specify the appropriate likelihood for your data
             chains = 2,  # Number of chains for MCMC sampling
             cores = 4,   # Number of CPU cores to use
             iter = iterations,  # Number of MCMC iterations
             warmup = warmup,
             prior = prior_LZC,
             init = init,
             sample_prior = 'yes',
             save_pars = save_pars(all = TRUE),
             file = "LZC_TimeofDay.rda", # Uncomment save model object
             control = control # adjust if divergence issues emerge
             )

MV1 <- bf(mvbind(Scale_1, Scale_2, Scale_3, Scale_4, Scale_5, Scale_6, Scale_7, Scale_8, Scale_9, Scale_10) | resp_trunc(lb = 0) ~ 1 + Responder  * Task * is_before_12 ) +
  set_rescor(FALSE)

timeofday_MSE <- brm(MV1, 
             data = data, 
             family = gaussian(),  # Specify the appropriate likelihood for your data
             chains = 2,  # Number of chains for MCMC sampling
             cores = 4,   # Number of CPU cores to use
             iter = iterations,  # Number of MCMC iterations
             warmup = warmup,
             prior = prior_MSE,
             init = init,
             sample_prior = 'yes',
             save_pars = save_pars(all = TRUE),
             file = "MSE_TimeofDay.rda", # Uncomment save model object
             control = control # adjust if divergence issues emerge
             )

## ----- Summary of effect estimates --------------
#LZC
LZCeffects <- describe_posterior(timeofday_LZC, test=c('pd', 'p_map')) %>%
  	as.data.frame() %>%
      mutate(across(is.numeric, round, digits=4))

#MSE
MSEeffects <- describe_posterior(timeofday_MSE, test=c('pd', 'p_map')) %>%
  	as.data.frame() %>%
      mutate(across(is.numeric, round, digits=4))

## Frequestist tests
LZC_pvalues = joint_tests(timeofday_LZC) %>%
    as.data.frame() %>% 
          mutate(across(is.numeric, round, digits=4))

MSE_pvalues = joint_tests(timeofday_MSE) %>%
    as.data.frame() %>% 
          mutate(across(is.numeric, round, digits=4))

## ----- Marginal Means --------------
LZC_marginalmeans = timeofday_LZC %>%
  emmeans(specs = ~ Responder:is_before_12, by = c('Task'))  %>%
    as.data.frame() %>% 
      mutate(across(is.numeric, round, digits=4))

MSE_marginalmeans = timeofday_MSE %>%
  emmeans(specs = ~ rep.meas:Responder, by = c('Task', 'is_before_12'))  %>%
    as.data.frame() %>% 
      mutate(across(is.numeric, round, digits=4))

fig1 <- emmip(timeofday_LZC, is_before_12 ~ Task | Responder, CIs = TRUE, col = "black",
      linearg = list(linetype = 'blank'), facetlab = 'label_value', xlab = 'Task', ylab = "LZC", tlab = ('Recording Time'), dotarg = list(size = 2), CIarg = list(alpha = 0.5)) +
    theme_bw()

fig2 <-  emmip(timeofday_LZC, Responder ~ is_before_12 | Task, CIs = TRUE, col = "black",
      linearg = list(linetype = 'blank'), facetlab = 'label_value', xlab = 'Response Status', ylab = "LZC", tlab = ('Recording Time'), dotarg = list(size = 2), CIarg = list(alpha = 0.5)) +
    theme_bw()

fig3 <- emmip(timeofday_MSE, Responder ~ rep.meas | is_before_12*Task, CIs = TRUE, col = "black",
      linearg = list(), facetlab = 'label_value', xlab = 'Scale', ylab = "Sample Entropy", tlab = ('Response Status'), dotarg = list(size = 2), CIarg = list(alpha = 0.5)) +
    theme_bw()
fig3 <- fig3 + theme(axis.text.x = element_text(angle = 90))

fig4 <-  emmip(timeofday_MSE, is_before_12 ~ rep.meas | Task*Responder, CIs = TRUE, col = "black",
      linearg = list(), facetlab = 'label_value', xlab = 'Scale', ylab = "Sample Entropy", tlab = ('Recording Time'), dotarg = list(size = 2), CIarg = list(alpha = 0.5)) +
    theme_bw()
fig4 <- fig4 + theme(axis.text.x = element_text(angle = 90))

## ----- Data Export --------------
# Export tables
write.xlsx(LZCeffects, outputFile, sheetName="LZC_TimeofDay_EffectEstimates",  append=TRUE)
write.xlsx(LZC_pvalues, outputFile, sheetName="LZC_TimeofDay_Pvalues",  append=TRUE)
write.xlsx(LZC_marginalmeans, outputFile, sheetName="LZC_TimeofDay_MarginalMeans",  append=TRUE)

write.xlsx(MSEeffects, outputFile, sheetName="MSE_TimeofDay_EffectEstimates",  append=TRUE)
write.xlsx(MSE_pvalues, outputFile, sheetName="MSE_TimeofDay_Pvalues",  append=TRUE)
write.xlsx(MSE_marginalmeans, outputFile, sheetName="MSE_TimeofDay_MarginalMeans",  append=TRUE)

# Save figures
setwd('')
ggsave("LZC_TimeofDay_Interaction1.jpeg", plot = fig1, width = 10, height = 10)
ggsave("LZC_TimeofDay_Interaction2.jpeg", plot = fig2, width = 10, height = 10)
ggsave("MSE_TimeofDay_Interaction1.jpeg", plot = fig3, width = 10, height = 10)
ggsave("MSE_TimeofDay_Interaction2.jpeg", plot = fig4, width = 10, height = 10)