### Model convergence and assumption checks ###
## ----pkgload, include = FALSE-------------------------------------------------
library(ggplot2)
library(brms)
library(dplyr)
library(tidyverse) # needed for data manipulation

## ---- Set working directory as required -------------------------------------------------
setwd('')

## ---- Load model object and save rhat and neff values -----------------
# 1. To load
finalModel <- readRDS("LZC_finalModel.rda.rds") # adjust filename as required

# 2. Extract posterior samples for inference
draws_fit <- as_draws_df(finalModel)

# 3. Save rhat values
rhat_vals <- rhat(finalModel)

# 4. Save Number of effective samples
neff_vals <- neff_ratio(finalModel)

## ---- Plotting-----------------
library(bayesplot) # Loaded after rhat as it is masked by BRMS

# 1. rhat  
fig1 <- mcmc_rhat_hist(rhat_vals) + theme_bw() +
  labs(title = "Rhat Plot") +
  theme(legend.position = c(.95, .2))

# 2. Neff
fig2 <- mcmc_neff_hist(neff_vals)  + theme_bw() +
  labs(title = "Effective Sampling Plot") +
  theme(legend.position = c(.95, .2))

# 3. Autocorrelation plot
fig3a <- mcmc_acf(draws_fit, 
         pars = vars(b_Intercept:"b_ResponderRESP:TaskEO"),
         lags = 5) +
  labs(title = "Autocorrelation Plots") +
  theme(legend.position = c(.95, .2))

fig3b <- mcmc_acf(draws_fit, 
         pars = vars("b_TimepointPOST:TaskEO":sigma),
         lags = 5) +
  labs(title = "Autocorrelation Plots") +
  theme(legend.position = c(.95, .2))

# 4. Plot other diagnostics
fig4a <- mcmc_trace(draws_fit, 
           pars = vars(b_Intercept:"b_ResponderRESP:TaskEO")) +
  labs(title = "Trace Plots") +
  theme(legend.position = c(.95, .2))

fig4b <- mcmc_trace(draws_fit, 
           pars = vars("b_TimepointPOST:TaskEO":sigma)) +
  labs(title = "Trace Plots") +
  theme(legend.position = c(.95, .2))

fig5 <- pp_check(finalModel, type = "stat_2d") # Keep posterior only
fig6 <- pp_check(finalModel, type = "scatter_avg", ndraws = 100)
fig7 <- pp_check(finalModel, type = "error_hist", ndraws = 11) 

# 5. Plot posterior distribution checks
# Overall
fig8 <- pp_check(finalModel, ndraws = 100)

# Grouped checks
fig9 <- pp_check(finalModel, type = "stat_grouped", stat = 'min', group = "Timepoint") # Keep (use for priors and posteriors)
fig10 <- pp_check(finalModel, type = "stat_grouped", stat = 'mean', group = "Timepoint") # Keep (use for priors and posteriors)
fig11 <- pp_check(finalModel, type = "stat_grouped", stat = 'max', group = "Timepoint") # Keep (use for priors and posteriors)

fig12 <- pp_check(finalModel, type = "stat_grouped", stat = 'min', group = "Task") # Keep (use for priors and posteriors)
fig13 <- pp_check(finalModel, type = "stat_grouped", stat = 'mean', group = "Task") # Keep (use for priors and posteriors)
fig14 <- pp_check(finalModel, type = "stat_grouped", stat = 'max', group = "Task") # Keep (use for priors and posteriors)

fig15 <- pp_check(finalModel, type = "stat_grouped", stat = 'min', group = "Responder") # Keep (use for priors and posteriors)
fig16 <- pp_check(finalModel, type = "stat_grouped", stat = 'mean', group = "Responder") # Keep (use for priors and posteriors)
fig17 <- pp_check(finalModel, type = "stat_grouped", stat = 'max', group = "Responder") # Keep (use for priors and posteriors)

## ---- Figure Export -----------------
# Set directory
setwd('')

# Save figures
ggsave("LZC_Model2_Rhat.jpeg", plot = fig1, width = 10, height = 10)
ggsave("LZC_Model2_Neff.jpeg", plot = fig2, width = 10, height = 10)
ggsave("LZC_Model2_AutoCorr_A.jpeg", plot = fig3a, width = 10, height = 10)
ggsave("LZC_Model2_AutoCorr_B.jpeg", plot = fig3b, width = 10, height = 10)
ggsave("LZC_Model2_Traces_A.jpeg", plot = fig4a, width = 10, height = 10)
ggsave("LZC_Model2_Traces_B.jpeg", plot = fig4b, width = 10, height = 10)

ggsave("LZC_Model2_stat_2d.jpeg", plot = fig5, width = 10, height = 10)
ggsave("LZC_Model2_ObsVsPred.jpeg", plot = fig6, width = 10, height = 10)
ggsave("LZC_Model2_ErrorDistribution.jpeg", plot = fig7, width = 10, height = 10)

ggsave("LZC_Model2_PostCheck_DensOverlay.jpeg", plot = fig8, width = 10, height = 10)

ggsave("LZC_Model2_PostCheck_Timepoint_min.jpeg", plot = fig9, width = 10, height = 10)
ggsave("LZC_Model2_PostCheck_Timepoint_mean.jpeg", plot = fig10, width = 10, height = 10)
ggsave("LZC_Model2_PostCheck_Timepoint_max.jpeg", plot = fig11, width = 10, height = 10)

ggsave("LZC_Model2_PostCheck_Task_min.jpeg", plot = fig12, width = 10, height = 10)
ggsave("LZC_Model2_PostCheck_Task_mean.jpeg", plot = fig13, width = 10, height = 10)
ggsave("LZC_Model2_PostCheck_Task_max.jpeg", plot = fig14, width = 10, height = 10)

ggsave("LZC_Model2_PostCheck_Responder_min.jpeg", plot = fig15, width = 10, height = 10)
ggsave("LZC_Model2_PostCheck_Responder_mean.jpeg", plot = fig16, width = 10, height = 10)
ggsave("LZC_Model2_PostCheck_Responder_max.jpeg", plot = fig17, width = 10, height = 10)