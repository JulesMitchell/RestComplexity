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
finalModel <- readRDS("LZCchan_finalModel.rda.rds") # adjust filename as required

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
fig2 <-mcmc_neff_hist(neff_vals)  + theme_bw() +
  labs(title = "Effective Sampling Plot") +
  theme(legend.position = c(.95, .2))

# 3. Autocorrelation plot
fig3a <- mcmc_acf(draws_fit, 
         pars = vars("b_Fp1_Intercept":"b_F3_Intercept"),
         lags = 5) +
  labs(title = "Intercept Autocorrelation Plots") +
  theme(legend.position = c(.95, .2))

fig3b <- mcmc_acf(draws_fit, 
         pars = vars("b_Fp1_ResponderRESP":"b_Fp1_ResponderRESP:TimepointFUP:TaskEO"),
         lags = 5) +
  labs(title = "Fp1 Autocorrelation Plots") +
  theme(legend.position = c(.95, .2))

fig3c <- mcmc_acf(draws_fit, 
         pars = vars("b_AF3_ResponderRESP":"b_AF3_ResponderRESP:TimepointFUP:TaskEO"),
         lags = 5) +
  labs(title = "AF3 Autocorrelation Plots") +
  theme(legend.position = c(.95, .2))

fig3d <- mcmc_acf(draws_fit, 
         pars = vars("b_FC1_ResponderRESP":"b_FC1_ResponderRESP:TimepointFUP:TaskEO"),
         lags = 5) +
  labs(title = "FC1 Autocorrelation Plots") +
  theme(legend.position = c(.95, .2))

fig3e <- mcmc_acf(draws_fit, 
         pars = vars("b_F3_ResponderRESP":"b_F3_ResponderRESP:TimepointFUP:TaskEO"),
         lags = 5) +
  labs(title = "F3 Autocorrelation Plots") +
  theme(legend.position = c(.95, .2))

# 4. Plot other diagnostics
fig4a <- mcmc_trace(draws_fit, 
         pars = vars("b_Fp1_Intercept":"b_F3_Intercept")) +
  labs(title = "Intercept Trace Plots") +
  theme(legend.position = c(.95, .2))

fig4b <- mcmc_trace(draws_fit, 
         pars = vars("b_Fp1_ResponderRESP":"b_Fp1_ResponderRESP:TimepointFUP:TaskEO")) +
  labs(title = "Fp1 Trace Plots") +
  theme(legend.position = c(.95, .2))

fig4c <- mcmc_trace(draws_fit, 
         pars = vars("b_AF3_ResponderRESP":"b_AF3_ResponderRESP:TimepointFUP:TaskEO")) +
  labs(title = "AF3 Trace Plots") +
  theme(legend.position = c(.95, .2))

fig4d <- mcmc_trace(draws_fit, 
         pars = vars("b_FC1_ResponderRESP":"b_FC1_ResponderRESP:TimepointFUP:TaskEO")) +
  labs(title = "FC1 Trace Plots") +
  theme(legend.position = c(.95, .2))

fig4e <- mcmc_trace(draws_fit, 
         pars = vars("b_F3_ResponderRESP":"b_F3_ResponderRESP:TimepointFUP:TaskEO")) +
  labs(title = "F3 Trace Plots") +
  theme(legend.position = c(.95, .2))

# Other important diagnostics
fig5a <- pp_check(finalModel, type = "stat_2d", resp = 'Fp1') 
fig5b <- pp_check(finalModel, type = "stat_2d", resp = 'AF3') 
fig5c <- pp_check(finalModel, type = "stat_2d", resp = 'FC1') 
fig5d <- pp_check(finalModel, type = "stat_2d", resp = 'F3') 

fig6a <- pp_check(finalModel, type = "scatter_avg", resp = 'Fp1', ndraws = 100) 
fig6b <- pp_check(finalModel, type = "scatter_avg", resp = 'AF3', ndraws = 100) 
fig6c <- pp_check(finalModel, type = "scatter_avg", resp = 'FC1', ndraws = 100) 
fig6d <- pp_check(finalModel, type = "scatter_avg", resp = 'F3', ndraws = 100)

fig7a <- pp_check(finalModel, type = "error_hist", resp = 'Fp1', ndraws = 11) 
fig7b <- pp_check(finalModel, type = "error_hist", resp = 'AF3', ndraws = 11) 
fig7c <- pp_check(finalModel, type = "error_hist", resp = 'FC1', ndraws = 11) 
fig7d <- pp_check(finalModel, type = "error_hist", resp = 'F3', ndraws = 11) 

# 5. Plot posterior distribution checks
# Overall
fig8a <- pp_check(finalModel, resp = 'Fp1', ndraws = 100) 
fig8b <- pp_check(finalModel, resp = 'AF3', ndraws = 100) 
fig8c <- pp_check(finalModel, resp = 'FC1', ndraws = 100) 
fig8d <- pp_check(finalModel, resp = 'F3', ndraws = 100)

# Grouped checks (add resp = )
fig9a <- pp_check(finalModel, type = "stat_grouped", stat = 'min', group = "Timepoint", resp = 'Fp1') # Keep (use for priors and posteriors)
fig9b <- pp_check(finalModel, type = "stat_grouped", stat = 'min', group = "Timepoint", resp = 'AF3') # Keep (use for priors and posteriors)
fig9c <- pp_check(finalModel, type = "stat_grouped", stat = 'min', group = "Timepoint", resp = 'FC1') # Keep (use for priors and posteriors)
fig9d <- pp_check(finalModel, type = "stat_grouped", stat = 'min', group = "Timepoint", resp = 'F3') # Keep (use for priors and posteriors)

fig10a <- pp_check(finalModel, type = "stat_grouped", stat = 'mean', group = "Timepoint", resp = 'Fp1') # Keep (use for priors and posteriors)
fig10b <- pp_check(finalModel, type = "stat_grouped", stat = 'mean', group = "Timepoint", resp = 'AF3') # Keep (use for priors and posteriors)
fig10c <- pp_check(finalModel, type = "stat_grouped", stat = 'mean', group = "Timepoint", resp = 'FC1') # Keep (use for priors and posteriors)
fig10d <- pp_check(finalModel, type = "stat_grouped", stat = 'mean', group = "Timepoint", resp = 'F3')

fig11a <- pp_check(finalModel, type = "stat_grouped", stat = 'max', group = "Timepoint", resp = 'Fp1') # Keep (use for priors and posteriors)
fig11b <- pp_check(finalModel, type = "stat_grouped", stat = 'max', group = "Timepoint", resp = 'AF3') # Keep (use for priors and posteriors)
fig11c <- pp_check(finalModel, type = "stat_grouped", stat = 'max', group = "Timepoint", resp = 'FC1') # Keep (use for priors and posteriors)
fig11d <- pp_check(finalModel, type = "stat_grouped", stat = 'max', group = "Timepoint", resp = 'F3') # Keep (use for priors and posteriors)

fig12a <- pp_check(finalModel, type = "stat_grouped", stat = 'min', group = "Task", resp = 'Fp1') # Keep (use for priors and posteriors)
fig12b <- pp_check(finalModel, type = "stat_grouped", stat = 'min', group = "Task", resp = 'AF3') # Keep (use for priors and posteriors)
fig12c <- pp_check(finalModel, type = "stat_grouped", stat = 'min', group = "Task", resp = 'FC1') # Keep (use for priors and posteriors)
fig12d <- pp_check(finalModel, type = "stat_grouped", stat = 'min', group = "Task", resp = 'F3') # Keep (use for priors and posteriors)

fig13a <- pp_check(finalModel, type = "stat_grouped", stat = 'mean', group = "Task", resp = 'Fp1') # Keep (use for priors and posteriors)
fig13b <- pp_check(finalModel, type = "stat_grouped", stat = 'mean', group = "Task", resp = 'AF3') # Keep (use for priors and posteriors)
fig13c <- pp_check(finalModel, type = "stat_grouped", stat = 'mean', group = "Task", resp = 'FC1') # Keep (use for priors and posteriors)
fig13d <- pp_check(finalModel, type = "stat_grouped", stat = 'mean', group = "Task", resp = 'F3') # Keep (use for priors and posteriors)

fig14a <- pp_check(finalModel, type = "stat_grouped", stat = 'max', group = "Task", resp = 'Fp1') # Keep (use for priors and posteriors)
fig14b <- pp_check(finalModel, type = "stat_grouped", stat = 'max', group = "Task", resp = 'AF3') # Keep (use for priors and posteriors)
fig14c <- pp_check(finalModel, type = "stat_grouped", stat = 'max', group = "Task", resp = 'FC1') # Keep (use for priors and posteriors)
fig14d <- pp_check(finalModel, type = "stat_grouped", stat = 'max', group = "Task", resp = 'F3') # Keep (use for priors and posteriors)

fig15a <- pp_check(finalModel, type = "stat_grouped", stat = 'min', group = "Responder", resp = 'Fp1') # Keep (use for priors and posteriors)
fig15b <- pp_check(finalModel, type = "stat_grouped", stat = 'min', group = "Responder", resp = 'AF3') # Keep (use for priors and posteriors)
fig15c <- pp_check(finalModel, type = "stat_grouped", stat = 'min', group = "Responder", resp = 'FC1') # Keep (use for priors and posteriors)
fig15d <- pp_check(finalModel, type = "stat_grouped", stat = 'min', group = "Responder", resp = 'F3') # Keep (use for priors and posteriors)

fig16a <- pp_check(finalModel, type = "stat_grouped", stat = 'mean', group = "Responder", resp = 'Fp1') # Keep (use for priors and posteriors)
fig16b <- pp_check(finalModel, type = "stat_grouped", stat = 'mean', group = "Responder", resp = 'AF3') # Keep (use for priors and posteriors)
fig16c <- pp_check(finalModel, type = "stat_grouped", stat = 'mean', group = "Responder", resp = 'FC1') # Keep (use for priors and posteriors)
fig16d <- pp_check(finalModel, type = "stat_grouped", stat = 'mean', group = "Responder", resp = 'F3') # Keep (use for priors and posteriors)

fig17a <- pp_check(finalModel, type = "stat_grouped", stat = 'max', group = "Responder", resp = 'Fp1') # Keep (use for priors and posteriors)
fig17b <- pp_check(finalModel, type = "stat_grouped", stat = 'max', group = "Responder", resp = 'AF3') # Keep (use for priors and posteriors)
fig17c <- pp_check(finalModel, type = "stat_grouped", stat = 'max', group = "Responder", resp = 'FC1') # Keep (use for priors and posteriors)
fig17d <- pp_check(finalModel, type = "stat_grouped", stat = 'max', group = "Responder", resp = 'F3') # Keep (use for priors and posteriors)

## ---- Figure Export -----------------
# Set directory
setwd('')

# Save figures
ggsave("LZCChan_fullModel_Rhat.jpeg", plot = fig1, width = 10, height = 10)
ggsave("LZCChan_fullModel_Neff.jpeg", plot = fig2, width = 10, height = 10)

ggsave("LZCChan_finalModel_AutoCorr_Intercept.jpeg", plot = fig3a, width = 10, height = 10)
ggsave("LZCChan_finalModel_AutoCorr_Fp1.jpeg", plot = fig3b, width = 10, height = 10)
ggsave("LZCChan_finalModel_AutoCorr_AF3.jpeg", plot = fig3c, width = 10, height = 10)
ggsave("LZCChan_finalModel_AutoCorr_FC1.jpeg", plot = fig3d, width = 10, height = 10)
ggsave("LZCChan_finalModel_AutoCorr_F3.jpeg", plot = fig3e, width = 10, height = 10)

ggsave("LZCChan_finalModel_Trace_Intercept.jpeg", plot = fig4a, width = 10, height = 10)
ggsave("LZCChan_finalModel_Trace_Fp1.jpeg", plot = fig4b, width = 10, height = 10)
ggsave("LZCChan_finalModel_Trace_AF3.jpeg", plot = fig4c, width = 10, height = 10)
ggsave("LZCChan_finalModel_Trace_FC1.jpeg", plot = fig4d, width = 10, height = 10)
ggsave("LZCChan_finalModel_Trace_F3.jpeg", plot = fig4e, width = 10, height = 10)

ggsave("LZCChan_finalModel_stat_2d_Fp1.jpeg", plot = fig5a, width = 10, height = 10)
ggsave("LZCChan_finalModel_stat_2d_AF3.jpeg", plot = fig5b, width = 10, height = 10)
ggsave("LZCChan_finalModel_stat_2d_FC1.jpeg", plot = fig5c, width = 10, height = 10)
ggsave("LZCChan_finalModel_stat_2d_F3.jpeg", plot = fig5d, width = 10, height = 10)

ggsave("LZCChan_finalModel_ObsVsPred_Fp1.jpeg", plot = fig6a, width = 10, height = 10)
ggsave("LZCChan_finalModel_ObsVsPred_AF3.jpeg", plot = fig6b, width = 10, height = 10)
ggsave("LZCChan_finalModel_ObsVsPred_FC1.jpeg", plot = fig6c, width = 10, height = 10)
ggsave("LZCChan_finalModel_ObsVsPred_F3.jpeg", plot = fig6d, width = 10, height = 10)

ggsave("LZCChan_finalModel_ErrorDistribution_Fp1.jpeg", plot = fig7a, width = 10, height = 10)
ggsave("LZCChan_finalModel_ErrorDistribution_AF3.jpeg", plot = fig7b, width = 10, height = 10)
ggsave("LZCChan_finalModel_ErrorDistribution_FC1.jpeg", plot = fig7c, width = 10, height = 10)
ggsave("LZCChan_finalModel_ErrorDistribution_F3.jpeg", plot = fig7d, width = 10, height = 10)

ggsave("LZCChan_finalModel_PostCheck_DensOverlay_Fp1.jpeg", plot = fig8a, width = 10, height = 10)
ggsave("LZCChan_finalModel_PostCheck_DensOverlay_AF3.jpeg", plot = fig8b, width = 10, height = 10)
ggsave("LZCChan_finalModel_PostCheck_DensOverlay_FC1.jpeg", plot = fig8c, width = 10, height = 10)
ggsave("LZCChan_finalModel_PostCheck_DensOverlay_F3.jpeg", plot = fig8d, width = 10, height = 10)

ggsave("LZCChan_finalModel_PostCheck_Timepoint_min_Fp1.jpeg", plot = fig9a, width = 10, height = 10)
ggsave("LZCChan_finalModel_PostCheck_Timepoint_min_AF3.jpeg", plot = fig9b, width = 10, height = 10)
ggsave("LZCChan_finalModel_PostCheck_Timepoint_min_FC1.jpeg", plot = fig9c, width = 10, height = 10)
ggsave("LZCChan_finalModel_PostCheck_Timepoint_min_F3.jpeg", plot = fig9d, width = 10, height = 10)

ggsave("LZCChan_finalModel_PostCheck_Timepoint_mean_Fp1.jpeg", plot = fig10a, width = 10, height = 10)
ggsave("LZCChan_finalModel_PostCheck_Timepoint_mean_AF3.jpeg", plot = fig10b, width = 10, height = 10)
ggsave("LZCChan_finalModel_PostCheck_Timepoint_mean_FC1.jpeg", plot = fig10c, width = 10, height = 10)
ggsave("LZCChan_finalModel_PostCheck_Timepoint_mean_F3.jpeg", plot = fig10d, width = 10, height = 10)

ggsave("LZCChan_finalModel_PostCheck_Timepoint_max_Fp1.jpeg", plot = fig11a, width = 10, height = 10)
ggsave("LZCChan_finalModel_PostCheck_Timepoint_max_AF3.jpeg", plot = fig11b, width = 10, height = 10)
ggsave("LZCChan_finalModel_PostCheck_Timepoint_max_FC1.jpeg", plot = fig11c, width = 10, height = 10)
ggsave("LZCChan_finalModel_PostCheck_Timepoint_max_F3.jpeg", plot = fig11d, width = 10, height = 10)

ggsave("LZCChan_finalModel_PostCheck_Task_min_Fp1.jpeg", plot = fig12a, width = 10, height = 10)
ggsave("LZCChan_finalModel_PostCheck_Task_min_AF3.jpeg", plot = fig12b, width = 10, height = 10)
ggsave("LZCChan_finalModel_PostCheck_Task_min_FC1.jpeg", plot = fig12c, width = 10, height = 10)
ggsave("LZCChan_finalModel_PostCheck_Task_min_F3.jpeg", plot = fig12d, width = 10, height = 10)

ggsave("LZCChan_finalModel_PostCheck_Task_mean_Fp1.jpeg", plot = fig13a, width = 10, height = 10)
ggsave("LZCChan_finalModel_PostCheck_Task_mean_AF3.jpeg", plot = fig13b, width = 10, height = 10)
ggsave("LZCChan_finalModel_PostCheck_Task_mean_FC1.jpeg", plot = fig13c, width = 10, height = 10)
ggsave("LZCChan_finalModel_PostCheck_Task_mean_F3.jpeg", plot = fig13d, width = 10, height = 10)

ggsave("LZCChan_finalModel_PostCheck_Task_max_Fp1.jpeg", plot = fig14a, width = 10, height = 10)
ggsave("LZCChan_finalModel_PostCheck_Task_max_AF3.jpeg", plot = fig14b, width = 10, height = 10)
ggsave("LZCChan_finalModel_PostCheck_Task_max_FC1.jpeg", plot = fig14c, width = 10, height = 10)
ggsave("LZCChan_finalModel_PostCheck_Task_max_F3.jpeg", plot = fig14d, width = 10, height = 10)

ggsave("LZCChan_finalModel_PostCheck_Responder_min_Fp1.jpeg", plot = fig15a, width = 10, height = 10)
ggsave("LZCChan_finalModel_PostCheck_Responder_min_AF3.jpeg", plot = fig15b, width = 10, height = 10)
ggsave("LZCChan_finalModel_PostCheck_Responder_min_FC1.jpeg", plot = fig15c, width = 10, height = 10)
ggsave("LZCChan_finalModel_PostCheck_Responder_min_F3.jpeg", plot = fig15d, width = 10, height = 10)

ggsave("LZCChan_finalModel_PostCheck_Responder_mean_Fp1.jpeg", plot = fig16a, width = 10, height = 10)
ggsave("LZCChan_finalModel_PostCheck_Responder_mean_AF3.jpeg", plot = fig16b, width = 10, height = 10)
ggsave("LZCChan_finalModel_PostCheck_Responder_mean_FC1.jpeg", plot = fig16c, width = 10, height = 10)
ggsave("LZCChan_finalModel_PostCheck_Responder_mean_F3.jpeg", plot = fig16d, width = 10, height = 10)

ggsave("LZCChan_finalModel_PostCheck_Responder_max_Fp1.jpeg", plot = fig17a, width = 10, height = 10)
ggsave("LZCChan_finalModel_PostCheck_Responder_max_AF3.jpeg", plot = fig17b, width = 10, height = 10)
ggsave("LZCChan_finalModel_PostCheck_Responder_max_FC1.jpeg", plot = fig17c, width = 10, height = 10)
ggsave("LZCChan_finalModel_PostCheck_Responder_max_F3.jpeg", plot = fig17d, width = 10, height = 10)