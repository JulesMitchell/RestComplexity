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
finalModel <- readRDS("MSE_MVfinalModel.rds") # adjust filename as required

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

# 3. Autocorrelation plot (Try hist plot)
fig3a <- mcmc_acf(draws_fit, 
         pars = vars("b_Scale1_Intercept":"b_Scale10_Intercept"),
         lags = 5) +
  labs(title = "Autocorrelation Plots") +
  theme(legend.position = c(.95, .2))

fig3b <- mcmc_acf(draws_fit, 
         pars = vars("b_Scale1_ResponderRESP":"b_Scale1_ResponderRESP:TimepointFUP:TaskEO"),
         lags = 5) +
  labs(title = "Scale 1 Autocorrelation Plots") +
  theme(legend.position = c(.95, .2))

fig3c <- mcmc_acf(draws_fit, 
         pars = vars("b_Scale2_ResponderRESP":"b_Scale2_ResponderRESP:TimepointFUP:TaskEO"),
         lags = 5) +
  labs(title = "Scale 2 Autocorrelation Plots") +
  theme(legend.position = c(.95, .2))

fig3d <- mcmc_acf(draws_fit, 
         pars = vars("b_Scale3_ResponderRESP":"b_Scale3_ResponderRESP:TimepointFUP:TaskEO"),
         lags = 5) +
  labs(title = "Scale 3 Autocorrelation Plots") +
  theme(legend.position = c(.95, .2))

fig3e <- mcmc_acf(draws_fit, 
         pars = vars("b_Scale4_ResponderRESP":"b_Scale4_ResponderRESP:TimepointFUP:TaskEO"),
         lags = 5) +
  labs(title = "Scale 4 Autocorrelation Plots") +
  theme(legend.position = c(.95, .2))

fig3f <- mcmc_acf(draws_fit, 
         pars = vars("b_Scale5_ResponderRESP":"b_Scale5_ResponderRESP:TimepointFUP:TaskEO"),
         lags = 5) +
  labs(title = "Scale 5 Autocorrelation Plots") +
  theme(legend.position = c(.95, .2))

fig3g <- mcmc_acf(draws_fit, 
         pars = vars("b_Scale6_ResponderRESP":"b_Scale6_ResponderRESP:TimepointFUP:TaskEO"),
         lags = 5) +
  labs(title = "Scale 6 Autocorrelation Plots") +
  theme(legend.position = c(.95, .2))

fig3h <- mcmc_acf(draws_fit, 
         pars = vars("b_Scale7_ResponderRESP":"b_Scale7_ResponderRESP:TimepointFUP:TaskEO"),
         lags = 5) +
  labs(title = "Scale 7 Autocorrelation Plots") +
  theme(legend.position = c(.95, .2))

fig3i <- mcmc_acf(draws_fit, 
         pars = vars("b_Scale8_ResponderRESP":"b_Scale8_ResponderRESP:TimepointFUP:TaskEO"),
         lags = 5) +
  labs(title = "Scale 8 Autocorrelation Plots") +
  theme(legend.position = c(.95, .2))

fig3j <- mcmc_acf(draws_fit, 
         pars = vars("b_Scale9_ResponderRESP":"b_Scale9_ResponderRESP:TimepointFUP:TaskEO"),
         lags = 5) +
  labs(title = "Scale 9 Autocorrelation Plots") +
  theme(legend.position = c(.95, .2))

fig3k <- mcmc_acf(draws_fit, 
         pars = vars("b_Scale10_ResponderRESP":"b_Scale10_ResponderRESP:TimepointFUP:TaskEO"),
         lags = 5) +
  labs(title = "Scale 10 Autocorrelation Plots") +
  theme(legend.position = c(.95, .2))  

# 4. Plot other diagnostics (try plot or mcmc_acf(draws_fit, pars = "") 
fig4a <- mcmc_trace(draws_fit, 
         pars = vars("b_Scale1_Intercept":"b_Scale10_Intercept"))+
  labs(title = "Intercept Trace Plots") +
  theme(legend.position = c(.95, .2))

fig4b <- mcmc_trace(draws_fit, 
         pars = vars("b_Scale1_ResponderRESP":"b_Scale1_ResponderRESP:TimepointFUP:TaskEO")) +
  labs(title = "Scale 1 Trace Plots") +
  theme(legend.position = c(.95, .2))

fig4c <- mcmc_trace(draws_fit, 
         pars = vars("b_Scale2_ResponderRESP":"b_Scale2_ResponderRESP:TimepointFUP:TaskEO")) +
  labs(title = "Scale 2 Trace Plots") +
  theme(legend.position = c(.95, .2))

fig4d <- mcmc_trace(draws_fit, 
         pars = vars("b_Scale3_ResponderRESP":"b_Scale3_ResponderRESP:TimepointFUP:TaskEO")) +
  labs(title = "Scale 3 Trace Plots") +
  theme(legend.position = c(.95, .2))

fig4e <- mcmc_trace(draws_fit, 
         pars = vars("b_Scale4_ResponderRESP":"b_Scale4_ResponderRESP:TimepointFUP:TaskEO")) +
  labs(title = "Scale 4 Trace Plots") +
  theme(legend.position = c(.95, .2))

fig4f <- mcmc_trace(draws_fit, 
         pars = vars("b_Scale5_ResponderRESP":"b_Scale5_ResponderRESP:TimepointFUP:TaskEO")) +
  labs(title = "Scale 5 Trace Plots") +
  theme(legend.position = c(.95, .2))

fig4g <- mcmc_trace(draws_fit, 
         pars = vars("b_Scale6_ResponderRESP":"b_Scale6_ResponderRESP:TimepointFUP:TaskEO")) +
  labs(title = "Scale 6 Trace Plots") +
  theme(legend.position = c(.95, .2))

fig4h <- mcmc_trace(draws_fit, 
         pars = vars("b_Scale7_ResponderRESP":"b_Scale7_ResponderRESP:TimepointFUP:TaskEO")) +
  labs(title = "Scale 7 Trace Plots") +
  theme(legend.position = c(.95, .2))

fig4i <- mcmc_trace(draws_fit, 
         pars = vars("b_Scale8_ResponderRESP":"b_Scale8_ResponderRESP:TimepointFUP:TaskEO")) +
  labs(title = "Scale 8 Trace Plots") +
  theme(legend.position = c(.95, .2))

fig4j <- mcmc_trace(draws_fit, 
         pars = vars("b_Scale9_ResponderRESP":"b_Scale9_ResponderRESP:TimepointFUP:TaskEO")) +
  labs(title = "Scale 9 Trace Plots") +
  theme(legend.position = c(.95, .2))

fig4k <- mcmc_trace(draws_fit, 
         pars = vars("b_Scale10_ResponderRESP":"b_Scale10_ResponderRESP:TimepointFUP:TaskEO")) +
  labs(title = "Scale 10 Trace Plots") +
  theme(legend.position = c(.95, .2)) 

# Other important diagnostics
fig5a <- pp_check(finalModel, type = "stat_2d", resp = 'Scale1') 
fig5b <- pp_check(finalModel, type = "stat_2d", resp = 'Scale2') 
fig5c <- pp_check(finalModel, type = "stat_2d", resp = 'Scale3') 
fig5d <- pp_check(finalModel, type = "stat_2d", resp = 'Scale4') 
fig5e <- pp_check(finalModel, type = "stat_2d", resp = 'Scale5') 
fig5f <- pp_check(finalModel, type = "stat_2d", resp = 'Scale6') 
fig5g <- pp_check(finalModel, type = "stat_2d", resp = 'Scale7') 
fig5h <- pp_check(finalModel, type = "stat_2d", resp = 'Scale8') 
fig5i <- pp_check(finalModel, type = "stat_2d", resp = 'Scale9') 
fig5j <- pp_check(finalModel, type = "stat_2d", resp = 'Scale10') 

fig6a <- pp_check(finalModel, type = "scatter_avg", resp = 'Scale1', ndraws = 100) 
fig6b <- pp_check(finalModel, type = "scatter_avg", resp = 'Scale2', ndraws = 100) 
fig6c <- pp_check(finalModel, type = "scatter_avg", resp = 'Scale3', ndraws = 100) 
fig6d <- pp_check(finalModel, type = "scatter_avg", resp = 'Scale4', ndraws = 100) 
fig6e <- pp_check(finalModel, type = "scatter_avg", resp = 'Scale5', ndraws = 100) 
fig6f <- pp_check(finalModel, type = "scatter_avg", resp = 'Scale6', ndraws = 100) 
fig6g <- pp_check(finalModel, type = "scatter_avg", resp = 'Scale7', ndraws = 100) 
fig6h <- pp_check(finalModel, type = "scatter_avg", resp = 'Scale8', ndraws = 100) 
fig6i <- pp_check(finalModel, type = "scatter_avg", resp = 'Scale9', ndraws = 100) 
fig6j <- pp_check(finalModel, type = "scatter_avg", resp = 'Scale10', ndraws = 100) 

fig7a <- pp_check(finalModel, type = "error_hist", resp = 'Scale1', ndraws = 11) 
fig7b <- pp_check(finalModel, type = "error_hist", resp = 'Scale2', ndraws = 11) 
fig7c <- pp_check(finalModel, type = "error_hist", resp = 'Scale3', ndraws = 11) 
fig7d <- pp_check(finalModel, type = "error_hist", resp = 'Scale4', ndraws = 11) 
fig7e <- pp_check(finalModel, type = "error_hist", resp = 'Scale5', ndraws = 11) 
fig7f <- pp_check(finalModel, type = "error_hist", resp = 'Scale6', ndraws = 11) 
fig7g <- pp_check(finalModel, type = "error_hist", resp = 'Scale7', ndraws = 11) 
fig7h <- pp_check(finalModel, type = "error_hist", resp = 'Scale8', ndraws = 11) 
fig7i <- pp_check(finalModel, type = "error_hist", resp = 'Scale9', ndraws = 11) 
fig7j <- pp_check(finalModel, type = "error_hist", resp = 'Scale10', ndraws = 11) 

# 5. Plot posterior distribution checks
# Overall
fig8a <- pp_check(finalModel, resp = 'Scale1', ndraws = 100) 
fig8b <- pp_check(finalModel, resp = 'Scale2', ndraws = 100) 
fig8c <- pp_check(finalModel, resp = 'Scale3', ndraws = 100) 
fig8d <- pp_check(finalModel, resp = 'Scale4', ndraws = 100) 
fig8e <- pp_check(finalModel, resp = 'Scale5', ndraws = 100) 
fig8f <- pp_check(finalModel, resp = 'Scale6', ndraws = 100) 
fig8g <- pp_check(finalModel, resp = 'Scale7', ndraws = 100) 
fig8h <- pp_check(finalModel, resp = 'Scale8', ndraws = 100) 
fig8i <- pp_check(finalModel, resp = 'Scale9', ndraws = 100) 
fig8j <- pp_check(finalModel, resp = 'Scale10', ndraws = 100) 

# Grouped checks
#Timepoint
fig9a <- pp_check(finalModel, type = "stat_grouped", stat = 'min', group = "Timepoint", resp = 'Scale1') # Keep (use for priors and posteriors)
fig9b <- pp_check(finalModel, type = "stat_grouped", stat = 'min', group = "Timepoint", resp = 'Scale2') # Keep (use for priors and posteriors)
fig9c <- pp_check(finalModel, type = "stat_grouped", stat = 'min', group = "Timepoint", resp = 'Scale3') # Keep (use for priors and posteriors)
fig9d <- pp_check(finalModel, type = "stat_grouped", stat = 'min', group = "Timepoint", resp = 'Scale4') # Keep (use for priors and posteriors)
fig9e <- pp_check(finalModel, type = "stat_grouped", stat = 'min', group = "Timepoint", resp = 'Scale5') # Keep (use for priors and posteriors)
fig9f <- pp_check(finalModel, type = "stat_grouped", stat = 'min', group = "Timepoint", resp = 'Scale6') # Keep (use for priors and posteriors)
fig9g <- pp_check(finalModel, type = "stat_grouped", stat = 'min', group = "Timepoint", resp = 'Scale7') # Keep (use for priors and posteriors)
fig9h <- pp_check(finalModel, type = "stat_grouped", stat = 'min', group = "Timepoint", resp = 'Scale8') # Keep (use for priors and posteriors)
fig9i <- pp_check(finalModel, type = "stat_grouped", stat = 'min', group = "Timepoint", resp = 'Scale9') # Keep (use for priors and posteriors)
fig9j <- pp_check(finalModel, type = "stat_grouped", stat = 'min', group = "Timepoint", resp = 'Scale10') # Keep (use for priors and posteriors)

fig10a <- pp_check(finalModel, type = "stat_grouped", stat = 'mean', group = "Timepoint", resp = 'Scale1') # Keep (use for priors and posteriors)
fig10b <- pp_check(finalModel, type = "stat_grouped", stat = 'mean', group = "Timepoint", resp = 'Scale2') # Keep (use for priors and posteriors)
fig10c <- pp_check(finalModel, type = "stat_grouped", stat = 'mean', group = "Timepoint", resp = 'Scale3') # Keep (use for priors and posteriors)
fig10d <- pp_check(finalModel, type = "stat_grouped", stat = 'mean', group = "Timepoint", resp = 'Scale4') # Keep (use for priors and posteriors)
fig10e <- pp_check(finalModel, type = "stat_grouped", stat = 'mean', group = "Timepoint", resp = 'Scale5') # Keep (use for priors and posteriors)
fig10f <- pp_check(finalModel, type = "stat_grouped", stat = 'mean', group = "Timepoint", resp = 'Scale6') # Keep (use for priors and posteriors)
fig10g <- pp_check(finalModel, type = "stat_grouped", stat = 'mean', group = "Timepoint", resp = 'Scale7') # Keep (use for priors and posteriors)
fig10h <- pp_check(finalModel, type = "stat_grouped", stat = 'mean', group = "Timepoint", resp = 'Scale8') # Keep (use for priors and posteriors)
fig10i <- pp_check(finalModel, type = "stat_grouped", stat = 'mean', group = "Timepoint", resp = 'Scale9') # Keep (use for priors and posteriors)
fig10j <- pp_check(finalModel, type = "stat_grouped", stat = 'mean', group = "Timepoint", resp = 'Scale10') # Keep (use for priors and posteriors)

fig11a <- pp_check(finalModel, type = "stat_grouped", stat = 'max', group = "Timepoint", resp = 'Scale1') # Keep (use for priors and posteriors)
fig11b <- pp_check(finalModel, type = "stat_grouped", stat = 'max', group = "Timepoint", resp = 'Scale2') # Keep (use for priors and posteriors)
fig11c <- pp_check(finalModel, type = "stat_grouped", stat = 'max', group = "Timepoint", resp = 'Scale3') # Keep (use for priors and posteriors)
fig11d <- pp_check(finalModel, type = "stat_grouped", stat = 'max', group = "Timepoint", resp = 'Scale4') # Keep (use for priors and posteriors)
fig11e <- pp_check(finalModel, type = "stat_grouped", stat = 'max', group = "Timepoint", resp = 'Scale5') # Keep (use for priors and posteriors)
fig11f <- pp_check(finalModel, type = "stat_grouped", stat = 'max', group = "Timepoint", resp = 'Scale6') # Keep (use for priors and posteriors)
fig11g <- pp_check(finalModel, type = "stat_grouped", stat = 'max', group = "Timepoint", resp = 'Scale7') # Keep (use for priors and posteriors)
fig11h <- pp_check(finalModel, type = "stat_grouped", stat = 'max', group = "Timepoint", resp = 'Scale8') # Keep (use for priors and posteriors)
fig11i <- pp_check(finalModel, type = "stat_grouped", stat = 'max', group = "Timepoint", resp = 'Scale9') # Keep (use for priors and posteriors)
fig11j <- pp_check(finalModel, type = "stat_grouped", stat = 'max', group = "Timepoint", resp = 'Scale10') # Keep (use for priors and posteriors)

#Task
fig12a <- pp_check(finalModel, type = "stat_grouped", stat = 'min', group = "Task", resp = 'Scale1') # Keep (use for priors and posteriors)
fig12b <- pp_check(finalModel, type = "stat_grouped", stat = 'min', group = "Task", resp = 'Scale2') # Keep (use for priors and posteriors)
fig12c <- pp_check(finalModel, type = "stat_grouped", stat = 'min', group = "Task", resp = 'Scale3') # Keep (use for priors and posteriors)
fig12d <- pp_check(finalModel, type = "stat_grouped", stat = 'min', group = "Task", resp = 'Scale4') # Keep (use for priors and posteriors)
fig12e <- pp_check(finalModel, type = "stat_grouped", stat = 'min', group = "Task", resp = 'Scale5') # Keep (use for priors and posteriors)
fig12f <- pp_check(finalModel, type = "stat_grouped", stat = 'min', group = "Task", resp = 'Scale6') # Keep (use for priors and posteriors)
fig12g <- pp_check(finalModel, type = "stat_grouped", stat = 'min', group = "Task", resp = 'Scale7') # Keep (use for priors and posteriors)
fig12h <- pp_check(finalModel, type = "stat_grouped", stat = 'min', group = "Task", resp = 'Scale8') # Keep (use for priors and posteriors)
fig12i <- pp_check(finalModel, type = "stat_grouped", stat = 'min', group = "Task", resp = 'Scale9') # Keep (use for priors and posteriors)
fig12j <- pp_check(finalModel, type = "stat_grouped", stat = 'min', group = "Task", resp = 'Scale10') # Keep (use for priors and posteriors)

fig13a <- pp_check(finalModel, type = "stat_grouped", stat = 'mean', group = "Task", resp = 'Scale1') # Keep (use for priors and posteriors)
fig13b <- pp_check(finalModel, type = "stat_grouped", stat = 'mean', group = "Task", resp = 'Scale2') # Keep (use for priors and posteriors)
fig13c <- pp_check(finalModel, type = "stat_grouped", stat = 'mean', group = "Task", resp = 'Scale3') # Keep (use for priors and posteriors)
fig13d <- pp_check(finalModel, type = "stat_grouped", stat = 'mean', group = "Task", resp = 'Scale4') # Keep (use for priors and posteriors)
fig13e <- pp_check(finalModel, type = "stat_grouped", stat = 'mean', group = "Task", resp = 'Scale5') # Keep (use for priors and posteriors)
fig13f <- pp_check(finalModel, type = "stat_grouped", stat = 'mean', group = "Task", resp = 'Scale6') # Keep (use for priors and posteriors)
fig13g <- pp_check(finalModel, type = "stat_grouped", stat = 'mean', group = "Task", resp = 'Scale7') # Keep (use for priors and posteriors)
fig13h <- pp_check(finalModel, type = "stat_grouped", stat = 'mean', group = "Task", resp = 'Scale8') # Keep (use for priors and posteriors)
fig13i <- pp_check(finalModel, type = "stat_grouped", stat = 'mean', group = "Task", resp = 'Scale9') # Keep (use for priors and posteriors)
fig13j <- pp_check(finalModel, type = "stat_grouped", stat = 'mean', group = "Task", resp = 'Scale10') # Keep (use for priors and posteriors)

fig14a <- pp_check(finalModel, type = "stat_grouped", stat = 'max', group = "Task", resp = 'Scale1') # Keep (use for priors and posteriors)
fig14b <- pp_check(finalModel, type = "stat_grouped", stat = 'max', group = "Task", resp = 'Scale2') # Keep (use for priors and posteriors)
fig14c <- pp_check(finalModel, type = "stat_grouped", stat = 'max', group = "Task", resp = 'Scale3') # Keep (use for priors and posteriors)
fig14d <- pp_check(finalModel, type = "stat_grouped", stat = 'max', group = "Task", resp = 'Scale4') # Keep (use for priors and posteriors)
fig14e <- pp_check(finalModel, type = "stat_grouped", stat = 'max', group = "Task", resp = 'Scale5') # Keep (use for priors and posteriors)
fig14f <- pp_check(finalModel, type = "stat_grouped", stat = 'max', group = "Task", resp = 'Scale6') # Keep (use for priors and posteriors)
fig14g <- pp_check(finalModel, type = "stat_grouped", stat = 'max', group = "Task", resp = 'Scale7') # Keep (use for priors and posteriors)
fig14h <- pp_check(finalModel, type = "stat_grouped", stat = 'max', group = "Task", resp = 'Scale8') # Keep (use for priors and posteriors)
fig14i <- pp_check(finalModel, type = "stat_grouped", stat = 'max', group = "Task", resp = 'Scale9') # Keep (use for priors and posteriors)
fig14j <- pp_check(finalModel, type = "stat_grouped", stat = 'max', group = "Task", resp = 'Scale10') # Keep (use for priors and posteriors)

# Responder
fig15a <- pp_check(finalModel, type = "stat_grouped", stat = 'min', group = "Responder", resp = 'Scale1') # Keep (use for priors and posteriors)
fig15b <- pp_check(finalModel, type = "stat_grouped", stat = 'min', group = "Responder", resp = 'Scale2') # Keep (use for priors and posteriors)
fig15c <- pp_check(finalModel, type = "stat_grouped", stat = 'min', group = "Responder", resp = 'Scale3') # Keep (use for priors and posteriors)
fig15d <- pp_check(finalModel, type = "stat_grouped", stat = 'min', group = "Responder", resp = 'Scale4') # Keep (use for priors and posteriors)
fig15e <- pp_check(finalModel, type = "stat_grouped", stat = 'min', group = "Responder", resp = 'Scale5') # Keep (use for priors and posteriors)
fig15f <- pp_check(finalModel, type = "stat_grouped", stat = 'min', group = "Responder", resp = 'Scale6') # Keep (use for priors and posteriors)
fig15g <- pp_check(finalModel, type = "stat_grouped", stat = 'min', group = "Responder", resp = 'Scale7') # Keep (use for priors and posteriors)
fig15h <- pp_check(finalModel, type = "stat_grouped", stat = 'min', group = "Responder", resp = 'Scale8') # Keep (use for priors and posteriors)
fig15i <- pp_check(finalModel, type = "stat_grouped", stat = 'min', group = "Responder", resp = 'Scale9') # Keep (use for priors and posteriors)
fig15j <- pp_check(finalModel, type = "stat_grouped", stat = 'min', group = "Responder", resp = 'Scale10') # Keep (use for priors and posteriors)

fig16a <- pp_check(finalModel, type = "stat_grouped", stat = 'mean', group = "Responder", resp = 'Scale1') # Keep (use for priors and posteriors)
fig16b <- pp_check(finalModel, type = "stat_grouped", stat = 'mean', group = "Responder", resp = 'Scale2') # Keep (use for priors and posteriors)
fig16c <- pp_check(finalModel, type = "stat_grouped", stat = 'mean', group = "Responder", resp = 'Scale3') # Keep (use for priors and posteriors)
fig16d <- pp_check(finalModel, type = "stat_grouped", stat = 'mean', group = "Responder", resp = 'Scale4') # Keep (use for priors and posteriors)
fig16e <- pp_check(finalModel, type = "stat_grouped", stat = 'mean', group = "Responder", resp = 'Scale5') # Keep (use for priors and posteriors)
fig16f <- pp_check(finalModel, type = "stat_grouped", stat = 'mean', group = "Responder", resp = 'Scale6') # Keep (use for priors and posteriors)
fig16g <- pp_check(finalModel, type = "stat_grouped", stat = 'mean', group = "Responder", resp = 'Scale7') # Keep (use for priors and posteriors)
fig16h <- pp_check(finalModel, type = "stat_grouped", stat = 'mean', group = "Responder", resp = 'Scale8') # Keep (use for priors and posteriors)
fig16i <- pp_check(finalModel, type = "stat_grouped", stat = 'mean', group = "Responder", resp = 'Scale9') # Keep (use for priors and posteriors)
fig16j <- pp_check(finalModel, type = "stat_grouped", stat = 'mean', group = "Responder", resp = 'Scale10') # Keep (use for priors and posteriors)

fig17a <- pp_check(finalModel, type = "stat_grouped", stat = 'max', group = "Responder", resp = 'Scale1') # Keep (use for priors and posteriors)
fig17b <- pp_check(finalModel, type = "stat_grouped", stat = 'max', group = "Responder", resp = 'Scale2') # Keep (use for priors and posteriors)
fig17c <- pp_check(finalModel, type = "stat_grouped", stat = 'max', group = "Responder", resp = 'Scale3') # Keep (use for priors and posteriors)
fig17d <- pp_check(finalModel, type = "stat_grouped", stat = 'max', group = "Responder", resp = 'Scale4') # Keep (use for priors and posteriors)
fig17e <- pp_check(finalModel, type = "stat_grouped", stat = 'max', group = "Responder", resp = 'Scale5') # Keep (use for priors and posteriors)
fig17f <- pp_check(finalModel, type = "stat_grouped", stat = 'max', group = "Responder", resp = 'Scale6') # Keep (use for priors and posteriors)
fig17g <- pp_check(finalModel, type = "stat_grouped", stat = 'max', group = "Responder", resp = 'Scale7') # Keep (use for priors and posteriors)
fig17h <- pp_check(finalModel, type = "stat_grouped", stat = 'max', group = "Responder", resp = 'Scale8') # Keep (use for priors and posteriors)
fig17i <- pp_check(finalModel, type = "stat_grouped", stat = 'max', group = "Responder", resp = 'Scale9') # Keep (use for priors and posteriors)
fig17j <- pp_check(finalModel, type = "stat_grouped", stat = 'max', group = "Responder", resp = 'Scale10') # Keep (use for priors and posteriors)

## ---- Figure Export -----------------
# Set directory
setwd('')
# Save figures
ggsave("MSE_finalModel_Rhat.jpeg", plot = fig1, width = 10, height = 10)
ggsave("MSE_finalModel_Neff.jpeg", plot = fig2, width = 10, height = 10)

ggsave("MSE_finalModel_AutoCorr_Intercept.jpeg", plot = fig3a, width = 10, height = 10)
ggsave("MSE_finalModel_AutoCorr_Scale 1.jpeg", plot = fig3b, width = 10, height = 10)
ggsave("MSE_finalModel_AutoCorr_Scale 2.jpeg", plot = fig3c, width = 10, height = 10)
ggsave("MSE_finalModel_AutoCorr_Scale 3.jpeg", plot = fig3d, width = 10, height = 10)
ggsave("MSE_finalModel_AutoCorr_Scale 4.jpeg", plot = fig3e, width = 10, height = 10)
ggsave("MSE_finalModel_AutoCorr_Scale 5.jpeg", plot = fig3f, width = 10, height = 10)
ggsave("MSE_finalModel_AutoCorr_Scale 6.jpeg", plot = fig3g, width = 10, height = 10)
ggsave("MSE_finalModel_AutoCorr_Scale 7.jpeg", plot = fig3h, width = 10, height = 10)
ggsave("MSE_finalModel_AutoCorr_Scale 8.jpeg", plot = fig3i, width = 10, height = 10)
ggsave("MSE_finalModel_AutoCorr_Scale 9.jpeg", plot = fig3j, width = 10, height = 10)
ggsave("MSE_finalModel_AutoCorr_Scale 10.jpeg", plot = fig3k, width = 10, height = 10)

ggsave("MSE_finalModel_Trace_Intercept.jpeg", plot = fig4a, width = 10, height = 10)
ggsave("MSE_finalModel_Trace_Scale 1.jpeg", plot = fig4b, width = 10, height = 10)
ggsave("MSE_finalModel_Trace_Scale 2.jpeg", plot = fig4c, width = 10, height = 10)
ggsave("MSE_finalModel_Trace_Scale 3.jpeg", plot = fig4d, width = 10, height = 10)
ggsave("MSE_finalModel_Trace_Scale 4.jpeg", plot = fig4e, width = 10, height = 10)
ggsave("MSE_finalModel_Trace_Scale 5.jpeg", plot = fig4f, width = 10, height = 10)
ggsave("MSE_finalModel_Trace_Scale 6.jpeg", plot = fig4g, width = 10, height = 10)
ggsave("MSE_finalModel_Trace_Scale 7.jpeg", plot = fig4h, width = 10, height = 10)
ggsave("MSE_finalModel_Trace_Scale 8.jpeg", plot = fig4i, width = 10, height = 10)
ggsave("MSE_finalModel_Trace_Scale 9.jpeg", plot = fig4j, width = 10, height = 10)
ggsave("MSE_finalModel_Trace_Scale 10.jpeg", plot = fig4k, width = 10, height = 10)

ggsave("MSE_finalModel_stat_2d_Scale1.jpeg", plot = fig5a, width = 10, height = 10)
ggsave("MSE_finalModel_stat_2d_Scale2.jpeg", plot = fig5b, width = 10, height = 10)
ggsave("MSE_finalModel_stat_2d_Scale3.jpeg", plot = fig5c, width = 10, height = 10)
ggsave("MSE_finalModel_stat_2d_Scale4.jpeg", plot = fig5d, width = 10, height = 10)
ggsave("MSE_finalModel_stat_2d_Scale5.jpeg", plot = fig5e, width = 10, height = 10)
ggsave("MSE_finalModel_stat_2d_Scale6.jpeg", plot = fig5f, width = 10, height = 10)
ggsave("MSE_finalModel_stat_2d_Scale7.jpeg", plot = fig5g, width = 10, height = 10)
ggsave("MSE_finalModel_stat_2d_Scale8.jpeg", plot = fig5h, width = 10, height = 10)
ggsave("MSE_finalModel_stat_2d_Scale9.jpeg", plot = fig5i, width = 10, height = 10)
ggsave("MSE_finalModel_stat_2d_Scale10.jpeg", plot = fig5j, width = 10, height = 10)

ggsave("MSE_finalModel_ObsVsPred_Scale1.jpeg", plot = fig6a, width = 10, height = 10)
ggsave("MSE_finalModel_ObsVsPred_Scale2.jpeg", plot = fig6b, width = 10, height = 10)
ggsave("MSE_finalModel_ObsVsPred_Scale3.jpeg", plot = fig6c, width = 10, height = 10)
ggsave("MSE_finalModel_ObsVsPred_Scale4.jpeg", plot = fig6d, width = 10, height = 10)
ggsave("MSE_finalModel_ObsVsPred_Scale5.jpeg", plot = fig6e, width = 10, height = 10)
ggsave("MSE_finalModel_ObsVsPred_Scale6.jpeg", plot = fig6f, width = 10, height = 10)
ggsave("MSE_finalModel_ObsVsPred_Scale7.jpeg", plot = fig6g, width = 10, height = 10)
ggsave("MSE_finalModel_ObsVsPred_Scale8.jpeg", plot = fig6h, width = 10, height = 10)
ggsave("MSE_finalModel_ObsVsPred_Scale9.jpeg", plot = fig6i, width = 10, height = 10)
ggsave("MSE_finalModel_ObsVsPred_Scale10.jpeg", plot = fig6j, width = 10, height = 10)

ggsave("MSE_finalModel_ErrorDistribution_Scale1.jpeg", plot = fig7a, width = 10, height = 10)
ggsave("MSE_finalModel_ErrorDistribution_Scale2.jpeg", plot = fig7b, width = 10, height = 10)
ggsave("MSE_finalModel_ErrorDistribution_Scale3.jpeg", plot = fig7c, width = 10, height = 10)
ggsave("MSE_finalModel_ErrorDistribution_Scale4.jpeg", plot = fig7d, width = 10, height = 10)
ggsave("MSE_finalModel_ErrorDistribution_Scale5.jpeg", plot = fig7e, width = 10, height = 10)
ggsave("MSE_finalModel_ErrorDistribution_Scale6.jpeg", plot = fig7f, width = 10, height = 10)
ggsave("MSE_finalModel_ErrorDistribution_Scale7.jpeg", plot = fig7g, width = 10, height = 10)
ggsave("MSE_finalModel_ErrorDistribution_Scale8.jpeg", plot = fig7h, width = 10, height = 10)
ggsave("MSE_finalModel_ErrorDistribution_Scale9.jpeg", plot = fig7i, width = 10, height = 10)
ggsave("MSE_finalModel_ErrorDistribution_Scale10.jpeg", plot = fig7j, width = 10, height = 10)

ggsave("MSE_finalModel_DensOverlay_Scale1.jpeg", plot = fig8a, width = 10, height = 10)
ggsave("MSE_finalModel_DensOverlay_Scale2.jpeg", plot = fig8b, width = 10, height = 10)
ggsave("MSE_finalModel_DensOverlay_Scale3.jpeg", plot = fig8c, width = 10, height = 10)
ggsave("MSE_finalModel_DensOverlay_Scale4.jpeg", plot = fig8d, width = 10, height = 10)
ggsave("MSE_finalModel_DensOverlay_Scale5.jpeg", plot = fig8e, width = 10, height = 10)
ggsave("MSE_finalModel_DensOverlay_Scale6.jpeg", plot = fig8f, width = 10, height = 10)
ggsave("MSE_finalModel_DensOverlay_Scale7.jpeg", plot = fig8g, width = 10, height = 10)
ggsave("MSE_finalModel_DensOverlay_Scale8.jpeg", plot = fig8h, width = 10, height = 10)
ggsave("MSE_finalModel_DensOverlay_Scale9.jpeg", plot = fig8i, width = 10, height = 10)
ggsave("MSE_finalModel_DensOverlay_Scale10.jpeg", plot = fig8j, width = 10, height = 10)

ggsave("MSE_finalModel_Timepoint_min_Scale1.jpeg", plot = fig9a, width = 10, height = 10)
ggsave("MSE_finalModel_Timepoint_min_Scale2.jpeg", plot = fig9b, width = 10, height = 10)
ggsave("MSE_finalModel_Timepoint_min_Scale3.jpeg", plot = fig9c, width = 10, height = 10)
ggsave("MSE_finalModel_Timepoint_min_Scale4.jpeg", plot = fig9d, width = 10, height = 10)
ggsave("MSE_finalModel_Timepoint_min_Scale5.jpeg", plot = fig9e, width = 10, height = 10)
ggsave("MSE_finalModel_Timepoint_min_Scale6.jpeg", plot = fig9f, width = 10, height = 10)
ggsave("MSE_finalModel_Timepoint_min_Scale7.jpeg", plot = fig9g, width = 10, height = 10)
ggsave("MSE_finalModel_Timepoint_min_Scale8.jpeg", plot = fig9h, width = 10, height = 10)
ggsave("MSE_finalModel_Timepoint_min_Scale9.jpeg", plot = fig9i, width = 10, height = 10)
ggsave("MSE_finalModel_Timepoint_min_Scale10.jpeg", plot = fig9j, width = 10, height = 10)

ggsave("MSE_finalModel_Timepoint_mean_Scale1.jpeg", plot = fig10a, width = 10, height = 10)
ggsave("MSE_finalModel_Timepoint_mean_Scale2.jpeg", plot = fig10b, width = 10, height = 10)
ggsave("MSE_finalModel_Timepoint_mean_Scale3.jpeg", plot = fig10c, width = 10, height = 10)
ggsave("MSE_finalModel_Timepoint_mean_Scale4.jpeg", plot = fig10d, width = 10, height = 10)
ggsave("MSE_finalModel_Timepoint_mean_Scale5.jpeg", plot = fig10e, width = 10, height = 10)
ggsave("MSE_finalModel_Timepoint_mean_Scale6.jpeg", plot = fig10f, width = 10, height = 10)
ggsave("MSE_finalModel_Timepoint_mean_Scale7.jpeg", plot = fig10g, width = 10, height = 10)
ggsave("MSE_finalModel_Timepoint_mean_Scale8.jpeg", plot = fig10h, width = 10, height = 10)
ggsave("MSE_finalModel_Timepoint_mean_Scale9.jpeg", plot = fig10i, width = 10, height = 10)
ggsave("MSE_finalModel_Timepoint_mean_Scale10.jpeg", plot = fig10j, width = 10, height = 10)

ggsave("MSE_finalModel_Timepoint_max_Scale1.jpeg", plot = fig11a, width = 10, height = 10)
ggsave("MSE_finalModel_Timepoint_max_Scale2.jpeg", plot = fig11b, width = 10, height = 10)
ggsave("MSE_finalModel_Timepoint_max_Scale3.jpeg", plot = fig11c, width = 10, height = 10)
ggsave("MSE_finalModel_Timepoint_max_Scale4.jpeg", plot = fig11d, width = 10, height = 10)
ggsave("MSE_finalModel_Timepoint_max_Scale5.jpeg", plot = fig11e, width = 10, height = 10)
ggsave("MSE_finalModel_Timepoint_max_Scale6.jpeg", plot = fig11f, width = 10, height = 10)
ggsave("MSE_finalModel_Timepoint_max_Scale7.jpeg", plot = fig11g, width = 10, height = 10)
ggsave("MSE_finalModel_Timepoint_max_Scale8.jpeg", plot = fig11h, width = 10, height = 10)
ggsave("MSE_finalModel_Timepoint_max_Scale9.jpeg", plot = fig11i, width = 10, height = 10)
ggsave("MSE_finalModel_Timepoint_max_Scale10.jpeg", plot = fig11j, width = 10, height = 10)

ggsave("MSE_finalModel_Task_min_Scale1.jpeg", plot = fig12a, width = 10, height = 10)
ggsave("MSE_finalModel_Task_min_Scale2.jpeg", plot = fig12b, width = 10, height = 10)
ggsave("MSE_finalModel_Task_min_Scale3.jpeg", plot = fig12c, width = 10, height = 10)
ggsave("MSE_finalModel_Task_min_Scale4.jpeg", plot = fig12d, width = 10, height = 10)
ggsave("MSE_finalModel_Task_min_Scale5.jpeg", plot = fig12e, width = 10, height = 10)
ggsave("MSE_finalModel_Task_min_Scale6.jpeg", plot = fig12f, width = 10, height = 10)
ggsave("MSE_finalModel_Task_min_Scale7.jpeg", plot = fig12g, width = 10, height = 10)
ggsave("MSE_finalModel_Task_min_Scale8.jpeg", plot = fig12h, width = 10, height = 10)
ggsave("MSE_finalModel_Task_min_Scale9.jpeg", plot = fig12i, width = 10, height = 10)
ggsave("MSE_finalModel_Task_min_Scale10.jpeg", plot = fig12j, width = 10, height = 10)

ggsave("MSE_finalModel_Task_mean_Scale1.jpeg", plot = fig13a, width = 10, height = 10)
ggsave("MSE_finalModel_Task_mean_Scale2.jpeg", plot = fig13b, width = 10, height = 10)
ggsave("MSE_finalModel_Task_mean_Scale3.jpeg", plot = fig13c, width = 10, height = 10)
ggsave("MSE_finalModel_Task_mean_Scale4.jpeg", plot = fig13d, width = 10, height = 10)
ggsave("MSE_finalModel_Task_mean_Scale5.jpeg", plot = fig13e, width = 10, height = 10)
ggsave("MSE_finalModel_Task_mean_Scale6.jpeg", plot = fig13f, width = 10, height = 10)
ggsave("MSE_finalModel_Task_mean_Scale7.jpeg", plot = fig13g, width = 10, height = 10)
ggsave("MSE_finalModel_Task_mean_Scale8.jpeg", plot = fig13h, width = 10, height = 10)
ggsave("MSE_finalModel_Task_mean_Scale9.jpeg", plot = fig13i, width = 10, height = 10)
ggsave("MSE_finalModel_Task_mean_Scale10.jpeg", plot = fig13j, width = 10, height = 10)

ggsave("MSE_finalModel_Task_max_Scale1.jpeg", plot = fig14a, width = 10, height = 10)
ggsave("MSE_finalModel_Task_max_Scale2.jpeg", plot = fig14b, width = 10, height = 10)
ggsave("MSE_finalModel_Task_max_Scale3.jpeg", plot = fig14c, width = 10, height = 10)
ggsave("MSE_finalModel_Task_max_Scale4.jpeg", plot = fig14d, width = 10, height = 10)
ggsave("MSE_finalModel_Task_max_Scale5.jpeg", plot = fig14e, width = 10, height = 10)
ggsave("MSE_finalModel_Task_max_Scale6.jpeg", plot = fig14f, width = 10, height = 10)
ggsave("MSE_finalModel_Task_max_Scale7.jpeg", plot = fig14g, width = 10, height = 10)
ggsave("MSE_finalModel_Task_max_Scale8.jpeg", plot = fig14h, width = 10, height = 10)
ggsave("MSE_finalModel_Task_max_Scale9.jpeg", plot = fig14i, width = 10, height = 10)
ggsave("MSE_finalModel_Task_max_Scale10.jpeg", plot = fig14j, width = 10, height = 10)

ggsave("MSE_finalModel_Responder_min_Scale1.jpeg", plot = fig15a, width = 10, height = 10)
ggsave("MSE_finalModel_Responder_min_Scale2.jpeg", plot = fig15b, width = 10, height = 10)
ggsave("MSE_finalModel_Responder_min_Scale3.jpeg", plot = fig15c, width = 10, height = 10)
ggsave("MSE_finalModel_Responder_min_Scale4.jpeg", plot = fig15d, width = 10, height = 10)
ggsave("MSE_finalModel_Responder_min_Scale5.jpeg", plot = fig15e, width = 10, height = 10)
ggsave("MSE_finalModel_Responder_min_Scale6.jpeg", plot = fig15f, width = 10, height = 10)
ggsave("MSE_finalModel_Responder_min_Scale7.jpeg", plot = fig15g, width = 10, height = 10)
ggsave("MSE_finalModel_Responder_min_Scale8.jpeg", plot = fig15h, width = 10, height = 10)
ggsave("MSE_finalModel_Responder_min_Scale9.jpeg", plot = fig15i, width = 10, height = 10)
ggsave("MSE_finalModel_Responder_min_Scale10.jpeg", plot = fig15j, width = 10, height = 10)

ggsave("MSE_finalModel_Responder_mean_Scale1.jpeg", plot = fig16a, width = 10, height = 10)
ggsave("MSE_finalModel_Responder_mean_Scale2.jpeg", plot = fig16b, width = 10, height = 10)
ggsave("MSE_finalModel_Responder_mean_Scale3.jpeg", plot = fig16c, width = 10, height = 10)
ggsave("MSE_finalModel_Responder_mean_Scale4.jpeg", plot = fig16d, width = 10, height = 10)
ggsave("MSE_finalModel_Responder_mean_Scale5.jpeg", plot = fig16e, width = 10, height = 10)
ggsave("MSE_finalModel_Responder_mean_Scale6.jpeg", plot = fig16f, width = 10, height = 10)
ggsave("MSE_finalModel_Responder_mean_Scale7.jpeg", plot = fig16g, width = 10, height = 10)
ggsave("MSE_finalModel_Responder_mean_Scale8.jpeg", plot = fig16h, width = 10, height = 10)
ggsave("MSE_finalModel_Responder_mean_Scale9.jpeg", plot = fig16i, width = 10, height = 10)
ggsave("MSE_finalModel_Responder_mean_Scale10.jpeg", plot = fig16j, width = 10, height = 10)

ggsave("MSE_finalModel_Responder_max_Scale1.jpeg", plot = fig17a, width = 10, height = 10)
ggsave("MSE_finalModel_Responder_max_Scale2.jpeg", plot = fig17b, width = 10, height = 10)
ggsave("MSE_finalModel_Responder_max_Scale3.jpeg", plot = fig17c, width = 10, height = 10)
ggsave("MSE_finalModel_Responder_max_Scale4.jpeg", plot = fig17d, width = 10, height = 10)
ggsave("MSE_finalModel_Responder_max_Scale5.jpeg", plot = fig17e, width = 10, height = 10)
ggsave("MSE_finalModel_Responder_max_Scale6.jpeg", plot = fig17f, width = 10, height = 10)
ggsave("MSE_finalModel_Responder_max_Scale7.jpeg", plot = fig17g, width = 10, height = 10)
ggsave("MSE_finalModel_Responder_max_Scale8.jpeg", plot = fig17h, width = 10, height = 10)
ggsave("MSE_finalModel_Responder_max_Scale9.jpeg", plot = fig17i, width = 10, height = 10)
ggsave("MSE_finalModel_Responder_max_Scale10.jpeg", plot = fig17j, width = 10, height = 10)