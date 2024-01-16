### PRIOR PREDICTIVE CHECK ###
## ----pkgload, include = FALSE-------------------------------------------------
library(brms)

## ---- Set working directory as required -------------------------------------------------
setwd('')

## ---- Load saved model object (useful for coming back to analysis later) -----------------
# 1. To load
finalModel_prior <- readRDS("LZC_finalModel_prior.rda.rds") # adjust filename as required

## ---- Review model prior and summary(estimates and CIs) -------------------------------------------------
#. 1 Prior predictive checks
prior_summary(finalModel_prior)  # Re-specify priors and re-run model if needed
summary(finalModel_prior)
fig1 <- pp_check(finalModel_prior, type  = 'hist', ndraws = 10, prefix='ppd')

## ---- Export figures -------------------------------------------------
# Set desired directory
setwd('')

# Save figures
ggsave("LZC_finalModel_PPCheck_hist.jpeg", plot = fig1, width = 10, height = 10)