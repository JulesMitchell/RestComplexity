### PRIOR PREDICTIVE CHECK ###
## ----pkgload, include = FALSE-------------------------------------------------
library(brms)
library(ggplot2)

## ---- Set working directory as required -------------------------------------------------
setwd('')

## ---- Load saved model object (useful for coming back to analysis later) -----------------
# 1. To load
LZCchan_fullModel_prior <- readRDS("LZCchan_finalModel_prior.rda.rds") # adjust filename as required

## ---- Review model prior and summary(estimates and CIs) -------------------------------------------------
#. 1 Prior predictive checks
prior_summary(LZCchan_fullModel_prior)  # Re-specify priors and re-run model if needed
summary(LZCchan_fullModel_prior)

fig1 <- pp_check(LZCchan_fullModel_prior, type  = 'hist', ndraws = 10, prefix='ppd', resp='Fp1')
fig2 <- pp_check(LZCchan_fullModel_prior, type  = 'hist', ndraws = 10, prefix='ppd', resp='AF3')
fig3 <- pp_check(LZCchan_fullModel_prior, type  = 'hist', ndraws = 10, prefix='ppd', resp='FC1')
fig4 <- pp_check(LZCchan_fullModel_prior, type  = 'hist', ndraws = 10, prefix='ppd', resp='F3')


## ---- Export figures -------------------------------------------------
# Set desired directory
setwd('')

# Save figures
ggsave("LZCchan_fullModel_PPCheck_Fp1_hist.jpeg", plot = fig1, width = 10, height = 10)
ggsave("LZCchan_fullModel_PPCheck_AF3__hist.jpeg", plot = fig2, width = 10, height = 10)
ggsave("LZCchan_fullModel_PPCheck_FC1_hist.jpeg", plot = fig3, width = 10, height = 10)
ggsave("LZCchan_fullModel_PPCheck_F3__hist.jpeg", plot = fig4, width = 10, height = 10)
