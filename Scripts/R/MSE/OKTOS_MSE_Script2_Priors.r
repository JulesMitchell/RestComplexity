### PRIOR PREDICTIVE CHECK ###
## ----pkgload, include = FALSE-------------------------------------------------
library(brms)
library(ggplot2)

## ---- Set working directory as required -------------------------------------------------
setwd('')

## ---- Load saved model object (useful for coming back to analysis later) -----------------
# 1. To load
finalModel_prior <- readRDS("MSE_MVfinalModel_prior.rda.rds") # adjust filename as required

## ---- Review model prior and summary(estimates and CIs) -------------------------------------------------
#. 1 Prior predictive checks
prior_summary(finalModel_prior)  # Re-specify priors and re-run model if needed
summary(finalModel_prior) # Check estimates produced from priors

fig1 <- pp_check(finalModel_prior, type  = 'hist', ndraws = 10, prefix='ppd', resp='Scale1')
fig2 <- pp_check(finalModel_prior, type  = 'hist', ndraws = 10, prefix='ppd', resp='Scale2')
fig3 <- pp_check(finalModel_prior, type  = 'hist', ndraws = 10, prefix='ppd', resp='Scale3')
fig4 <- pp_check(finalModel_prior, type  = 'hist', ndraws = 10, prefix='ppd', resp='Scale4')
fig5 <- pp_check(finalModel_prior, type  = 'hist', ndraws = 10, prefix='ppd', resp='Scale5')
fig6 <- pp_check(finalModel_prior, type  = 'hist', ndraws = 10, prefix='ppd', resp='Scale6')
fig7 <- pp_check(finalModel_prior, type  = 'hist', ndraws = 10, prefix='ppd', resp='Scale7')
fig8 <- pp_check(finalModel_prior, type  = 'hist', ndraws = 10, prefix='ppd', resp='Scale8')
fig9 <- pp_check(finalModel_prior, type  = 'hist', ndraws = 10, prefix='ppd', resp='Scale9')
fig10 <- pp_check(finalModel_prior, type  = 'hist', ndraws = 10, prefix='ppd', resp='Scale10')

## ---- Export figures -------------------------------------------------
# Set desired directory
setwd('')

# Save figures
ggsave("MSE_finalModel_PPCheck_Scale1_hist.jpeg", plot = fig1, width = 10, height = 10)
ggsave("MSE_finalModel_PPCheck_Scale2_hist.jpeg", plot = fig2, width = 10, height = 10)
ggsave("MSE_finalModel_PPCheck_Scale3_hist.jpeg", plot = fig3, width = 10, height = 10)
ggsave("MSE_finalModel_PPCheck_Scale4_hist.jpeg", plot = fig4, width = 10, height = 10)
ggsave("MSE_finalModel_PPCheck_Scale5_hist.jpeg", plot = fig5, width = 10, height = 10)
ggsave("MSE_finalModel_PPCheck_Scale6_hist.jpeg", plot = fig6, width = 10, height = 10)
ggsave("MSE_finalModel_PPCheck_Scale7_hist.jpeg", plot = fig7, width = 10, height = 10)
ggsave("MSE_finalModel_PPCheck_Scale8_hist.jpeg", plot = fig8, width = 10, height = 10)
ggsave("MSE_finalModel_PPCheck_Scale9_hist.jpeg", plot = fig9, width = 10, height = 10)
ggsave("MSE_finalModel_PPCheck_Scale10_hist.jpeg", plot = fig10, width = 10, height = 10)