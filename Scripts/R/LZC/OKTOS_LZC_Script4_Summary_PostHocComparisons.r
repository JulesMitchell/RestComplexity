### SUMMARRY AND POST HOC CONTRASTS###
## ----pkgload, include = FALSE-------------------------------------------------
library(ggplot2)
library(brms)
library(dplyr)
library(tidyverse) # needed for data manipulation
library(tidybayes) # needed for data manipulation
library(emmeans)
library(bayestestR)
library(knitr)
library(kableExtra)

## ---- Set working directory and excel file for data export -------------------------------------------------
setwd('')
outputFile <- "ModelOutputs.xlsx"

## ---- Load model object and save rhat and neff values -----------------
# 1. To load
finalModel <- readRDS("LZC_finalModel.rda.rds") # adjust filename as required

## ----- Summary of effect estimates --------------
effects <- describe_posterior(finalModel, test=c('pd', 'p_map')) %>%
  	as.data.frame() %>%
      mutate(across(is.numeric, round, digits=4))

# For the frequentists
pvalues = joint_tests(finalModel) %>%
    as.data.frame() %>% 
      mutate(across(is.numeric, round, digits=4))

# Main Effects 
# Task (i.e. across time and responder): 
marginalmeans_task <- finalModel %>% 
  emmeans( ~ Task) %>%
    as.data.frame() %>% 
      mutate(across(is.numeric, round, digits=4))

# Timepoint (across task, and responder)
marginalmeans_timepoint <- finalModel %>% 
  emmeans( ~ Timepoint) %>%
    as.data.frame() %>% 
      mutate(across(is.numeric, round, digits=4))

# Responder (across task, time, and scale)
marginalmeans_responder <- finalModel %>% 
  emmeans( ~ Responder) %>%
    as.data.frame() %>% 
      mutate(across(is.numeric, round, digits=4))

# Interactions (First, visualise the nature of interactions)
# Shows changes with time across participants
fig1 <- emmip(finalModel, Responder ~ Timepoint | Task, CIs = TRUE, col = "black",
  linearg = list(), facetlab = 'label_value', xlab = 'Timepoint', ylab = "LZC", tlab = ('Response Status'), dotarg = list(size = 2), CIarg = list(alpha = 0.5)) +
  theme_bw()

# Shows changes with time broken down by task/response status
fig2 <- emmip(finalModel, ~ Timepoint | Responder*Task, CIs = TRUE, col = "black",
  linearg = list(), facetlab = 'label_value', xlab = 'Timepoint', ylab = "LZC", tlab = ('Response Status'), dotarg = list(size = 2), CIarg = list(alpha = 0.5)) +
  theme_bw()

# Shows task differences are consistent across time/response status
fig3 <- emmip(finalModel, ~ Task | Responder*Timepoint, CIs = TRUE, col = "black",
  linearg = list(linetype = 'blank'), facetlab = 'label_value', xlab = 'Task', ylab = "LZC", tlab = ('Response Status'), dotarg = list(size = 2), CIarg = list(alpha = 0.5)) +
  theme_bw()

# Shows response status differences are different across task/time
fig4 <- emmip(finalModel, ~ Responder | Task*Timepoint, CIs = TRUE, col = "black",
  linearg = list(linetype = 'blank'), facetlab = 'label_value', xlab = 'Response Status', ylab = "LZC", dotarg = list(size = 2), CIarg = list(alpha = 0.5)) +
  theme_bw()

## ## ----- Post-Hoc Comparisons (Marginal Means)--------------
# We are interested responder differences at each timepoint (Saved to marginal means for export)
marginalmeans_interaction = finalModel %>% # Keep (Shows responder comparisons)
  emmeans(specs = ~ Responder, by = c('Timepoint','Task'))  %>%
    as.data.frame() %>% 
      mutate(across(is.numeric, round, digits=4))

## ----- Combine dataframes --------------
marginalmeans <- bind_rows(marginalmeans_task, marginalmeans_timepoint, marginalmeans_responder, marginalmeans_interaction)

## ----- Data Export --------------
# Export tables
write.xlsx(effects, outputFile, sheetName="LZC_EffectEstimates",  append=TRUE)
write.xlsx(pvalues, outputFile, sheetName="LZC_Pvalues",  append=TRUE)
write.xlsx(marginalmeans, outputFile, sheetName="LZC_MarginalMeans",  append=TRUE)

# Save figures
setwd('')
ggsave("LZC_Interaction1.jpeg", plot = fig1, width = 10, height = 10)
ggsave("LZC_Interaction2.jpeg", plot = fig2, width = 10, height = 10)
ggsave("LZC_Interaction3.jpeg", plot = fig3, width = 10, height = 10)
ggsave("LZC_Interaction4.jpeg", plot = fig4, width = 10, height = 10)