### POST HOC CONTRASTS AND HYPOTHESIS TESTING ###
## ----pkgload, include = FALSE-------------------------------------------------
library(ggplot2)
library(brms)
library(tidyverse)
library(tidybayes) 
library(emmeans)
library(bayestestR)
library(knitr)
library(kableExtra)
library(xlsx)

## ---- Set working directory as required -------------------------------------------------
setwd('')
outputFile <- "ModelOutputs.xlsx"

## ---- Load model object and save rhat and neff values -----------------
# 1. To load
finalModel <- readRDS("LZCchan_finalModel.rda.rds") # adjust filename as required

## ----- Summary of Estimates --------------
effects <- describe_posterior(finalModel, test=c('pd', 'p_map')) %>%
  	as.data.frame() %>%
      mutate(across(is.numeric, round, digits=4))

# For the frequentists
pvalues = joint_tests(finalModel) %>%
    as.data.frame() %>% 
      mutate(across(is.numeric, round, digits=4))

## ----- Post-Hoc Comparisons (Best Model only)--------------
# Task (i.e. across time and responder): 
marginalmeans_task <- finalModel %>% 
  emmeans( ~ Task:rep.meas) %>%
    as.data.frame() %>% 
      mutate(across(is.numeric, round, digits=4))

# Timepoint (across task, responder, and scale)
marginalmeans_timepoint <- finalModel %>% 
  emmeans( ~ Timepoint:rep.meas) %>%
    as.data.frame() %>% 
      mutate(across(is.numeric, round, digits=4))

# Responder (across task, time, and scale)
marginalmeans_responder <- finalModel %>% 
  emmeans( ~ Responder:rep.meas) %>%
    as.data.frame() %>% 
      mutate(across(is.numeric, round, digits=4))

## Interactions
## First, visualise the nature of interactions
# Shows changes with time across participants
fig1 <- emmip(finalModel, Responder ~ rep.meas | Task*Timepoint, CIs = TRUE, 
       linearg = list(size = 1.5, linetype='blank'),  # Increase line width for visibility
       facetlab = 'label_value', 
       xlab = 'Channel', 
       ylab = "Lempel-Ziv Complexity", 
       tlab = ('Response Status'), 
       dotarg = list(size = 3),  # Slightly larger dots for better visibility
       CIarg = list(lwd = 2, alpha = 0.5)) + 
     theme(text = element_text(size = 18),  # Increase overall text size
           legend.position = "bottom") + 
     scale_color_brewer(palette = "Set1")

## Marginal Means
marginalmeans_interaction <- finalModel %>% # Keep (Shows responder comparisons)
  emmeans(specs = ~ rep.meas:Responder, by = c('Timepoint','Task'))  %>%
    as.data.frame() %>% 
      mutate(across(is.numeric, round, digits=4))

## ----- Combine dataframes --------------
marginalmeans <- bind_rows(marginalmeans_task, marginalmeans_timepoint, marginalmeans_responder, marginalmeans_interaction)

## ----- Data Export --------------
# Export tables
write.xlsx(effects, outputFile, sheetName="LZCChan_EffectEstimates",  append=TRUE)
write.xlsx(pvalues, outputFile, sheetName="LZCChan_Pvalues",  append=TRUE)
write.xlsx(marginalmeans, outputFile, sheetName="LZCChan_MarginalMeans",  append=TRUE)

# Save figures
setwd('')
ggsave("LZCchan_Interaction1.jpeg", plot = fig1, width = 10, height = 10)