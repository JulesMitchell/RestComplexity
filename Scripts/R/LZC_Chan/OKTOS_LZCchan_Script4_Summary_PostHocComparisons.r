### POST HOC CONTRASTS AND HYPOTHESIS TESTING ###
## ----pkgload, include = FALSE-------------------------------------------------
library(ggplot2)
library(brms)
library(dplyr)
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
fig1 <- emmip(finalModel, Responder ~ rep.meas | Task*Timepoint, CIs = TRUE, col = "black",
  linearg = list(linetype = "blank"), facetlab = 'label_value', xlab = 'Channel', ylab = "LZC", tlab = ('Response Status'), dotarg = list(size = 2), CIarg = list(alpha = 0.5)) +
  theme_bw()

# Shows changes with time broken down by task/response status
fig2 <- emmip(finalModel, rep.meas ~ Timepoint | Responder*Task, CIs = TRUE, col = "black",
  linearg = list(), facetlab = 'label_value', xlab = 'Timepoint', ylab = "LZC", tlab = ('Response Status'), dotarg = list(size = 2), CIarg = list(alpha = 0.5)) +
  theme_bw()

# Shows task differences are consistent across time/response status
fig3 <- emmip(finalModel, rep.meas ~ Task | Responder*Timepoint, CIs = TRUE, col = "black",
  linearg = list(linetype = "blank"), facetlab = 'label_value', xlab = 'Task', ylab = "LZC", tlab = ('Response Status'), dotarg = list(size = 2), CIarg = list(alpha = 0.5)) +
  theme_bw()

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
ggsave("LZCchan_Interaction2.jpeg", plot = fig2, width = 10, height = 10)
ggsave("LZCchan_Interaction3.jpeg", plot = fig3, width = 10, height = 10)