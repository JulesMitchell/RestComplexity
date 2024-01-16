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
finalModel <- readRDS("MSE_MVfinalModel.rds") # adjust filename as required

## ----- Summary of effect estimates --------------
effects <- describe_posterior(finalModel, test=c('pd', 'p_map')) %>% # add 'equitest' for ROPE test
  	as.data.frame() %>% 
      mutate(across(is.numeric, round, digits=4))

## For the frequentists
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
marginalmeans_responder <-finalModel %>% 
  emmeans( ~ Responder:rep.meas) %>%
    as.data.frame() %>% 
      mutate(across(is.numeric, round, digits=4))

## Interactions
fig1 <- emmip(finalModel, Responder ~ rep.meas | Task*Timepoint, CIs = TRUE, col = "black",
linearg = list(), facetlab = 'label_value', xlab = 'Scale', ylab = "Sample Entropy", tlab = ('Response Status'), dotarg = list(size = 2), CIarg = list(alpha = 0.5)) + theme(axis.text.x = element_text(angle = 90)) +
theme_bw() 
fig1 <- fig1 + theme(axis.text.x = element_text(angle = 90))

fig2 <- emmip(finalModel, Task ~ rep.meas | Responder*Timepoint, CIs = TRUE, col = "black",
  linearg = list(), facetlab = 'label_value', xlab = 'Scale', ylab = "Sample Entropy", tlab = ('Task'), dotarg = list(size = 2), CIarg = list(alpha = 0.5)) +
  theme_bw()
fig2 <- fig2 + theme(axis.text.x = element_text(angle = 90))

fig3 <- emmip(finalModel, rep.meas ~ Timepoint | Task*Responder, CIs = TRUE, col = "black",
  linearg = list(), facetlab = 'label_value', xlab = 'Timepoint', ylab = "Sample Entropy", tlab = ('Scale'), dotarg = list(size = 2), CIarg = list(alpha = 0.5)) +
  theme_bw() 

fig4 <- emmip(finalModel, rep.meas ~ Task | Timepoint , CIs = TRUE, col = "black",
  linearg = list(), facetlab = 'label_value', xlab = 'Task', ylab = "Sample Entropy", tlab = ('Scale'), dotarg = list(size = 2), CIarg = list(alpha = 0.5)) +
  theme_bw() 

# We are interested responder differences at each timepoint
marginalmeans_interaction <- finalModel %>% 
  emmeans(specs = ~ rep.meas:Responder, by = c('Timepoint','Task'))  %>%
    as.data.frame() %>% 
      mutate(across(is.numeric, round, digits=4))

## ----- Combine dataframes --------------
marginalmeans <- bind_rows(marginalmeans_task, marginalmeans_timepoint, marginalmeans_responder, marginalmeans_interaction)

## ----- Data Export --------------
# Export tables
write.xlsx(effects, outputFile, sheetName="MSE_EffectEstimates",  append=TRUE)
write.xlsx(pvalues, outputFile, sheetName="MSE_Pvalues",  append=TRUE)
write.xlsx(marginalmeans, outputFile, sheetName="MSE_MarginalMeans",  append=TRUE)

# Save figures
setwd('')
ggsave("MSE_Interaction1.jpeg", plot = fig1, width = 10, height = 10)
ggsave("MSE_Interaction2.jpeg", plot = fig2, width = 10, height = 10)
ggsave("MSE_Interaction3.jpeg", plot = fig3, width = 10, height = 10)
ggsave("MSE_Interaction4.jpeg", plot = fig4, width = 10, height = 10)