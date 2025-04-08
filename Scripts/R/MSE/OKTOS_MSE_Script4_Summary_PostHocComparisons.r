### POST HOC CONTRASTS AND HYPOTHESIS TESTING ###
## ----pkgload, include = FALSE-------------------------------------------------
library(ggplot2)
library(brms)
library(emmeans)
library(tidyverse) 
library(tidybayes)
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

## Interactions plots
# Figure 1. condition-by-scale interaction
fig1 <- emmip(finalModel, Task ~ rep.meas | Responder*Timepoint, CIs = TRUE, 
       linearg = list(size = 1.5),  # Increase line width for visibility
       facetlab = 'label_value', 
       xlab = 'Scale', 
       ylab = "Sample Entropy", 
       tlab = ('Condition'), 
       dotarg = list(size = 3),  # Slightly larger dots for better visibility
       CIarg = list(lwd = 2, alpha = 0.5)) + 
     theme(text = element_text(size = 18),  # Increase overall text size
           legend.position = "bottom") + 
     scale_color_brewer(palette = "Set1") +
     scale_x_discrete(labels=c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"))

# Figure 2. response-by-scale interaction across timepoint and condition
fig2 <- emmip(finalModel, Responder ~ rep.meas | Task*Timepoint, CIs = TRUE, 
       linearg = list(size = 1.5),  # Increase line width for visibility
       facetlab = 'label_value', 
       xlab = 'Scale', 
       ylab = "Sample Entropy", 
       tlab = ('Response Status'), 
       dotarg = list(size = 3),  # Slightly larger dots for better visibility
       CIarg = list(lwd = 2, alpha = 0.5)) + 
     theme(text = element_text(size = 18),  # Increase overall text size
           legend.position = "bottom") + 
     scale_color_brewer(palette = "Set1") +
     scale_x_discrete(labels=c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"))

# Figure 3. Conference poster plot
fig3 <- emmip(finalModel, Task ~ rep.meas | Timepoint, CIs = TRUE, 
       linearg = list(size = 1.5),  # Increase line width for visibility
       facetlab = 'label_value', 
       xlab = 'Scale', 
       ylab = "Sample Entropy", 
       tlab = ('Condition'), 
       dotarg = list(size = 3),  # Slightly larger dots for better visibility
       CIarg = list(lwd = 2, alpha = 0.5)) + 
     theme(text = element_text(size = 18),  # Increase overall text size
           legend.position = "bottom") + 
     scale_color_brewer(palette = "Set1") +
     scale_x_discrete(labels=c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"))

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