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

# Interactions
#Figure 3. Condition-by-timepoint interaction across response status
fig1 <- emmip(finalModel, Responder ~ Task | Timepoint , CIs = TRUE, 
      linearg = list(size = 1.5, linetype = "blank"),  # Increase line width for visibility
      facetlab = 'label_value', 
      xlab = 'Condition', 
      ylab = "Lempel-Ziv Complexity",
      tlab = 'Response Status',
      dotarg = list(size = 3),  # Slightly larger dots for better visibility
      CIarg = list(lwd = 2, alpha = 0.5)) + 
    theme(text = element_text(size = 18),  # Increase overall text size
          legend.position = "bottom") + 
    scale_color_brewer(palette = "Set1")

#Figure 4. Responder-by-timepoint interaction across condition
fig2 <- emmip(finalModel, Responder ~ Timepoint | Task, CIs = TRUE, 
       linearg = list(size = 1.5),  # Increase line width for visibility
       facetlab = 'label_value', 
       xlab = 'Timepoint', 
       ylab = "Lempel-Ziv Complexity", 
       tlab = ('Response Status'), 
       dotarg = list(size = 3),  # Slightly larger dots for better visibility
       CIarg = list(lwd = 2, alpha = 0.5)) + 
      theme(text = element_text(size = 18),  # Increase overall text size
      legend.position = "bottom") + 
      scale_color_brewer(palette = "Set1")

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