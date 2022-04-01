---
title: "Replication of the data analysis in Study1 by Troy et al. (2017) with an Iranian sample."
author: "Alimohammad Soufizadeh"
date: "March, 2022"
---

pacman::p_load(pacman, tidyverse, kableExtra, psych, 
               janitor, car, performance, see,
               gridExtra, interactions, devtools,
               rmarkdown, knitr, patchwork, readxl, papaja, report)

# read all data
raw_data <- read_excel("Coded_Combined.xlsx") #save the excel file as a dataframe named "all_data"

# Change column names
raw_data <- rename(raw_data, "duration" = "Duration(Second)")

# 1. drop all NA
clean_data <- drop_na(raw_data)

# Check for duplicates
sum(duplicated(clean_data)) # No duplicates

# Data summary
summary(clean_data)

# Rename columns
clean_data <- rename(clean_data, "gender" = "Sex")

# Check demographics
clean_data$gender <- factor(clean_data$gender, levels=c(1,2,3,4))
summary (clean_data$gender)


#Score questionnaires:

#save variables in separate data frame
scored_data <- clean_data %>%
  
  #row mean of the selected columns
  mutate(across(c("PSS2", "PSS3"), ~{6 - .}), # recode PSS items 2 & 3 (6 - response)
         across(c("BSM1", "BSM3", "BSM7", "BSM8"), ~{8 - .}), # first recode BSM items 1, 3, 7, 8 (8 - response)
         mean_cra = rowMeans(select(., starts_with("CRA"))),
         mean_hcru = rowMeans(select(., starts_with("HCRU"))),
         mean_pss = rowMeans(select(., starts_with("PSS"))),
         
          #for cesd, we need the sum
         sum_cesd = rowSums(select(., starts_with("CES_D"))))%>%

  # select the scores/final variables used, remove the raw items
  select(id, Age, SES, gender, mean_cra, mean_hcru,
         sum_cesd, mean_pss)

# Descriptives

# In the first step, the data are summarized to get the descriptive statistics.
#Subsequently, the data are reformatted.
descriptives <- scored_data %>% 
  dplyr::summarize(across(c(SES, Age, mean_cra, mean_hcru,sum_cesd, 
                            mean_pss),
                   list(mean = mean, sd = sd, min = min, max = max))) %>%
  
  # bring everything in long format
  pivot_longer(everything(), names_to = "name") %>%
  
  # separate names at last underscore
  separate(name, into = c("name","descriptive"), sep = "_(?=[^_]+$)") %>%
  
  # get into a bit wider format again
  pivot_wider(names_from = name, values_from = value) %>%
  
  # rename to have nicer column names
  rename(Summary = descriptive,
         CRA = mean_cra,  #
         HCRU = mean_hcru, #
         PSS = mean_pss,  #
         CESD = sum_cesd) # 

# Calculate cronbach’s alphas
# Select the items from the raw data that belong to the specific scale.
# calculate alpha and extract raw_alpha from the list the alpha function generates.
alpha <- clean_data %>%
  dplyr::summarize(
            # Replication Block Alphas
            cra_alpha = select(.,starts_with("CRA")) %>% psych::alpha() %>%
              pluck("total", "raw_alpha"), # extract total and then raw_alpha from list
            hcru_alpha = select(.,starts_with("HCRU")) %>% psych::alpha() %>%
              pluck("total", "raw_alpha"),
            cesd_alpha = select(.,starts_with("CES_D")) %>% psych::alpha() %>%
              pluck("total", "raw_alpha"),
            pss_alpha = select(.,starts_with("PSS")) %>% psych::alpha(check.keys=TRUE) %>%
              pluck("total", "raw_alpha"))

# add alphas as extra row to the descriptives table
descriptives <- descriptives %>%
  add_row(Summary = "alpha", SES = NA, CRA = alpha$cra_alpha, HCRU = alpha$hcru_alpha,
          PSS = alpha$pss_alpha, CESD = alpha$cesd_alpha)

# make it a nicely formatted table
library(rmarkdown)
apa_table(descriptives) # is only shown when RMarkdown document is knitted

# Plot monthly family income
income_plot<-hist(scored_data$SES,
                  main="Family income distribution",
                  xlab="family income category")


# Mean centre all IVs for regressions with interaction terms
centred_data <- scored_data %>%
  mutate(CRA_c = scale(mean_cra, center = TRUE, scale = FALSE),
         SES_c = scale(SES, center = TRUE, scale = FALSE),
         PSS_c = scale(mean_pss, center = TRUE, scale = FALSE),
         HCRU_c = scale(mean_hcru, center = TRUE, scale = FALSE))

# main regression
# CRA and SES individually and the interaction between both
fit <- lm(sum_cesd ~ PSS_c + CRA_c + SES_c + CRA_c:SES_c, data = centred_data)
# include our covariates individually:
#HCRU
fit_2 <- lm(sum_cesd ~ PSS_c + CRA_c + SES_c + CRA_c:SES_c + HCRU_c, data = centred_data)
#age
fit_3 <- lm(sum_cesd ~ PSS_c + CRA_c + SES_c + CRA_c:SES_c + Age, data = centred_data)
#gender
fit_4 <- lm(sum_cesd ~ PSS_c + CRA_c + SES_c + CRA_c:SES_c + gender, data = centred_data)

# Troy et al. (2017) also modeled one regression with race, however, 
# the current study collected data from Iran without asking question about race. 
# This is discussed in detail in the paper.

#They also modelled one regression without controlling for life stress (PSS):
fit_5 <- lm(sum_cesd ~ CRA_c + SES_c + CRA_c:SES_c, data = centred_data)

check_model(fit)

summary(fit)

interaction_plot <- interactions::interact_plot(model = fit,
                                                pred = CRA_c,
                                                modx = SES_c,
                                                interval=TRUE,
                                                x.label = "Cognitive Reappraisal Ability",
                                                y.label= "Depressive Symptoms",
                                                legend.main = "family income")

# CLEAN UP #################################################

# Clear environment
rm(list = ls()) 

# Clear packages
p_unload(all)

# Clear console
cat("\014")
