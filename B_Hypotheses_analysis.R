
# title: "Replication of the data analysis in Study1 by Troy et al. (2017) with an Iranian sample."
# author: "Alimohammad Soufizadeh"
# date: "March, 2022"

set.seed(79)

## ----------------------------------------------------------------------------------------
# Load packages
pacman::p_load(pacman, tidyverse, kableExtra, psych, interactions,jtools,
               gridExtra,knitr, readxl, papaja, rmarkdown)


## ----------------------------------------------------------------------------------------
raw_data <- read_excel("Coded_data.xlsx")


## ----------------------------------------------------------------------------------------
# Change variable names
raw_data <- rename(raw_data, 
                   "duration" = "Duration(Second)", # in seconds
                   "gender" = "Sex")


## ----------------------------------------------------------------------------------------
# summarise raw data
summary(raw_data)


## ----------------------------------------------------------------------------------------
# 1. exclude all who didn't complete the survey or had missing data
no_missing <- drop_na(raw_data)


## ----------------------------------------------------------------------------------------
# 2. exclude participants with zero variance among their answers
no_zero_var <- no_missing[apply(no_missing[, -c(1:4)], 1, var) != 0, ]


## ----------------------------------------------------------------------------------------
# 3. exclude participants who took less than 3 minutes (180 seconds) to complete the survey
clean_data <- no_zero_var %>% filter(duration >= 180)


## ----------------------------------------------------------------------------------------
# Check for duplicates
sum(duplicated(clean_data))


## ----------------------------------------------------------------------------------------
summary(clean_data)


## ----Function to score data--------------------------------------------------------------
# Function to Score data
function_score_data <-function(clean_data){

#save variables in separate data frame
scored_data <- clean_data %>%
  
  #row mean of the selected columns
  mutate(across(c("PSS2", "PSS3"), ~{6 - .}), # recode PSS items 2 & 3 (6 - response)
         across(c("BSM1", "BSM3", "BSM7", "BSM8"), ~{8 - .}), # first recode BSM items 1, 3, 7, 8 (8 - response)
         mean_cra = rowMeans(select(., starts_with("CRA"))),
         mean_hcru = rowMeans(select(., starts_with("HCRU"))),
         mean_pss = rowMeans(select(., starts_with("PSS"))),
         
         mean_bsm = rowMeans(select(., starts_with("BSM"))),
         #for cesd, we need the sum
         sum_cesd = rowSums(select(., starts_with("CES_D"))))%>%
return(scored_data)
}


## ----Score data--------------------------------------------------------------------------
# Score the data with no outliers
scored_data <- function_score_data(clean_data) #score data first, to find outliers among scored data


## ----------------------------------------------------------------------------------------
# Missing Data & outliers checked
summary(scored_data)


## ----------------------------------------------------------------------------------------
# Check demographics
# 1 = male, 2 = female, 3 = non-binary, 4 = don't want to answer
scored_data$gender <- factor(scored_data$gender, levels=c(1,2,3,4)) # Factor gender
summary(scored_data$gender)


## ----------------------------------------------------------------------------------------
descriptives <- scored_data %>% 
  dplyr::summarize(across(c(SES, Age, mean_cra, mean_hcru,sum_cesd, 
                            mean_pss, mean_bsm),
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
         CESD = sum_cesd, #
         BSM = mean_bsm) #


## ----------------------------------------------------------------------------------------
# Calculate cronbach’s alphas
alpha <- scored_data %>%
  dplyr::summarize(
    # Replication Block Alphas
    cra_alpha = select(.,starts_with("CRA")) %>% psych::alpha() %>%
      pluck("total", "raw_alpha"), # extract total and then raw_alpha from list
    hcru_alpha = select(.,starts_with("HCRU")) %>% psych::alpha() %>%
      pluck("total", "raw_alpha"),
    cesd_alpha = select(.,starts_with("CES_D")) %>% psych::alpha() %>%
      pluck("total", "raw_alpha"),
    pss_alpha = select(.,starts_with("PSS")) %>% psych::alpha(check.keys=TRUE) %>%   pluck("total", "raw_alpha"),
    
    # BSM Alphas
    BSM_alpha = select(.,starts_with("BSM")) %>% psych::alpha(check.keys=TRUE) %>%   pluck("total", "raw_alpha"))


## ----------------------------------------------------------------------------------------
# add alphas as extra row to the descriptives table
descriptives <- descriptives %>%
  add_row(Summary = "alpha", SES = NA, CRA = alpha$cra_alpha, HCRU = alpha$hcru_alpha,
          PSS = alpha$pss_alpha, CESD = alpha$cesd_alpha, BSM = alpha$BSM_alpha)


## ----------------------------------------------------------------------------------------
# Make a nicely formatted table
apa_table(descriptives) # is only shown when RMarkdown document is knitted


## ----------------------------------------------------------------------------------------
income_plot<-hist(scored_data$SES,
                  main="Family income distribution",
                  xlab="family income category")


## ----------------------------------------------------------------------------------------
# Mean centre all IVs for regressions with interaction terms
centre_data_to_csv <- function(scored_data){
  centred_data <- scored_data %>%
  mutate(CRA_c = scale(mean_cra, center = TRUE, scale = FALSE),
         SES_c = scale(SES, center = TRUE, scale = FALSE),
         PSS_c = scale(mean_pss, center = TRUE, scale = FALSE),
         HCRU_c = scale(mean_hcru, center = TRUE, scale = FALSE),
         BSM_c = scale(mean_bsm, center = TRUE, scale = FALSE))%>%

  # select the scores/final variables used, remove the raw items
  select(Age, gender, Edu, sum_cesd, SES_c , CRA_c, PSS_c, HCRU_c, BSM_c)
# Save prepared data
write.csv(centred_data,"ready_for_analysis.csv", row.names = FALSE)

return(centred_data)
}


## ----------------------------------------------------------------------------------------
# data <- read_csv("ready_for_analysis.csv") # Read csv data from computer
# or
centred_data <- centre_data_to_csv(scored_data)


## ----------------------------------------------------------------------------------------
# Original model 
org.model <- lm(sum_cesd ~ PSS_c + CRA_c + SES_c + CRA_c:SES_c, data = centred_data)
summary(org.model)


## ----------------------------------------------------------------------------------------
# prettier summary with partial correlations
summ(org.model, confint = TRUE, part.corr = TRUE)


## ----------------------------------------------------------------------------------------
performance::check_model(org.model)


## ----------------------------------------------------------------------------------------
# same model with variable names
mod.mod.model = lm(sum_cesd ~ PSS_c + CRA_c + SES_c + BSM_c + CRA_c*SES_c  + CRA_c*BSM_c + SES_c*BSM_c + CRA_c*SES_c*BSM_c, data = centred_data)
summary(mod.mod.model)


## ----------------------------------------------------------------------------------------
# prettier summary with partial correlations
summ(mod.mod.model, confint = TRUE, part.corr = TRUE)


## ----------------------------------------------------------------------------------------
performance::check_model(mod.mod.model)


## ----------------------------------------------------------------------------------------
interactions::sim_slopes(mod.mod.model, pred = CRA_c, modx=SES_c, mod2 = BSM_c, modxvals = NULL, jnalpha = 0.05, digits = 3, n.sd = 1, jnplot = TRUE, confint = TRUE)


## ----------------------------------------------------------------------------------------
interactions::interact_plot(mod.mod.model, pred = CRA_c, modx = SES_c, mod2 = BSM_c, centered = "none", y.label = "Cognitive Reappraisal Ability",x.label = "Socioeconomic Status", interval = TRUE, data = centred_data)


## ----------------------------------------------------------------------------------------
# compare models
anova(org.model,mod.mod.model)

