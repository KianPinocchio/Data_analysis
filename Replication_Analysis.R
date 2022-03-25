# Data Analysis based on Replication protocol
# Researcher: Alimohammad Soufizadeh
# Date: March 25th, 2022

pacman::p_load(pacman, tidyverse, kableExtra, psych, janitor, car, performance, see,
  gridExtra, interactions, devtools)

library(readxl) # for importing excel file

devtools::install_github("crsh/papaja")
devtools::install_github("easystats/report")

# read all data
raw_data <- read_excel("Coded_Combined.xlsx") #save the csv file as a dataframe named "all_data"

# drop all NA
clean_data <- drop_na(raw_data)

# Check for duplicates
sum(duplicated(clean_data))

# Data summary
summary(clean_data)

# Rename columns
clean_data <- rename(clean_data, "gender" = "Sex")

# Check demographics
clean_data$gender <- factor(clean_data$gender, levels=c(1,2,3,4),
                          labels=c("Male", 
                                   "Female",
                                   "non-Binary",
                                   "IDWA"))
summary (clean_data$gender)
summary (clean_data$Age)

#Score questionnaires:

# Create MCQ subscale variable groups
mcq_pos <- c("MCQ1","MCQ7","MCQ10",
             "MCQ19","MCQ23","MCQ28")

mcq_neg <- c("MCQ2","MCQ4","MCQ9",
             "MCQ11","MCQ15","MCQ21")

mcq_cc <- c("MCQ8","MCQ14","MCQ17",
             "MCQ24","MCQ26","MCQ29")

mcq_nc <- c("MCQ6","MCQ13","MCQ20",
             "MCQ22","MCQ25","MCQ27")

mcq_csc <- c("MCQ3","MCQ5","MCQ12",
             "MCQ16","MCQ18","MCQ30")


#save variables in separate data frame
scored_data <- clean_data %>%
  
  #row mean of the selected columns
  mutate(across(c("PSS2", "PSS3"), ~{6 - .}), # first recode PSS items 2 & 3 (6 - response)
         mean_cra = rowMeans(select(., starts_with("CRA"))),
         mean_hcru = rowMeans(select(., starts_with("HCRU"))),
         mean_pss = rowMeans(select(., starts_with("PSS"))),
        
         mean_bsm = rowMeans(select(., starts_with("BSM"))),
          #for cesd, we need the sum
         sum_cesd = rowSums(select(., starts_with("CES_D"))),
         
         # sum all MCQ subscales
         sum_pos = rowSums(select(.,all_of(mcq_pos))),
         sum_neg = rowSums(select(.,all_of(mcq_neg))),
         sum_cc = rowSums(select(.,all_of(mcq_cc))),
         sum_nc = rowSums(select(.,all_of(mcq_nc))),
         sum_csc = rowSums(select(.,all_of(mcq_csc))),
         sum_MCQ = rowSums(select(.,starts_with("MCQ"))))%>%
         
  
  # select the scores/final variables used, remove the raw items
  select(id, Age, SES, gender, mean_cra, mean_hcru,
         sum_cesd, mean_pss, mean_bsm, sum_pos, sum_neg, sum_cc, sum_nc, sum_csc, sum_MCQ)



# CLEAN UP #################################################

# Clear environment
rm(list = ls()) 

# Clear packages
p_unload(all)

# Clear console
cat("\014")
