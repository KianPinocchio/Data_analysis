# Data Analysis based on Replication protocol
# Researcher: Alimohammad Soufizadeh
# Date: March 25th, 2022

pacman::p_load(pacman, tidyverse, kableExtra, psych, janitor, car, performance, see,
  gridExtra, interactions, devtools,rmarkdown, knitr, patchwork)

library(readxl) # for importing excel file

devtools::install_github("crsh/papaja")
devtools::install_github("easystats/report")

# read all data
raw_data <- read_excel("Coded_Combined.xlsx") #save the csv file as a dataframe named "all_data"

# drop all NA
df <- raw_data
df[4:69]
# Drop NA skipping Age column
df1 <- df[complete.cases(df[4:69]),]

clean_data <- df1

#clean_data <- drop_na(df)
# Check for duplicates
sum(duplicated(clean_data))

# Data summary
summary(clean_data)

# Rename columns
clean_data <- rename(clean_data, "gender" = "Sex")

# Check demographics
clean_data$gender <- factor(clean_data$gender, levels=c(1,2,3,4))
summary (clean_data$gender)


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
  mutate(across(c("PSS2", "PSS3"), ~{6 - .}), # recode PSS items 2 & 3 (6 - response)
         across(c("BSM1", "BSM3", "BSM7", "BSM8"), ~{8 - .}), # first recode BSM items 1, 3, 7, 8 (8 - response)
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

# Descriptives

# In the first step, the data are summarized to get the descriptive statistics.
#Subsequently, the data are reformatted.
descriptives <- scored_data %>% 
  dplyr::summarize(across(c(SES, Age, mean_cra, mean_hcru,sum_cesd, 
                            mean_pss, mean_bsm, sum_pos, 
                            sum_neg, sum_cc, sum_nc, sum_csc, sum_MCQ),
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
         BSM = mean_bsm,  #
         POS = sum_pos,  #
         NEG = sum_neg, # 
         CC = sum_cc, #
         NC = sum_nc, #
         CSC = sum_csc, #
         MCQT = sum_MCQ) # MCQ =Meta Cognitions Questionnaire Total

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
              pluck("total", "raw_alpha"),
            
            # BSM Alphas
            BSM_alpha = select(.,starts_with("BSM")) %>% psych::alpha(check.keys=TRUE) %>%
              pluck("total", "raw_alpha"),
            
            # MCQ scale Alphas
            pos_alpha = select(.,all_of(mcq_pos)) %>% psych::alpha() %>%
              pluck("total", "raw_alpha"),
            neg_alpha = select(.,all_of(mcq_neg)) %>% psych::alpha() %>%
              pluck("total", "raw_alpha"),
            cc_alpha = select(.,all_of(mcq_cc)) %>% psych::alpha() %>%
              pluck("total", "raw_alpha"),
            nc_alpha = select(.,all_of(mcq_nc)) %>% psych::alpha() %>%
              pluck("total", "raw_alpha"),
            csc_alpha = select(.,all_of(mcq_csc)) %>% psych::alpha() %>%
              pluck("total", "raw_alpha"),
            mcq_alpha = select(.,all_of(mcq_csc)) %>% psych::alpha() %>%
              pluck("total", "raw_alpha"))

# add alphas as extra row to the descriptives table
descriptives <- descriptives %>%
  add_row(Summary = "alpha", SES = NA, CRA = alpha$cra_alpha, HCRU = alpha$hcru_alpha,
          PSS = alpha$pss_alpha, CESD = alpha$cesd_alpha, BSM = alpha$BSM_alpha,
          POS = alpha$pos_alpha, NEG = alpha$neg_alpha, CC = alpha$cc_alpha,
          NC = alpha$nc_alpha, CSC = alpha$csc_alpha, MCQT = alpha$mcq_alpha)

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
         HCRU_c = scale(mean_hcru, center = TRUE, scale = FALSE),
         BSM_c = scale(mean_bsm, center = TRUE, scale = FALSE),
         POS_c = scale(sum_pos, center = TRUE, scale = FALSE),
         NEG_c = scale(sum_neg, center = TRUE, scale = FALSE),
         CC_c = scale(sum_cc, center = TRUE, scale = FALSE),
         NC_c = scale(sum_nc, center = TRUE, scale = FALSE),
         CSC_c = scale(sum_csc, center = TRUE, scale = FALSE),
         MCQ_c = scale(sum_MCQ, center = TRUE, scale = FALSE),)

interaction = BSM_c*SES_c
# main regression:
# CRA and SES individually and the interaction between both
fit <- lm(sum_cesd ~ PSS_c + CRA_c + SES_c + CRA_c:SES_c, data = centred_data)
# include our covariates individually:
#HCRU
fit_2 <- lm(sum_cesd ~ PSS_c + CRA_c + SES_c + CRA_c:SES_c + HCRU_c, data = centred_data)
#age
fit_3 <- lm(sum_cesd ~ PSS_c + CRA_c + SES_c + CRA_c:SES_c + Age, data = centred_data)
#gender
fit_4 <- lm(sum_cesd ~ PSS_c + CRA_c + SES_c + CRA_c:SES_c + gender, data = centred_data)
#They also modelled one regression without controlling for life stress (PSS):
fit_6 <- lm(sum_cesd ~ CRA_c + SES_c + CRA_c:SES_c, data = centred_data)

summary(fit7)

check_model(fit)

# CLEAN UP #################################################

# Clear environment
rm(list = ls()) 

# Clear packages
p_unload(all)

# Clear console
cat("\014")
