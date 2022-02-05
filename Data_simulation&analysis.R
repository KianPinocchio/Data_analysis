
## DATA Simulation and Analysis
## Ali Soufizadeh

set.seed(1)
# install packages ###########################
pacman::p_load(pacman, caret, lars, tidyverse, MASS, truncnorm, Runran,
               kableExtra,
               psych,
               janitor,
               car,
               performance,
               see,
               gridExtra,
               interactions,
               devtools)

# Sample data to run the analysis code #
sample_data_function = function(N_total){
  #### Replication Block ####
  
  #Participant Number
  participant_number = 1:N_total
  
  #Age
  participant_age = round(urnorm(n=N_total,mean=36.62,sd=13.5,lb=18))
  
  #gender
  participant_gender = sample(c("m","f"), 
                              prob = c(0.52,0.48), 
                              size = N_total, 
                              replace = T)
  
  ## turn gender into factor for regression
  participant_gender = as.factor(participant_gender)
  
  # cognitive reappraisal ability (CRA)
  cra <- data.frame(cra_1 = sample(1:7, N_total, replace = TRUE),
                    cra_2 = sample(1:7, N_total, replace = TRUE),
                    cra_3 = sample(1:7, N_total, replace = TRUE),
                    cra_4 = sample(1:7, N_total, replace = TRUE),
                    cra_5 = sample(1:7, N_total, replace = TRUE),
                    cra_6 = sample(1:7, N_total, replace = TRUE),
                    cra_7 = sample(1:7, N_total, replace = TRUE),
                    cra_8 = sample(1:7, N_total, replace = TRUE))
  
  # Socioeconomic Status (ses)
  # Based on the data provided in the study
  ses = rtruncnorm(n = N_total, a=1, b=12, mean = 5.27, sd = 3.06)
  ses = round(ses)
  
  #Habitual Cognitive Reappraisal Use  (hcru)
  hcru <- data.frame(hcru_1 = sample(1:7, N_total, replace = TRUE),
                     hcru_2 = sample(1:7, N_total, replace = TRUE),
                     hcru_3 = sample(1:7, N_total, replace = TRUE),
                     hcru_4 = sample(1:7, N_total, replace = TRUE),
                     hcru_5 = sample(1:7, N_total, replace = TRUE),
                     hcru_6 = sample(1:7, N_total, replace = TRUE))
  
  #Perceived Stress Scale (PSS)
  pss <- data.frame(pss_1 = sample(1:5, N_total, replace = TRUE),  # responses to item 1..
                    pss_2 = sample(1:5, N_total, replace = TRUE),
                    pss_3 = sample(1:5, N_total, replace = TRUE),
                    pss_4 = sample(1:5, N_total, replace = TRUE))
  
  # Depressive Symptoms (CES-D 5 items)
  cesd <- data.frame(cesd_1 = sample(0:3, N_total, replace = TRUE),  # responses to item 1..
                     cesd_2 = sample(0:3, N_total, replace = TRUE),
                     cesd_3 = sample(0:3, N_total, replace = TRUE),
                     cesd_4 = sample(0:3, N_total, replace = TRUE),
                     cesd_5 = sample(0:3, N_total, replace = TRUE))
  
  # Meta-cognitions questionnaire-30 (MCQ-30 30-items)
  cra <- data.frame(cra_1 = sample(1:7, N_total, replace = TRUE),
                    cra_2 = sample(1:7, N_total, replace = TRUE),
                    cra_3 = sample(1:7, N_total, replace = TRUE),
                    cra_4 = sample(1:7, N_total, replace = TRUE),
                    cra_5 = sample(1:7, N_total, replace = TRUE),
                    cra_6 = sample(1:7, N_total, replace = TRUE),
                    cra_7 = sample(1:7, N_total, replace = TRUE),
                    cra_8 = sample(1:7, N_total, replace = TRUE))
  
  # combine all dataframes
  all_data <- left_join(income, cra, by = "participant_number") %>%   # %>% 
    left_join(., hcru, by = "participant_number") %>% 
    left_join(., cesd, by = "participant_number") %>% 
    left_join(., pss, by = "participant_number") %>% 
    left_join(., demographics, by = "participant_number")
  
  #interaction of CRA and SES
  interaction = (CRA * SES * int_coefficient)
  
  #Outcome based on Troy et al. (2017) model without covariates
  # #CES-D ~ PSS + CRA + SES + CRA*SES
  outcome = (CRA*CRA_coefficient + 
               SES*SES_coefficient + 
               interaction +
               PSS*PSS_coefficient + 
               error)
  
  POS = 11.80 (4.29)
  NEG =  13.30 (4.04)
  CC =  11.50 (3.98)
  NC =  15.70 (3.49)
  CSC =  16.00 (4.00)
  #### Extension+ Block ####
  #Meta-cognition questionnaire-30 (MCQ)
  # Estimation based on Tajrishi et al. (2011)
  MCQ_POS = rtruncnorm(n = N_total, a=6, b=24, mean = 11.80, sd = 4.29)
  MCQ_POS = round(MCQ_POS)
  
  MCQ_NEG = rtruncnorm(n = N_total, a=6, b=24, mean = 13.30, sd = 4.04)
  MCQ_NEG = round(MCQ_NEG)
  
  MCQ_CC = rtruncnorm(n = N_total, a=6, b=24, mean = 11.50, sd = 3.98)
  MCQ_CC = round(MCQ_CC)
  
  MCQ_NC = rtruncnorm(n = N_total, a=6, b=24, mean = 15.70, sd = 3.49)
  MCQ_NC = round(MCQ_NC)
  
  MCQ_CSC = rtruncnorm(n = N_total, a=1, b=5, mean = 16.00, sd = 4.00)
  MCQ_CSC = round(MCQ_CSC)
  
  # Store all simulated data into a dataframe
  data = data.frame(nr = participant_number,
                    age = participant_age,
                    gender = participant_gender,
                    outcome = outcome,
                    CRA = CRA,
                    SES = SES,
                    PSS = PSS,
                    HCRU = HCRU,
                    MCQ_POS = MCQ_POS,
                    MCQ_NEG = MCQ_NEG,
                    MCQ_CC = MCQ_CC,
                    MCQ_NC = MCQ_NC,
                    MCQ_CSC = MCQ_CSC)
  
  return(data)}

#### Descriptives ####
# means, sd, min, max
# In the first step, the data are summarized to get the descriptive statistics.
# Subsequently, the data are reformatted. 

data_simulation_function = function(N_total, 
                                    CRA_coefficient,
                                    SES_coefficient,
                                    PSS_coefficient,
                                    int_coefficient)
  
descriptive_function = function(scored_data){ 
  
  descriptives <- scored_data %>% 
  summarize(across(c(SES, CRA, PSS, HCRU, sum_cesd, age), 
                   list(mean = mean, sd = sd, min = min, max = max))) %>% 
  # bring everything in long format
  pivot_longer(everything(), names_to = "name") %>%   
  # separate names at last underscore
  separate(name, into = c("name","descriptive"), sep = "_(?=[^_]+$)") %>%          
  # get into a bit wider format again
  pivot_wider(names_from = name, values_from = value) %>%
  # rename to have nicer column names
  rename(Summary = descriptive,  SES = family_income, CRA =  mean_cra,
         HCRU = mean_hcru, PSS = mean_pss, CESD = sum_cesd) 
}

# Calculate cronbach's alphas
## *Use RAW scores to calculate alphas

# Select the items from the raw data that belong to the specific scale.
# calculate alpha and extract raw_alpha from the list the alpha function generates.
alpha <- all_data %>% 
  summarize(cra_alpha = select(.,starts_with("cra")) %>% psych::alpha() %>% 
              pluck("total", "raw_alpha"),  # extract total and then raw_alpha from list 
            hcru_alpha = select(.,starts_with("hcru")) %>% psych::alpha() %>% 
              pluck("total", "raw_alpha"), 
            cesd_alpha = select(.,starts_with("cesd")) %>% psych::alpha() %>%
              pluck("total", "raw_alpha"),
            pss_alpha = select(.,starts_with("pss")) %>% psych::alpha() %>%
              pluck("total", "raw_alpha"))



# CLEAN UP #################################################

# Clear environment
rm(list = ls()) 

# Clear packages
p_unload(all)

# Clear console
cat("\014")