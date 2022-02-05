
## DATA Simulation
## Ali Soufizadeh

set.seed(1)
# install packages ###########################
pacman::p_load(pacman, caret, lars, tidyverse, MASS, truncnorm, Runuran,
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
  
  # Socioeconomic Status (ses)
  # Based on the data provided in the study
  ses = rtruncnorm(n = N_total, a=1, b=12, mean = 5.27, sd = 3.06)
  ses = round(ses)
  
  #combine demographics
  demographics <- data.frame(nr = participant_number,
                             age= participant_age,
                             gender = participant_gender,
                             ses = ses)
  
  # cognitive reappraisal ability (CRA)
  cra <- data.frame(cra_1 = sample(1:7, N_total, replace = TRUE),
                    cra_2 = sample(1:7, N_total, replace = TRUE),
                    cra_3 = sample(1:7, N_total, replace = TRUE),
                    cra_4 = sample(1:7, N_total, replace = TRUE),
                    cra_5 = sample(1:7, N_total, replace = TRUE),
                    cra_6 = sample(1:7, N_total, replace = TRUE),
                    cra_7 = sample(1:7, N_total, replace = TRUE),
                    cra_8 = sample(1:7, N_total, replace = TRUE))
  
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
  mcq_question_numbers = 30
  
  # Simulate data for 30 mcq scale questions
  mcq = data.frame(replicate(n = mcq_question_numbers,
                        sample(1:7, N_total, replace = TRUE)))
  
  # Create 30 variable (question) titles: mcq_1 to mcq_30 with a for loop
  column_name_list = NULL # where we store new variable names
  
  for (index in 1 : mcq_question_numbers){ 
    column_name = paste(c("mcq_",as.character(index)),collapse='')
    column_name_list <- append(column_name_list, column_name)
    #i <- i+1
  }
  
  # Change the name of simulated data's columns to mcq_1 to mcq_30
  setnames(mcq, old = colnames(mcq), new = column_name_list)
  
  # Beliefs about Social Mobility (BSM)
  bsm <- data.frame(bsm_1 = sample(1:7, N_total, replace = TRUE),
                    bsm_2 = sample(1:7, N_total, replace = TRUE),
                    bsm_3 = sample(1:7, N_total, replace = TRUE),
                    bsm_4 = sample(1:7, N_total, replace = TRUE),
                    bsm_5 = sample(1:7, N_total, replace = TRUE),
                    bsm_6 = sample(1:7, N_total, replace = TRUE),
                    bsm_7 = sample(1:7, N_total, replace = TRUE),
                    bsm_8 = sample(1:7, N_total, replace = TRUE))
  
  # combine all dataframes
  all_data <- cbind(demographics,cra,hcru,cesd,pss,mcq,bsm)
  
  write.csv(all_data,"simulated_data.csv", row.names = FALSE)
  
  return(all_data)}

all_data = sample_data_function(N_total = 300)

# CLEAN UP #################################################

# Clear environment
rm(list = ls()) 

# Clear packages
p_unload(all)

# Clear console
cat("\014")