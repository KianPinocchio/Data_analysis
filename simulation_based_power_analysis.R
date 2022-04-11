
## Objectives:
## 1.To decide on an appropriate Standard Deviation of Error 
#   for the simulated data based on data from an existing study.
# 
# 2. Use the estimated SD to simulate the data and run a simulation based Power Analysis
## The outcome (CES-D-5) in Study 1 (Troy et al., 2017) had the following  properties: (M = 3.46, SD = 3.34, alpha =.81)
# In addition the study reports total number of participants (N_total) 
# and the regression coefficients of all variables needed to simulate data and 
# estimate the error of the outcome variable.

#   I will be using the approach outlined here https://github.com/kekecsz/Simulation_based_power_analysis.git

## Reference: Troy, A. S., Ford, B. Q., McRae, K., Zarolia, P., & Mauss, I. B. (2017). 
#   Change the things you can: Emotion regulation is more beneficial for people from lower than from higher socioeconomic status. 
#     Emotion, 17(1), 141–154. https://doi.org/10.1037/emo0000210

set.seed(79)

# install packages ######################
pacman::p_load(pacman, tidyverse, truncnorm, Runuran, dplyr)

# simulation function ###################
data_simulation_function <- function(N_total, 
                                    CRA_coefficient,
                                    SES_coefficient,
                                    PSS_coefficient,
                                    int_coefficient,
                                    sd_error){
  
  #cognitive reappraisal ability (CRA)
  CRA = rtruncnorm(n = N_total, a=1, b=7, mean = 5.12, sd = 1.21)
  CRA = round(CRA)
  
  #Socioeconomic Status (SES)
  SES = rtruncnorm(n = N_total, a=1, b=12, mean = 5.27, sd = 3.06)
  SES = round(SES)
  
  #Perceived Stress Scale (PSS)
  PSS = rtruncnorm(n = N_total, a=1, b=5, mean = 2.70, sd = 0.9)
  PSS = round(PSS)
  
  #error
  error = rnorm(n = N_total, m = 0, sd = sd_error)
  
  #interaction of CRA and SES
  interaction = (CRA * SES * int_coefficient)
  
  # Depressive Symptoms (CES-D)
  outcome = (CRA*CRA_coefficient + 
               SES*SES_coefficient + 
               interaction +
               PSS*PSS_coefficient + 
               error)
  
  # # Replication study data
  replication_data = data.frame(outcome = outcome,
                              CRA = CRA,
                              SES = SES,
                              PSS = PSS)

  # # Simulate Meta-cognitive questionnaire data based on Estimation based on Tajrishi (2011)
  mcq_pos = rtruncnorm(n = N_total, a=6, b=24, mean = 11.80 , sd = 4.29) # Positive beliefs about worry
  mcq_pos = round(mcq_pos)

  mcq_neg = rtruncnorm(n = N_total, a=6, b=24, mean = 13.30 , sd = 4.04) # negative beliefs about uncontrolability of worry
  mcq_neg = round(mcq_neg)

  mcq_cc = rtruncnorm(n = N_total, a=6, b=24, mean = 11.50, sd = 3.98)
  mcq_cc = round(mcq_cc)

  mcq_nc = rtruncnorm(n = N_total, a=6, b=24, mean = 15.70, sd = 3.49)
  mcq_nc = round(mcq_nc)

  mcq_csc = rtruncnorm(n = N_total, a=6, b=24, mean = 16.00, sd = 4.00)
  mcq_csc = round(mcq_csc)

  extension_study_data <- data.frame(mcq_pos = mcq_pos,
                                      mcq_neg = mcq_neg,
                                      mcq_cc = mcq_cc,
                                      mcq_nc = mcq_nc,
                                      mcq_csc = mcq_csc)

  # Simulate Beliefs about Social Mobility (BSM) data.
  bsm = rtruncnorm(n = N_total, a=1, b=7, mean = 4 , sd = 1)
  bsm = round(bsm)
  
  extension_study_data$mcq_t <- apply(extension_study_data,1,mean) #scoring mcq scale
  
  extension_study_data$bsm <- bsm # adding the bsm score to the dataframe
  
  # Storing all simualted data together
  simulated_data = cbind(replication_data, extension_study_data)
  
  
  return(simulated_data) }


## Here we replicate the simulation function many times
#   to get a large sample of various Standard Deviations of our simulations
error_simulation <- function(N_total, 
                             CRA_coefficient,
                             SES_coefficient,
                             PSS_coefficient,
                             int_coefficient,
                             sd_error,
                             nr_iterations){
  
  # replicate the simulation function
  data = data.frame(replicate( n = nr_iterations,
                               data_simulation_function(N_total, 
                                                        CRA_coefficient,
                                                        SES_coefficient,
                                                        PSS_coefficient,
                                                        int_coefficient,
                                                        sd_error)))
  
  ## Steps to make outcome data readable to visualise by hist() later on
  # 1.extract outcome data
  outcome <- data.frame(data["outcome",])
  
  # 2.extract outcome observation indexes / column names
  outcome_observations = c(colnames(outcome))
  
  # 3.create a readable dataframe of outcome data
  outcome <- outcome %>% 
    unchop(all_of(outcome_observations))
  
  # 4.calculate Standard Deviation of all outcome observations
  outcome <- apply(outcome,2,sd)
  
  return(outcome)
}

# visualise the standard deviation of all the simulated outcomes to decide 
# on the best estimate of error's standard deviation
sd_of_error_estimation <- function(N_total,
                                   CRA_coefficient,
                                   SES_coefficient,
                                   PSS_coefficient,
                                   int_coefficient,
                                   sd_error, # Standard Deviation of error
                                   nr_iterations){
  
  all_outcome_observations_SD = error_simulation(N_total,
                                                 CRA_coefficient,
                                                 SES_coefficient,
                                                 PSS_coefficient,
                                                 int_coefficient,
                                                 sd_error,
                                                 nr_iterations)
  
  ## summarise our results: We are looking for a Mean close to the 
  #  Standard Deviation of the outcome reported by the study (SD = 3.34)
  print(summary(all_outcome_observations_SD))
  
  # visualise all observations.
  hist(all_outcome_observations_SD, main = "outcomes' standard deviations")
}


sd_of_error_estimation(N_total = 301,
                       CRA_coefficient = -0.15,
                       SES_coefficient = -0.03,
                       PSS_coefficient = 0.53,
                       int_coefficient = 0.09,
                       sd_error = 3,
                       nr_iterations = 100)


#### Regression Analysis ####
# Using the estimated SD (SD = 3) we run a simulation based power analysis below
# Model is based on the study: entering all IVs at the same time
regression_analysis <- function(data){ 
  regression = lm(outcome ~ PSS + CRA + SES+ CRA:SES, data = data)
  p_value = summary(regression)$coefficients[2,4]
  
  decision = if(p_value < 0.05){"H1"} else {"Inconclusive"}
  
  return(decision) }

### Analysis and Simulation function together
analysis_and_simulation_function <- function(N_total, 
                                             CRA_coefficient,
                                             SES_coefficient,
                                             PSS_coefficient,
                                             int_coefficient,
                                             sd_error){
  
  simulated_data = data_simulation_function(N_total, 
                                            CRA_coefficient,
                                            SES_coefficient,
                                            PSS_coefficient,
                                            int_coefficient,
                                            sd_error)
  
  decision = regression_analysis(data = simulated_data)
  
  return(decision)
}

# Number of times we iterate the Simulation and Analysis
number_iterations = 1000

## Iterate all functions
## Save all H1 accepted results
all_decisions = replicate(n = number_iterations, 
                          analysis_and_simulation_function(N_total = 400, 
                                                            CRA_coefficient = -0.15,
                                                            SES_coefficient = -0.03,
                                                            PSS_coefficient = 0.53,
                                                            int_coefficient = 0.09,
                                                            sd_error = 3))

## Percentage of times H1 accepted
sum(all_decisions == "H1")/length(all_decisions)

# CLEAN UP #################################################

# Clear environment
rm(list = ls()) 

# Clear packages
p_unload(all)

# Clear console
cat("\014")
