---
Title: "Data analysis for the extension protocol of study: The interplay of cognitive reappraisal ability, socioeconomic status and mental health."
Author: "Alimohammad Soufizadeh"
Date: "March, 2022"
---

# Load packages
pacman::p_load(pacman, tidyverse, kableExtra, psych, lavaan,
               gridExtra,knitr, readxl, papaja, report, mice, VIM, rmarkdown)


# read all data
raw_data <- read_excel("Coded_data.xlsx")

# Change variable names
raw_data <- rename(raw_data, 
                   "duration" = "Duration(Second)", # in seconds
                   "gender" = "Sex")

# Handling Missing Data

# 1
# 1. drop all NA
# Run only if you want to run analysis with dropping all NA
clean_data <- drop_na(raw_data) 

# 2
# 2. ALT way of handling Missing Data: Drop ALL NA skipping the Age column
clean_data <- raw_data[complete.cases(raw_data[4:69]),]

# 3. Impute missing data
# Missing data pattern
missing.pattern<-md.pattern(raw_data)

# visualise missing data
library(VIM)
mice_plot <- aggr(raw_data,numbers=TRUE, sortVars=TRUE,
                    labels=names(raw_data), cex.axis=.7,
                    gap=3, ylab=c("Missing data","Pattern"))

# Percentage of missing data per row
no_missing <- raw_data
percent_missing <- function(x){sum(is.na(x)/length(x)*100)}

missing <- apply(no_missing,1,percent_missing)
table(missing)

# Look for data to be imputed based on rows
replace_rows <- subset(no_missing, missing <= 5)
no_rows <- subset(no_missing, missing > 5)

# Then look for data to be imputed based on columns
missing <- apply(replace_rows, 2, percent_missing)
table(missing)

replace_columns <- replace_rows %>% select_if(missing<=5)
no_columns <- replace_rows %>% select_if(missing > 5)


#IMPUTE DATA
#Mice easily imputes, taking into account data type.
tempnomiss <- mice(replace_columns)


# Visualise missing data AGAIN!!!
library(VIM)
mice_plot <- aggr(replace_columns,numbers=TRUE, sortVars=TRUE,
                  labels=names(replace_columns), cex.axis=1,
                  gap=1, ylab=c("Missing data","Pattern"))

# Get all the imputed data
imputed_columns <- complete(tempnomiss)

# Putting the data we dropped before back together with the rest
all_columns <- cbind(no_columns, imputed_columns)
'all_rows <- rbind(all_columns, no_rows)' # Only use if you want all other rows with NA binded as well

# Outliers
# Prepare for outlier detection
columns = c("id", "Platform", "consent","duration", "Age","gender", "Edu")
demographics <- subset(all_columns, select = columns) 
all_columns <- all_columns[ , ! names(all_columns) %in% columns]
imputed_data <- cbind(demographics, all_columns) # Save all clean data



# Outliers Mahalanobis
# Degrees of freedom in this case is based on the nr of variables (columns)
# Detect outliers based on **unscored** data

# Mahalanobis for unscored data
mahal <- mahalanobis(imputed_data[ , -c(1:7)],
  colMeans(imputed_data[ , -c(1:7)], na.rm=TRUE),
  cov(imputed_data[ , -c(1:7)], use ="pairwise.complete.obs"))

cutoff <- qchisq(p = 1 - .001, #1 minus alpha
                 df = ncol(imputed_data[ , -c(1:7)])) # number of columns

# Drop all outliers detected based on *unscored* data
cutoff
summary(mahal < cutoff) #notice the direction 
no_outlier <- subset(imputed_data, mahal < cutoff)
summary(no_outlier)


# Score data
# Prepare to score the data with temporarily no outliers

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


# Function to Score data
function_score_data <-function(no_outlier){

#save variables in separate data frame
scored_data <- no_outlier %>%
  
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
return(scored_data)
}

# Score the data with no outliers
scored_data <- function_score_data(no_outlier) #score data first, to find outliers among scored data


# Summary of cleaned data 
# Missing Data & outliers checked
summary(scored_data)

# Assumptions

#Additivity is the assumption that each variable adds something to the model
#You basically do not want to use the same variable twice, as that lowers power
#Often this is described as multicollinearity (The problem is multicollineartiy). The assumption is called additivity.
#Mainly, SEM analysis has a lot of correlated variables, you just want to make sure they aren't perfectly correlated


# Additivity
library(corrplot)
plot_data <- cbind(scored_data$SES,scored_data[ , -c(1:69)])
corrplot(cor(plot_data))


# Assumptions Set Up
random_variable <- rchisq(nrow(scored_data), 7)
fake_model <- lm(scored_data$sum_cesd ~ ., # What's the difference if I regress on scored_data$sum_cesd ??
                 data = scored_data[ , -c(1:69)])
standardized <- rstudent(fake_model) # residuals
fitvalues <- scale(fake_model$fitted.values) # z-score them

# Linearity
#We assume the the multivariate relationship between continuous variables is linear (i.e., no curved)
#There are many ways to test this, but we can use a QQ/PP Plot to examine for linearity
plot(fake_model, 2)

# Normality
#We expect that the residuals are normally distributed
hist(standardized)

#Assumptions Homogeneity and Homoscedasticity
#These assumptions are about equality of the variances
#Here the assumption is equality in the spread of variance across predicted values 
{plot(standardized, fitvalues)
  abline(v = 0)
  abline(h = 0)
}

# Check for duplicates
sum(duplicated(scored_data))

# Check demographics
scored_data$gender <- factor(scored_data$gender, levels=c(1,2,3,4)) # Factor gender
summary(scored_data$gender)

# Descriptives
#In the first step, the data are summarized to get the descriptive statistics.
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

# Cronbach’s alphas
#Select the items from the *raw* or *un-scored* data that belong to the specific scale.
#calculate alpha and extract raw_alpha from the list the alpha function generates.

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

# Descriptives table
# Make a nicely formatted table
apa_table(descriptives) # is only shown when RMarkdown document is knitted

# Plot monthly family income
income_plot<-hist(scored_data$SES,
                  main="Family income distribution",
                  xlab="family income category")

# Mean centre all IVs
# Mean centre all IVs for regressions with interaction terms
centre_data_to_csv <- function(scored_data){
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
         MCQ_c = scale(sum_MCQ, center = TRUE, scale = FALSE))%>%

  # select the scores/final variables used, remove the raw items
  select(Age, gender, Edu, sum_cesd, SES_c , CRA_c, PSS_c, HCRU_c, BSM_c, POS_c, NEG_c, CC_c, CSC_c, MCQ_c)
# Save prepared data
write.csv(centred_data,"ready_for_analysis.csv", row.names = FALSE)

return(centred_data)
}

# read prepared data for analysis
# data <- read_csv("ready_for_analysis.csv") # Read csv data from computer
# or
ready_for_analysis <- centre_data_to_csv(scored_data)

# Hypotheses B1 & B2: Three-way interaction

# Rename variables for convenience in modelling
data <- ready_for_analysis %>% mutate(rename(ready_for_analysis,
                                "X" = "CRA_c",
                                "Y" = "sum_cesd",
                                "W" = "SES_c",
                                "Z" = "BSM_c",
                                "COV" = "PSS_c",
                               "M" = "NEG_c"))

# Create interaction terms
data <- data %>% mutate(X.W = X * W,
                        X.Z = X * Z,
                        W.Z = W * Z,
                        X.W.Z = X * W * Z,
                        M.W = M * W,
                        M.Z = M * Z,
                        M.W.Z = M * W * Z)

# Original model 
org.model <- lm(sum_cesd ~ PSS_c + CRA_c + SES_c + CRA_c:SES_c, data = data)

# Three-way moderation model
mod.mod.model = lm(sum_cesd ~ PSS_c + CRA_c + SES_c + BSM_c + CRA_c*SES_c  + CRA_c*BSM_c + SES_c*BSM_c + CRA_c*SES_c*BSM_c, data = data)
summary(mod.mod.model)

# Check model fit
performance::check_model(mod.mod.model)

# Simple slopes analysis
interactions::sim_slopes(mod.mod.model, pred = X, modx=W, mod2 = Z, modxvals = NULL, jnalpha = 0.05, digits = 3, n.sd = 1)

# Plot simple slopes for interaction
interactions::interact_plot(mod.mod.model, pred = X, modx = W, mod2 = Z, centered = "none", y.label = "Cognitive Reappraisal Ability",x.label = "Socioeconomic Status", interval = TRUE, data = data)

# compare models
anova(org.model,mod.mod.model)

# Hypotheses C1 & C2: Moderated Mediation
# The Model
MOD.MED.model <-
"
# Mediator
M ~ a*X

# Outcome being predicted by the model
Y ~ c1*X + c2*W + c3*Z + c4*X.W + c5*X.Z + c6*W.Z + c7*X.W.Z +
b1*M + b2*W + b3*Z + b4*M.W + b5*M.Z + b6*W.Z + b7*M.W.Z + g*COV

# Covariance: A main predictor's covariance is fixed so it does not 
# covary with the rest of the variables, since the theory does not explicitely address this
COV ~~ COV

# Intercept and variance of W for conditional effects analysis
W ~ W.mean*1
W ~~ W.var * W

# Intercept and variance of Z for conditional effects analysis
Z ~ Z.mean*1
Z ~~ Z.var*Z

# Indirect effect of X on Y through M, conditional on mean values of W and Z
indirect := (a) * ( b1 + b4*W.mean + b5*Z.mean)

# Direct effect of X on Y, moderated by W and Z
direct := c1 + c4*W.mean + c5*Z.mean

# Total effect
total := direct + indirect

# Proportion of effect mediated
prop.mediated := indirect / total

# Conditional indirect effect through mediator (M)
indirect.below :=(a)*(b1+b4*(W.mean - sqrt(W.var))+ b5*(Z.mean - sqrt(Z.var)))
indirect.above :=(a)*(b1+b4*(W.mean + sqrt(W.var))+ b5*(Z.mean + sqrt(Z.var)))

# Conditional direct effect moderated by W and Z
direct.below:=c1 + c4*(W.mean - sqrt(W.var)) + c5*(Z.mean - sqrt(Z.var))
direct.above:=c1 + c4*(W.mean + sqrt(W.var)) + c5*(Z.mean + sqrt(Z.var))

# Conditional total effect
total.below := direct.below + indirect.below
total.above := direct.above + indirect.above

# Proportion of mediation under conditions of W and one SD above or below mean
prop.mediated.below := indirect.below / total.below
prop.mediated.above := indirect.above / total.above
"

# Fit the model
Mod.Med.SEM <- sem(model = MOD.MED.model,
                   data = data,
                   se = "bootstrap",
                   bootstrap = 200) # do 5000 when actually running the code

# Model summary
summary(Mod.Med.SEM, fit.measures = FALSE, standardize = TRUE, rsquare = TRUE)

# Parameters table
parameterEstimates(Mod.Med.SEM, remove.nonfree = TRUE)


# Plot the moderated mediation model
# Plot model
library(semPlot)
plot = semPaths(Mod.Med.SEM, fixedStyle = 1, layout = "tree", 
                  intercepts = F, label.scale=T, nCharNodes = 0, 
                  sizeMan2=3, sizeMan=7, asize=2, residuals = F, exoCov = T)

# CLEAN UP
# Clear environment
rm(list = ls()) 

# Clear packages 
p_unload(all)

# Clear console
cat("\014")