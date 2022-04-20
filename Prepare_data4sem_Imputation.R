# title: impute and clean data for SEM
# date: "April, 2022"

## set seed
set.seed(79)

# Load packages
pacman::p_load(pacman, tidyverse, kableExtra, psych, corrplot, performance, MVN, ICS,
               dplyr, gridExtra, knitr, readxl, papaja, mice, VIM, janitor)

# read raw data
raw_data <- read_excel("/Users/Pinocchio/R/Thesis_git_repository/Data_analysis/Data/Coded_data.xlsx")

# Change variable names
df_adjr <- rename(raw_data, # adjust raw data frame (df_adjr) for analyses
                   "duration" = "Duration(Second)", # in seconds
                   "gender" = "Sex")

columns = c("id", "Platform", "consent","duration", "Age","gender", "Edu", "SES")
demographics <- subset(df_adjr, select = columns) 
df_adjr <- df_adjr[ , ! names(df_adjr) %in% columns]
df_adjr <- cbind(demographics, df_adjr)


# summary raw data
describe(df_adjr)

# visualise missing data
mice_plot <- aggr(df_adjr,numbers=TRUE, sortVars=TRUE,
                    labels=names(df_adjr), cex.axis=.7,
                    gap=3, ylab=c("Missing data","Pattern"))

# exclusion criteria
# Percentage of missing data per row
temp_no_missing <- df_adjr
percent_missing <- function(x){sum(is.na(x)/length(x)*100)}
missing <- apply(temp_no_missing,1,percent_missing)
table(missing)
  
# Look for data to be imputed based on rows
replace_rows <- subset(temp_no_missing, missing <= 5)
no_rows <- subset(temp_no_missing, missing > 5)
  
# Then look for data to be imputed based on columns
missing <- apply(replace_rows[,-c(1:7)], 2, percent_missing)
table(missing)
replace_columns <- replace_rows[,-c(1:7)] %>% select_if(missing<=5)
no_columns <- replace_rows[,-c(8:69)]

# Describe data
temp_data <- cbind(no_columns, replace_columns)
describe(temp_data[,c(1:8)])

# Imputation function
function_data_imputation <- function(replace_columns, no_columns){  #IMPUTE DATA
  tempnomiss <- mice(replace_columns, print=FALSE)
  
  imputed_columns <- complete(tempnomiss) # Get all the imputed data
  
  clean_data <- cbind(no_columns, imputed_columns) # Putting the data we dropped before back together with the rest
  
  return(clean_data)
}

# Impute data
clean_data <- function_data_imputation(replace_columns, no_columns) # Run the data imputation function or the replication criteria for exclusion

describe(clean_data)

# visualise missing after imputation
mice_plot <- aggr(clean_data,numbers=TRUE, sortVars=TRUE,
                  labels=colnames(clean_data), cex.axis=1,
                  gap=1, ylab=c("Missing data","Pattern"))

# identify outliers based on Mahalanobis distance
mahal <- mahalanobis(clean_data[ , -c(1:7)], # Mahalanobis for unscored data
  colMeans(clean_data[ , -c(1:7)], na.rm=TRUE),
  cov(clean_data[ , -c(1:7)], use ="pairwise.complete.obs"))

cutoff <- qchisq(p = 1 - .001, #1 minus alpha
                 df = ncol(clean_data[ , -c(1:7)])) # number of columns

cutoff # mahalanobis cutoff score

# Drop all outliers detected based on *unscored* data
summary(mahal < cutoff) #notice the direction 
no_outlier <- subset(clean_data, mahal < cutoff)

# Summarise
describe(no_outlier)

# Check for duplicates
sum(duplicated(no_outlier))

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
         sum_csc = rowSums(select(.,all_of(mcq_csc))))%>%
  
  return(scored_data)
}

# Run Score data FUN
scored_data <- function_score_data(no_outlier) # Score the data with no outliers

# Save scored data to csv
write.csv(scored_data, "SEM_data_imputed.csv", row.names = FALSE)

# summary of  scored data
describe(scored_data[,-c(1:69)]) # Missing Data & outliers checked

# correlation plot
plot_data <- cbind(scored_data["SES"],scored_data[ , c(70:80)])
corPlot(cor(plot_data), upper = F)

# normality test
mvntest_result <- mvn(scored_data[8:69], mvnTest = "hz")
mvntest_result$multivariateNormality

# Kurtosis test
mvnorm.kur.test(scored_data[8:69], method = "simulation")

# Skewness test
mvnorm.skew.test(scored_data[8:69])

# Demographics
# Gender
gender_freq <- scored_data %>% tabyl(gender)
levels(gender_freq$gender) <- c("male","female", "non-binary", "DWA")
apa_table(gender_freq, caption = "Proportions of Gender")

# Education
edu_freq <- scored_data %>% tabyl(Edu)
levels(edu_freq$Edu) <- c("Elementary or lower","Highschool Diploma", "Bachelor's", "Master's", "PhD or higher")
apa_table(edu_freq, caption = "Proportions of Education level")

# Compute Descriptives
descriptives <- scored_data %>% 
  summarize(across(c(SES, Age, mean_cra, mean_hcru,sum_cesd, 
                            mean_pss, mean_bsm, sum_pos,sum_neg, 
                            sum_cc, sum_nc, sum_csc, sum_MCQ),
                          list(mean = mean, sd = sd, min = min, max = max)), na.rm= T) %>%
  
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
    
    # BSM Alpha
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
  add_row(Summary = "alpha", SES = NA, Age = NA, CRA = alpha$cra_alpha, HCRU = alpha$hcru_alpha,
          PSS = alpha$pss_alpha, CESD = alpha$cesd_alpha, BSM = alpha$BSM_alpha, 
          POS = alpha$pos_alpha, NEG = alpha$neg_alpha, CC = alpha$cc_alpha,
          NC = alpha$nc_alpha, CSC = alpha$csc_alpha, MCQT = alpha$mcq_alpha)

# Descriptives table
apa_table(descriptives) # is only shown when RMarkdown document is knitted

# income plot
income_plot<-hist(scored_data$SES,
                  main="Family income distribution",
                  xlab="family income category")
