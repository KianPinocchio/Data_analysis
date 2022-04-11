
# title: "Replication of the data analysis in Study1 by Troy et al. (2017) with an Iranian sample."
# author: "Alimohammad Soufizadeh"
# date: "March, 2022"

set.seed(79)

## ---------------------------------------------------------------------------------------------------------------------------------------------------------
pacman::p_load(pacman, tidyverse, kableExtra, psych, janitor, car, performance, 
               see,gridExtra, interactions, devtools,rmarkdown, 
               knitr, patchwork, readxl,papaja, jtools)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------
# save the excel file as a dataframe named "all_data"
raw_data <- read_excel("Coded_data.xlsx")


## ---------------------------------------------------------------------------------------------------------------------------------------------------------
# Change column names
raw_data <- rename(raw_data, 
                   "duration" = "Duration(Second)",
                   "gender" = "Sex")


## ---------------------------------------------------------------------------------------------------------------------------------------------------------
# summarise raw data
summary(raw_data)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------
# 1. exclude all who didn't complete the survey or had missing data
no_missing <- drop_na(raw_data)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------
# 2. exclude participants with zero variance among their answers
no_zero_var <- no_missing[apply(no_missing[, -c(1:4)], 1, var) != 0, ]


## ---------------------------------------------------------------------------------------------------------------------------------------------------------
# 3. exclude participants who took less than 3 minutes (180 seconds) to complete the survey
clean_data <- no_zero_var %>% filter(duration >= 180)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------
# Check for duplicates
sum(duplicated(clean_data))


## ---------------------------------------------------------------------------------------------------------------------------------------------------------
summary(clean_data)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------
# Gender
clean_data$gender <- factor(clean_data$gender, levels=c(1,2,3,4))
summary(clean_data$gender)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------
# Education
clean_data$Edu <- factor(clean_data$Edu, levels=c(1,2,3,4,5))
summary(clean_data$Edu)

## ---------------------------------------------------------------------------------------------------------------------------------------------------------
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

## ---------------------------------------------------------------------------------------------------------------------------------------------------------
scored_data <- clean_data %>%
  mutate(across(c("PSS2", "PSS3"), ~{6 - .}),
         across(c("BSM1", "BSM3", "BSM7", "BSM8"), ~{8 - .}),
         mean_cra = rowMeans(select(., starts_with("CRA"))),
         mean_hcru = rowMeans(select(., starts_with("HCRU"))),
         mean_pss = rowMeans(select(., starts_with("PSS"))),
         mean_bsm = rowMeans(select(., starts_with("BSM"))),
         sum_cesd = rowSums(select(., starts_with("CES_D"))),
         
         mean_bsm = rowMeans(select(., starts_with("BSM"))),
         #for cesd, we need the sum
         sum_cesd = rowSums(select(., starts_with("CES_D"))),
         
         # sum all MCQ subscales
         sum_pos = rowSums(select(.,all_of(mcq_pos))),
         sum_neg = rowSums(select(.,all_of(mcq_neg))),
         sum_cc = rowSums(select(.,all_of(mcq_cc))),
         sum_nc = rowSums(select(.,all_of(mcq_nc))),
         sum_csc = rowSums(select(.,all_of(mcq_csc))),
         sum_MCQ = rowSums(select(.,starts_with("MCQ"))))

## ---------------------------------------------------------------------------------------------------------------------------------------------------------
write.csv(scored_data, "scored_data.csv", row.names = FALSE) # Save scored data in a csv file

## ---------------------------------------------------------------------------------------------------------------------------------------------------------
gender_freq <- scored_data %>% tabyl(gender)
levels(gender_freq$gender) <- c("male","female", "non-binary", "DWA")

edu_freq <- scored_data %>% tabyl(Edu)
levels(edu_freq$Edu) <- c("Elementary or lower","Highschool Diploma", "Bachelor's", "Master's", "PhD or higher")

frequencies<-edu_freq %>% #combine tables
add_row(Edu=gender_freq$gender, n = gender_freq$n, percent = gender_freq$percent) %>%
rename(Category = Edu)

apa_table(frequencies, caption = "Proportions of Education level and gender categories")


## ---------------------------------------------------------------------------------------------------------------------------------------------------------
descriptives <- scored_data %>% 
  dplyr::summarize(across(c(SES, Age, mean_cra, mean_hcru,sum_cesd, 
                            mean_pss),
                   list(mean = mean, sd = sd, min = min, max = max)))%>%

  pivot_longer(everything(), names_to = "name") %>%
  
  separate(name, into = c("name","descriptive"), sep = "_(?=[^_]+$)")%>%
  
  pivot_wider(names_from = name, values_from = value) %>%
  
  rename(Summary = descriptive,
         CRA = mean_cra,  
         HCRU = mean_hcru,
         PSS = mean_pss,  
         CESD = sum_cesd)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------
alpha <- clean_data %>%
  dplyr::summarize(
            # Replication Block Alphas
            cra_alpha = select(.,starts_with("CRA")) %>% psych::alpha() %>%
              pluck("total", "raw_alpha"),
            hcru_alpha = select(.,starts_with("HCRU")) %>% psych::alpha() %>%
              pluck("total", "raw_alpha"),
            cesd_alpha = select(.,starts_with("CES_D")) %>% psych::alpha() %>%
              pluck("total", "raw_alpha"),
            pss_alpha = select(.,starts_with("PSS")) %>% psych::alpha(check.keys=TRUE) %>%
              pluck("total", "raw_alpha"))


## ---------------------------------------------------------------------------------------------------------------------------------------------------------
# add alphas as extra row to the descriptives table
descriptives <- descriptives %>%
  add_row(Summary = "alpha", SES = NA, CRA = alpha$cra_alpha,HCRU = alpha$hcru_alpha,PSS = alpha$pss_alpha, CESD = alpha$cesd_alpha)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------
# make it a nicely formatted table
apa_table(descriptives)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------
income_plot<-hist(scored_data$SES,
                  main="Family income distribution",
                  xlab="family income category")


## ---------------------------------------------------------------------------------------------------------------------------------------------------------
centred_data <- scored_data %>%
  mutate(CRA_c = scale(mean_cra, center = TRUE, scale = FALSE),
         SES_c = scale(SES, center = TRUE, scale = FALSE),
         PSS_c = scale(mean_pss, center = TRUE, scale = FALSE),
         HCRU_c = scale(mean_hcru, center = TRUE, scale = FALSE))


## ---------------------------------------------------------------------------------------------------------------------------------------------------------
#CRA and SES individually and the interaction between both
fit <- lm(sum_cesd ~ PSS_c + CRA_c + SES_c + CRA_c:SES_c, data = centred_data)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------
check_model(fit)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------
summary(fit)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------
# Fit 
# prettier summary with partial correlations
summ(fit, confint = TRUE, part.corr = TRUE)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------
interactions::sim_slopes(fit, pred = CRA_c, modx=SES_c,modxvals = NULL, jnalpha = 0.05, digits = 3, n.sd = 1, jnplot = TRUE, confint = TRUE)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------
# create interaction plot
interaction_plot <- interactions::interact_plot(model = fit,
                                                pred = CRA_c,
                                                modx = SES_c,
                                                interval=TRUE, 
                                                x.label = "Cognitive Reappraisal Ability",
                                                y.label= "Depressive Symptoms",
                                                legend.main = "family income")

interaction_plot


## ---------------------------------------------------------------------------------------------------------------------------------------------------------
# HCRU
fit_2 <- lm(sum_cesd ~ PSS_c + CRA_c + SES_c + CRA_c:SES_c + HCRU_c, data = centred_data)
summ(fit_2, confint = TRUE, part.corr = TRUE)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------
# Age
fit_3 <- lm(sum_cesd ~ PSS_c + CRA_c + SES_c + CRA_c:SES_c + Age, data = centred_data)
summ(fit_3, confint = TRUE, part.corr = TRUE)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------
interactions::sim_slopes(fit_3, pred = CRA_c, modx=SES_c,modxvals = NULL, 
                         jnalpha = 0.05, digits = 3, n.sd = 1, jnplot = TRUE, confint = TRUE)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------
# create interaction plot
interaction_plot_3 <- interactions::interact_plot(model = fit_3,
                                                pred = CRA_c,
                                                modx = SES_c,
                                                interval=TRUE, 
                                                x.label = "Cognitive Reappraisal Ability",
                                                y.label= "Depressive Symptoms",
                                                legend.main = "family income")

interaction_plot_3


## ---------------------------------------------------------------------------------------------------------------------------------------------------------
# Gender
fit_4 <- lm(sum_cesd ~ PSS_c + CRA_c + SES_c + CRA_c:SES_c + gender, data = centred_data)
summ(fit_4, confint = TRUE, part.corr = TRUE)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------
# Education
fit_5 <- lm(sum_cesd ~ PSS_c + CRA_c + SES_c + CRA_c:SES_c + Edu, data = centred_data)
summ(fit_5, confint = TRUE, part.corr = TRUE)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------
# Troy et al. (2017) also modeled one regression with race, however, the current study collected data from Iran without asking question about race. This is discussed in detail in the paper.


## ---------------------------------------------------------------------------------------------------------------------------------------------------------
# Troy et al. (2017) also modeled one regression without controlling for life stress (PSS)
fit_6 <- lm(sum_cesd ~ CRA_c + SES_c + CRA_c:SES_c, data = centred_data)

summ(fit_6, confint = TRUE, part.corr = TRUE)

