
# Topic: C. Structural equation modelling for CREP-21-29
# Date: April, 2022

# Set seed
set.seed(79)

# Load packages
pacman::p_load(pacman, tidyverse, psych, lavaan, dplyr, semPlot, readxl, rmarkdown)

data_path = "scored_data"
# Read data
scored_data <- read_csv(data_path)

describe(scored_data[,-c(1:69)])

# mean centre independent and moderator for interaction terms
centred_data <- scored_data %>%
  mutate(cra_c = scale(mean_cra, center = TRUE, scale = FALSE),
         ses_c = scale(SES, center = TRUE, scale = FALSE),
         bsm_c = scale(mean_bsm, center = TRUE, scale = FALSE),
         csc_c = scale(sum_csc, center = TRUE, scale = FALSE))


df <- cbind(centred_data["SES"],centred_data[ , c(70:84)])

# Rename data for modelling ease in lavaan
df <- df %>% mutate(rename(df,
                           "X" = "cra_c",
                           "Y" = "sum_cesd",
                           "W" = "ses_c",
                           "Z" = "bsm_c",
                           "COV" = "mean_pss",
                           "M" = "csc_c"))

# Create interaction terms
df <- df %>% mutate(X.W = X * W,
                    X.Z = X * Z,
                    W.Z = W * Z,
                    X.W.Z = X * W * Z,
                    M.W = M * W,
                    M.Z = M* Z,
                    M.W.Z = M * W * Z)

write.csv(df,"SEM_ready.csv")

# Variables guide

# Meta-cogitions Questionnaire subscales**
  
# pos: positive beliefs about worry
# neg: negative beliefs about un-controllability of worry
# cc: cognitive confidence
# nc: need for control
# csc: cognitive self-consciences
# replace the mediator with appropriate variable everytime you run the code.
  
  
# Variables description
# X: independent variable // cognitive reappraisal ability (CRA)
# Y: dependent variable // depressive symptoms (CES-D-5)
# M: mediator // negative beliefs about uncontrollabiity of worry (NEG)
# W: first moderator // socioecnomic status (SES)
# Z: second moderator // beliefs about socioeconomic mobility (BSM)
# X.W, X.Z, ....: are interaction terms
# COV: covariate // Life stress (PSS)
# All variables are manifest and continuous.

# Model based on Hayes (2013), model 19 (page24)
# http://dm.darden.virginia.edu/ResearchMethods/Templates.pdf

# Algebra based on: Stride C.B., Gardner S., Catley. N. & Thomas, F.(2015) 'Mplus code for the mediation, moderation, and moderated mediation model templates from Andrew Hayes' PROCESS analysis examples' , http://www.figureitout.org.uk

# Y = b0 + b1M + b2MV + b3MQ + b4MVQ + c1'X + c2'V + c3'Q + c4'XV + c5'XQ + c6'VQ + c7'XVQ
# M = a0 + a1X

# Lavaan Syntax including conditional effects
Mod.Med.Model <- "
# Mediator
M ~ a*X

# Outcome being predicted by the model
Y ~ c1 * X + c2*W + c3*Z + c4*X.W + c5*X.Z + c6*W.Z + c7*X.W.Z + b1*M + b2*M.W + b3*M.Z + b4*M.W.Z

# Effects
indirect := a * (b1 + b2 + b3 + b4)
direct:= c1 + c4 + c5 + c7

# Total effect
total := direct + indirect

# Intercepts and variance for conditional effects analysis
W ~ W.mean*1
Z ~ Z.mean*1

# variances for conditional effects analysis
W ~~ W.var * W
Z ~~ Z.var * Z

# Conditional indirect effect moderated by W and Z: a * (b1 + b2 + b3 + b4)
indirect.LwLz:= a * (b1 + b2 * (W.mean - sqrt(W.var)) + b3 * (Z.mean - sqrt(Z.var)) + b4 * (W.mean - sqrt(W.var)) * (Z.mean - sqrt(Z.var)))
indirect.LwHz:= a * (b1 + b2 * (W.mean - sqrt(W.var)) + b3 * (Z.mean + sqrt(Z.var)) + b4 * (W.mean - sqrt(W.var)) * (Z.mean + sqrt(Z.var)))
indirect.HwLz:= a * (b1 + b2 * (W.mean + sqrt(W.var)) + b3 * (Z.mean - sqrt(Z.var)) + b4 * (W.mean + sqrt(W.var)) * (Z.mean - sqrt(Z.var)))
indirect.HwHz:= a * (b1 + b2 * (W.mean + sqrt(W.var)) + b3 * (Z.mean + sqrt(Z.var)) + b4 * (W.mean + sqrt(W.var)) * (Z.mean + sqrt(Z.var)))

# Conditional direct effect moderated by W and Z: c1 + c4 + c5 + c7
direct.LwLz:= c1 + c4*(W.mean - sqrt(W.var)) + c5*(Z.mean - sqrt(Z.var)) + c7 * (W.mean - sqrt(W.var)) * (Z.mean - sqrt(Z.var))
direct.LwHz:= c1 + c4*(W.mean - sqrt(W.var)) + c5*(Z.mean + sqrt(Z.var)) + c7 * (W.mean - sqrt(W.var)) * (Z.mean + sqrt(Z.var))
direct.HwLz:= c1 + c4*(W.mean + sqrt(W.var)) + c5*(Z.mean - sqrt(Z.var)) + c7 * (W.mean + sqrt(W.var)) * (Z.mean - sqrt(Z.var))
direct.HwHz:= c1 + c4*(W.mean + sqrt(W.var)) + c5*(Z.mean + sqrt(Z.var)) + c7 * (W.mean + sqrt(W.var)) * (Z.mean + sqrt(Z.var))
"
Mod.Med.fit <- sem(model = Mod.Med.Model,
                       meanstructure = TRUE,
                       data = df,
                       se = "bootstrap",
                       bootstrap = 1000)

# Summary
summary(Mod.Med.fit, fit.measures = TRUE, standardize = TRUE, rsquare = TRUE)

# Parameter estimates
parameterEstimates(Mod.Med.fit, remove.nonfree = TRUE, ci = TRUE, 
                   boot.ci.type = "perc", pvalue = TRUE, output = "pretty")

# Model plot
plot = semPaths(Mod.Med.fit, fixedStyle = 2, layout = "tree2", whatLabels = "est",
                intercepts = F, label.scale=T, nCharNodes = 4,
                sizeMan2=4, sizeMan=4.5, asize=1.5, residuals = T, exoCov = F)

# probed Covariances table
cov_table <- data.frame(fitted(Mod.Med.fit.pos))
write.csv(cov_table , "cov_matrix.pos.csv", row.names = T) # Save covariance matrix
