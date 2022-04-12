
# Topic: Structural equation modelling for CREP-21-29
# Date: 

# Set seed
set.seed(79)

# Load packages
pacman::p_load(pacman, tidyverse, psych, lavaan, dplyr, semPlot, readxl, rmarkdown)

# Read data
analyze_data <- read_csv("/Users/Pinocchio/R/Thesis_git_repository/Data_analysis/Data/C_Imp_scored.csv")

describe(cbind(analyze_data[,5:8],analyze_data[,70:89]))

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
# Model base on Hayes (2013), model 19 (page24)
# http://dm.darden.virginia.edu/ResearchMethods/Templates.pdf


# Lavaan Syntax including conditional effects
Mod.Med.Model1 <- "
# Mediator
sum_neg ~  a*X + b2*W + b3*Z + b4*W.Z + d1*COV 

# Outcome being predicted by the model
Y ~ c1 * X + c2*W + c3*Z + c4*X.W + c5*X.Z + c6*W.Z + c7*X.W.Z + b1*sum_neg + d*COV

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
Mod.Med.fit1 <- sem(model = Mod.Med.Model1,
                       meanstructure = TRUE,
                       data = analyze_data,
                       se = "bootstrap",
                       meanstructure = TRUE,
                       bootstrap = 1000)

# Lavaan Syntax excluding conditional effects
Mod.Med.Model2 <-
"
# Mediator
sum_neg ~  a*X + b2*W + b3*Z + b4*W.Z + d1*COV 

# Outcome being predicted by the model
Y ~ c1 * X + c2*W + c3*Z + c4*X.W + c5*X.Z + c6*W.Z + c7*X.W.Z + b1*sum_neg + d*COV

# Effects
indirect := a * (b1 + b2 + b3 + b4)
direct:= c1 + c4 + c5 + c7

# Total effect
total := direct + indirect
"

Mod.Med.fit2 <- sem(model = Mod.Med.Model2,
                       meanstructure = TRUE,
                       data = analyze_data,
                       se = "bootstrap",
                       meanstructure = TRUE,
                       bootstrap = 1000)

# Summary
summary(Mod.Med.fit1, fit.measures = TRUE, standardize = TRUE, rsquare = TRUE)

# Parameter estimates
parameterEstimates(Mod.Med.fit1, remove.nonfree = TRUE, ci = TRUE, 
                   boot.ci.type = "perc", pvalue = TRUE, output = "pretty")

# Model plot
plot = semPaths(Mod.Med.fit1, fixedStyle = 2, layout = "tree2", whatLabels = "est",
                intercepts = F, label.scale=T, nCharNodes = 4,
                sizeMan2=4, sizeMan=4.5, asize=1.5, residuals = T, exoCov = F)

# probed Covariances table
cov_table <- data.frame(vcov(Mod.Med.fit1))
write.csv(cov_table , "C_imp_neg_cov.csv", row.names = T) # Save covariance matrix