## -------------------------------------------------------------
## This script is used to generate the table for GLM models 
##
## Wanwan 
## 2022-10-31
## -------------------------------------------------------------

## setup -----------
rm(list = ls());gc()
require("ggplot2")
require("dplyr")
require("reshape2")
require("MASS")
require("pscl")
#require("xlsx")
#options(error = recover, warn = 2)

## data pre-processing --------
## for Ravlt data 
dat <- read.csv("./data/connectivity_ravlt.csv")
# modify columns if needed
datRavlt <- data.frame("subj_id"= dat$src_subject_id)
datRavlt$age <- dat$interview_age/12
datRavlt$sex_F <- ifelse(dat$sex == "F",1,0)
datRavlt$sexAge <- datRavlt$sex_F * datRavlt$age
datRavlt$'RAVLT_Sum_of_Trials_1_5' = dat$ravlt_L
datRavlt$'RAVLT_Immediate_Recall' = dat$trial6
datRavlt$'Left_PCC_Left_AG_Connectivity' = dat$ROI28_Langulargyrus
datRavlt$'Left_PCC_Left_PHG_Connectivity' = dat$ROI9_Lmesialtemporal
# drop NA
datRavlt <- datRavlt[complete.cases(datRavlt),]

## for Neuroticism data
dat <- read.csv("./data/connectivity_personality_emotion.csv")
# modify columns if needed
datNeuro <- data.frame("subj_id"= dat$src_subject_id)
datNeuro$age <- dat$interview_age/12
datNeuro$sex_F <- ifelse(dat$sex == "F",1,0)
datNeuro$sexAge <- datNeuro$sex_F * datNeuro$age
datNeuro$'Neuroticism' = dat$Neuroticism
datNeuro$'Left_PCC_Right_STS_Connectivity' = dat$ROI17_Rsuperiortemporal
datNeuro$'Left_PCC_Left_Insula_Connectivity' = dat$ROI7_Linsula
# drop NA
datNeuro <- datNeuro[complete.cases(datNeuro),]


## set formulas -------------------------------------
myF <- function(type){
  myFormula <- " "
  if(type == "1a"){
    myFormula <- "RAVLT_Sum_of_Trials_1_5 ~ sex_F + age + sexAge + Left_PCC_Left_AG_Connectivity"
  }else if(type == "1b"){
    myFormula <- "RAVLT_Sum_of_Trials_1_5 ~ age + Left_PCC_Left_AG_Connectivity"
  }else if(type == "1c"){
    myFormula <- "RAVLT_Sum_of_Trials_1_5 ~ sex_F + Left_PCC_Left_AG_Connectivity + offset(log(age)) "
  }else if(type == "2a"){
    myFormula <- "RAVLT_Sum_of_Trials_1_5 ~ sex_F + age + sexAge + Left_PCC_Left_PHG_Connectivity"
  }else if(type == "2b"){
    myFormula <- "RAVLT_Sum_of_Trials_1_5 ~ age + Left_PCC_Left_PHG_Connectivity"
  }else if(type == "2c"){
    myFormula <- "RAVLT_Sum_of_Trials_1_5 ~ sex_F + Left_PCC_Left_PHG_Connectivity + offset(log(age))"
  }else if(type == "3a"){
    myFormula <- "Neuroticism ~ sex_F + age + sexAge + Left_PCC_Right_STS_Connectivity"
  }else if(type == "3b"){
    myFormula <- "Neuroticism ~ age + Left_PCC_Right_STS_Connectivity"
  }else if(type == "3c"){
    myFormula <- "Neuroticism ~ sex_F + Left_PCC_Right_STS_Connectivity + offset(log(age)) "
  }else if(type == "4a"){
    myFormula <- "Neuroticism ~ sex_F + age + sexAge + Left_PCC_Left_Insula_Connectivity"
  }else if(type == "4b"){
    myFormula <- "Neuroticism ~ age + Left_PCC_Left_Insula_Connectivity"
  }else if(type == "4c"){
    myFormula <- "Neuroticism ~ sex_F + Left_PCC_Left_Insula_Connectivity + offset(log(age))"
  }else if(type == "5a"){
    myFormula <- "RAVLT_Immediate_Recall ~ sex_F + age + sexAge + Left_PCC_Left_AG_Connectivity"
  }else if(type == "5b"){
    myFormula <- "RAVLT_Immediate_Recall ~ age + Left_PCC_Left_AG_Connectivity"
  }else if(type == "5c"){
    myFormula <- "RAVLT_Immediate_Recall ~ sex_F + Left_PCC_Left_AG_Connectivity + offset(log(age))"
  }else if(type == "6a"){
    myFormula <- "RAVLT_Immediate_Recall ~ sex_F + age + sexAge + Left_PCC_Left_PHG_Connectivity"
  }else if(type == "6b"){
    myFormula <- "RAVLT_Immediate_Recall ~ age + Left_PCC_Left_PHG_Connectivity"
  }else if(type == "6c"){
    myFormula <- "RAVLT_Immediate_Recall ~ sex_F+ Left_PCC_Left_PHG_Connectivity + offset(log(age))"
  }
  myFormula
}
#formula_list <- paste(rep(c(1:6), each = 3), c("a","b","c"), sep = "")
formula_list <- paste(rep(c(1:6), each = 2), c("a","b"), sep = "")
nF <- length(formula_list)

## run model -----------------
# res <- data.frame("Response" = rep("",nF),
#                   "sex" = rep(c(1,0,1),6),
#                   "age" = rep(c(1,1,0),6),
#                   "sex*Age" = rep(c(1,0,0),6),
#                   "offset(age)" = rep(c(0,0,1),6),
#                   "Formula" = rep("",nF),
#                   "Connectivity_pvalue" = rep(0,nF),
#                   "AIC" = rep(0,nF))
res <- data.frame("Response" = rep("",nF),
                  "sex" = rep(c(1,0),6),
                  "age" = rep(c(1,1),6),
                  "sex*Age" = rep(c(1,0),6),
                  "offset(age)" = rep(c(0,0),6),
                  "Formula" = rep("",nF),
                  "Connectivity_pvalue" = rep(0,nF),
                  "AIC" = rep(0,nF))
for(i in 1:nF){
  f <- formula_list[i]
  myFormula <- as.formula(myF(f))
  # which data 
  if(strsplit(f,"")[[1]][1] %in% c("3","4")){
    dat4model <- datNeuro
  }else{
    dat4model <- datRavlt
  }
  # which model
  if(strsplit(f,"")[[1]][1] %in% c("5","6")){
    myModel <- glm(myFormula, data = dat4model, family = poisson)
  }else{
    myModel <- glm.nb(myFormula, data = dat4model)
  }
  myS <- summary(myModel)
  res$Response[i] <- as.character(myS$terms[[2]])
  res$Formula[i] <- myF(f)
  res$Connectivity_pvalue[i] <- round(myS$coefficients[grep("Left_",row.names(myS$coefficients)),4],4)
  res$AIC[i] <- myS$aic
  #res$twologLik[i] <- myS$twologlik
  print(f)
}
res$Connectivity_sig <- ifelse(res$Connectivity_pvalue <= 0.05,1,0)
res <- res[,c(1,2,3,4,5,7,9,8,6)]
write.csv(res, file = "GLM_reuslts.csv",row.names = FALSE, quote=FALSE)


## run linear and anova ------------
## generate standardized version for both data
datRavlt_stand <-  datRavlt[,2:8]
for(j in 1:ncol(datRavlt_stand)){
  col <- datRavlt_stand[,j]
  datRavlt_stand[,j] <- (col - mean(col))/ sd(col)
}
datNeuro_stand <-  datNeuro[,2:7]
for(j in 1:ncol(datNeuro_stand)){
  col <- datNeuro_stand[,j]
  datNeuro_stand[,j] <- (col - mean(col))/ sd(col)
}

## run model 
require("car")
formula_list <- paste(rep(c(1,2,5,6,3,4), each = 2), c("a","b"), sep = "")
nF <- length(formula_list)
res <- data.frame("Response" = rep("",nF),
                  "coef" = rep(0,nF),
                  "pvalue" = rep(0,nF),
                  "adj_r2" = rep(0,nF),
                  #"anova_pvalue" = rep(0,nF),
                  "Formula" = rep("",nF))
for(i in 1:nF){
  f <- formula_list[i]
  myFormula <- as.formula(myF(f))
  # which data 
  if(strsplit(f,"")[[1]][1] %in% c("3","4")){
    dat4model <- datNeuro_stand
  }else{
    dat4model <- datRavlt_stand
  }
  
  
  # which model
  myModel <- lm(myFormula, data = dat4model)
  myS <- summary(myModel)

  res$Response[i] <- as.character(myS$terms[[2]])
  res$Formula[i] <- myF(f)
  res$coef[i] <- round(myS$coefficients[grep("Left_",row.names(myS$coefficients)),1],4)
  res$pvalue[i] <- round(myS$coefficients[grep("Left_",row.names(myS$coefficients)),4],4)
  res$adj_r2[i] <- round(myS$adj.r.squared,4)
  #res$twologLik[i] <- myS$twologlik
  
  print(f)
}
res$sig <- ifelse(res$pvalue <= 0.05,1,0)
#res <- res[,c(1,2,3,4,5,7,9,8,6)]
write.csv(res, file = "LM_reuslts.csv",row.names = FALSE, quote=FALSE)

