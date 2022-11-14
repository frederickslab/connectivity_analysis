#install.packages("ggpubr")
library(tidyverse)
library(ggplot2)
library(ggpubr)

#Demographics
demographics = read.csv("hcp-a_n595.csv")

summary(demographics$interview_age/12)
summary(demographics$sex)
summary(demographics$race)
summary(demographics$ethnic_group)
summary(demographics$site)
summary(demographics$hcp_handedness_score)
summary(demographics$handedness)

male1 <- subset(demographics, sex=="M")
female1 <- subset(demographics, sex=="F")

summary(male1$interview_age/12)
summary(male1$sex)
summary(male1$race)
summary(male1$ethnic_group)
summary(male1$site)
summary(male1$hcp_handedness_score)
summary(male1$handedness)

summary(female1$interview_age/12)
summary(female1$sex)
summary(female1$race)
summary(female1$ethnic_group)
summary(female1$site)
summary(female1$hcp_handedness_score)
summary(female1$handedness)

hist(demographics$interview_age/12, xlab="Age (years)", ylab="Number of subjects", main="Age Distribution", xlim=c(30,100), ylim=c(0,100))

#-------------

#Normality tests
connectivity_ravlt = read.csv("connectivity_ravlt.csv")
hist(connectivity_ravlt$ravlt_total, xlab="RAVLT sum of 1-5", ylab="Number of subjects")
shapiro.test(connectivity_ravlt$ravlt_L)
#above is corrected RAVLT

personality_emotion = read.csv("connectivity_personality_emotion.csv")
hist(personality_emotion$Neuroticism, xlab="Neuroticism", ylab="Number of subjects")
shapiro.test(personality_emotion$Neuroticism)

connectivity_moca = read.csv("connectivity_moca.csv")
hist(connectivity_moca$moca_total, xlab="MOCA total", ylab="Number of subjects")
shapiro.test(connectivity_moca$moca_total)


#-------------
#RAVLT correlations

connectivity_ravlt = read.csv("connectivity_ravlt.csv")

#RAVLT x LPCC:LAngGyr
ggscatter(connectivity_ravlt, x = "ravlt_L", y = "ROI28_Langulargyrus",
          add = "reg.line", conf.int = TRUE, add.params = list(color="black"),
          cor.coef = TRUE, cor.method = "spearman", cor.coef.size = 5,
          xlab = "RAVLT Sum of Trials 1-5", ylab = "Left PCC:Left AG Connectivity", 
          palette=c("red", "blue"), color = "sex", size = 1.5) +
  font("xlab", size=20) + font("ylab", size=20) +
  ggtitle("RAVLT vs. Left PCC:Left AG Connectivity") + font("title", size=20) + theme(plot.title = element_text(hjust = 0.5))

#RAVLT x LPCC:RAngGyr
ggscatter(connectivity_ravlt, x = "ravlt_L", y = "ROI27_Rangulargyrus",
          add = "reg.line", conf.int = TRUE, add.params = list(color="black"),
          cor.coef = TRUE, cor.method = "spearman", cor.coef.size = 5,
          xlab = "RAVLT Sum of Trials 1-5", ylab = "Left PCC:Right AG Connectivity",
          palette=c("red", "blue"), color = "sex", size = 1.5) + 
  font("xlab", size=20) + font("ylab", size=20) +
  ggtitle("RAVLT vs. Left PCC:Right AG Connectivity") + font("title", size=20) + theme(plot.title = element_text(hjust = 0.5))

#RAVLT x LPCC:Lmesialtemporal
ggscatter(connectivity_ravlt, x = "ravlt_L", y = "ROI9_Lmesialtemporal",
          add = "reg.line", conf.int = TRUE, add.params = list(color="black"),
          cor.coef = TRUE, cor.method = "spearman", cor.coef.size = 5,
          xlab = "RAVLT Sum of Trials 1-5", ylab = "Left PCC:Left PHG Connectivity", 
          palette=c("red", "blue"), color = "sex", size=1.5) +
  font("xlab", size=20) + font("ylab", size=20) +
  ggtitle("RAVLT vs. Left PCC:Left PHG Connectivity") + font("title", size=20) + theme(plot.title = element_text(hjust = 0.5))

#RAVLT x LPCC:Rmesialtemporal
ggscatter(connectivity_ravlt, x = "ravlt_L", y = "ROI8_Rmesialtemporal",
          add = "reg.line", conf.int = TRUE, add.params = list(color="black"),
          cor.coef = TRUE, cor.method = "spearman", cor.coef.size = 5,
          xlab = "RAVLT Sum of Trials 1-5", ylab = "Left PCC:Right PHG Connectivity",
          palette=c("red", "blue"), color = "sex", size = 1.5) + 
  font("xlab", size=20) + font("ylab", size=20) +
  ggtitle("RAVLT vs. Left PCC:Right PHG Connectivity") + font("title", size=20) + theme(plot.title = element_text(hjust = 0.5))

#-------------
#NEUROTICISM correlations

personality_emotion = read.csv("connectivity_personality_emotion.csv")

#NEUROTICISM x LPCC:RsuperiorTemporal
ggscatter(personality_emotion, x = "Neuroticism", y = "ROI17_Rsuperiortemporal",
          add = "reg.line", conf.int = TRUE, add.params = list(color="black"),
          cor.coef = TRUE, cor.method = "spearman", cor.coef.size = 5,
          xlab = "Neuroticism", ylab = "Left PCC:Right STS Connectivity", 
          palette=c("red","blue"), color="sex", size=1.5) +
  font("xlab", size=20) + font("ylab", size=20) +
  ggtitle("Neuroticism vs. Left PCC:Right STS Connectivity") + font("title", size=20) + theme(plot.title = element_text(hjust = 0.5))

#NEUROTICISM x LPCC:Linsula
ggscatter(personality_emotion, x = "Neuroticism", y = "ROI7_Linsula",
          add = "reg.line", conf.int = TRUE, add.params = list(color="black"),
          cor.coef = TRUE, cor.method = "spearman", cor.coef.size = 5,
          xlab = "Neuroticism", ylab = "Left PCC:Left Insula Connectivity", 
          palette=c("red","blue"), color="sex", size=1.5) +
  font("xlab", size=20) + font("ylab", size=20) +
  ggtitle("Neuroticism vs. Left PCC:Left Insula Connectivity") + font("title", size=20) + theme(plot.title = element_text(hjust = 0.5))

#-------------