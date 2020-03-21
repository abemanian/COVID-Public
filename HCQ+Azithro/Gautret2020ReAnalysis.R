library(survminer)
library(survival)
library(dplyr)

Gautret2020 <- read.csv('Gautret2020Data.csv')
#Combined var, 0 is control, 1 is HCq only, 2 is HCq + Azi
Gautret2020$HCqAziComb <- Gautret2020$HCq + Gautret2020$Azi 

sfitComb <- survfit(Surv(T,Y) ~ HCqAziComb, Gautret2020)
sfitHCqVsControl <- survfit(Surv(T,Y) ~ HCq, Gautret2020)
sfitHCqAzi <- survfit(Surv(T,Y) ~ Azi, filter(Gautret2020,HCq==1))
sfitComb_AdultsOnly <- survfit(Surv(T,Y) ~ HCqAziComb,filter(Gautret2020,Age > 17))

ggsurvplot(sfitComb,Gautret2020,risk.table = TRUE, conf.int = TRUE, pval = TRUE,conf.int.style="step",
           ylab="Proportion of Patients With Positive Test",xlab="Time (Days)",legend.labs=c("No Treatment","HCq Only","HCq + Azi"))
ggsurvplot(sfitHCqVsControl,Gautret2020,risk.table = TRUE, conf.int = TRUE, pval = TRUE,conf.int.style="step",
           ylab="Proportion of Patients With Positive Test",xlab="Time (Days)",legend.labs=c("No Treatment","HCq Only"))
ggsurvplot(sfitHCqAzi,filter(Gautret2020,HCq==1),risk.table = TRUE, conf.int = TRUE, pval = TRUE,conf.int.style="step",
           ylab="Proportion of Patients With Positive Test",xlab="Time (Days)",legend.labs=c("HCq Only","HCq + Azi"))
ggsurvplot(sfitComb_AdultsOnly,filter(Gautret2020,Age > 17),risk.table = TRUE, conf.int = TRUE, pval = TRUE,conf.int.style="step",
           ylab="Proportion of Patients With Positive Test",xlab="Time (Days)",legend.labs=c("No Treatment","HCq Only","HCq + Azi"))
