library(haven)
library(survival)
surv_fit1 <- coxph(Surv(tstart, tstop, event) ~ dofang_score + age +gender+urban+timein, 
                  data = death_coxinter, 
                  x = TRUE,
                  model=TRUE)
death_coxinter$dofang_group3 <- factor(
  ifelse(death_coxinter$dofang_score <= 4.5, 1,
         ifelse(death_coxinter$dofang_score <= 5.5, 2,3)
  
),
levels = 1:3,  
ordered = TRUE  
)

contrasts(death_coxinter$dofang_group3) <- contr.treatment(3, base = 1)
surv_fit2 <- coxph(Surv(tstart, tstop, event) ~ dofang_group3 + age +gender+urban+timein, 
                   data = death_coxinter, 
                   x = TRUE,
                   model=TRUE)


library(survival)
library(rms)
library(survminer)
library(ggplot2)
death_coxinter1 <- death_coxinter[,c("dofang_score","tstart","tstop","event",
                                     "age","gender","urban","timein")]
dd <- datadist(death_coxinter1)
options(datadist = "dd")

f <- Surv(tstart, tstop, event) ~ rcs(dofang_score, 3) + age + gender + urban + timein
fit_rcs <- cph(f, data = death_coxinter1, x = TRUE, y = TRUE, surv = TRUE)
