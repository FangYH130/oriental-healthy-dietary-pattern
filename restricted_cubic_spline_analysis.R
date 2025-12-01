library(rms) 
library(survminer)
library(ggplot2) 
library(patchwork)
library(haven)  
library(dplyr)  
library(ggplot2)
library(survival)

rm(list=ls())
setwd("E:/")
workdir <- "E:/"

data <- as.data.frame(read_sas("score.sas7bdat")) 
data$ob<-as.factor(data$ob)
data$ob<-relevel(data$ob, ref="1")

dd <- datadist(data)
options(datadist='dd')

fit_ob3 <- lrm(ob~rcs(scorea,3)+ur_gt+A1+age,data=data)
an<-anova(fit_ob3)
plot(Predict(fit_ob3, scorea,fun=exp), anova=an, pval=T)
OR<-Predict(fit_ob3, scorea,fun=exp,ref.zero = TRUE)
Pic_ob <- ggplot(OR,anova=an, pval=T)+
  geom_line(data=OR, aes(scorea,yhat),linetype=1,size=1,alpha = 0.7,colour="#0070b9")+
  geom_ribbon(data=OR, aes(scorea,ymin = lower, ymax = upper),alpha = 0.1,fill="#0070b9")+
  geom_vline(aes(xintercept=5.0), colour="#d40e8c", linetype=2, size=0.7)+
  geom_hline(yintercept=1, colour="grey", linetype=2, size=0.7)+
  theme_classic()+
  labs(title = "Obesity", x="Score", y="OR (95%CI)")+
  scale_x_continuous(limits = NULL,
                     breaks = c(1,2,3,4,5,6,7,8,9,10))+
  scale_y_continuous(limits = c(0, 1.6),
                     breaks = c(0,0.2,0.4,0.6,0.8,1.0,1.2,1.4,1.6))+
  geom_text(aes(x=5,y=1,label='Score=5.00'),
            vjust=-1.5,hjust=-0.1,size=4)

data$overweight<-as.factor(data$overweight)
data$overweight<-relevel(data$overweight, ref="1")
fit_overweight3 <- lrm(overweight~rcs(scorea,3)+ur_gt+A1+age,data=data)
an<-anova(fit_overweight3)
plot(Predict(fit_overweight3, scorea,fun=exp), anova=an, pval=T)
OR<-Predict(fit_overweight3, scorea,fun=exp,ref.zero = TRUE)
Pic_overweight <- ggplot(OR,anova=an, pval=T)+
  geom_line(data=OR, aes(scorea,yhat),linetype=1,size=1,alpha = 0.7,colour="#0070b9")+
  geom_ribbon(data=OR, aes(scorea,ymin = lower, ymax = upper),alpha = 0.1,fill="#0070b9")+
  geom_vline(aes(xintercept=5.34), colour="#d40e8c", linetype=2, size=0.7)+
  geom_hline(yintercept=1, colour="grey", linetype=2, size=0.7)+
  theme_classic()+
  labs(title = "Overweight and Obesity", x="Score", y="OR (95%CI)")+
  scale_x_continuous(limits = NULL,
                     breaks = c(1,2,3,4,5,6,7,8,9,10))+
  scale_y_continuous(limits = c(0, 1.6),
                     breaks = c(0,0.2,0.4,0.6,0.8,1.0,1.2,1.4,1.6))+
  geom_text(aes(x=5.34,y=1,label='Score=5.34'),
            vjust=-1.5,hjust=-0.1,size=4)

data$hypertension<-as.factor(data$hypertension)
data$hypertension<-relevel(data$hypertension, ref="1")
fit_hypertension5 <- lrm(hypertension~rcs(scorea,5)+ur_gt+A1+age,data=data)
an<-anova(fit_hypertension5)
plot(Predict(fit_hypertension5, scorea,fun=exp), anova=an, pval=T)
OR<-Predict(fit_hypertension5, scorea,fun=exp,ref.zero = TRUE)
Pic_hypertension <- ggplot(OR,anova=an, pval=T)+
  geom_line(data=OR, aes(scorea,yhat),linetype=1,size=1,alpha = 0.7,colour="#0070b9")+
  geom_ribbon(data=OR, aes(scorea,ymin = lower, ymax = upper),alpha = 0.1,fill="#0070b9")+
  geom_vline(aes(xintercept=6.45), colour="#d40e8c", linetype=2, size=0.7)+
  geom_hline(yintercept=1, colour="grey", linetype=2, size=0.7)+
  theme_classic()+
  labs(title = "Hypertension", x="Score", y="OR (95%CI)")+
  scale_x_continuous(limits = NULL,
                     breaks = c(1,2,3,4,5,6,7,8,9,10))+
  scale_y_continuous(limits = c(0, 1.4),
                     breaks = c(0,0.2,0.4,0.6,0.8,1.0,1.2,1.4))+
  geom_text(aes(x=6.5,y=1,label='Score=6.45'),
            vjust=-1.5,hjust=-0.1,size=4)
