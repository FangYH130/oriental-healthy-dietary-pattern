library(rms) #RCS
library(survminer)#曲线
library(ggplot2) #作图
library(patchwork)
library(haven)  # 用于读取 SAS 数据集
library(dplyr)  # 用于数据处理
library(ggplot2)
library(survival)

rm(list=ls())
setwd("E:/2. 项目工作/1. 东方膳食模式评分/10分制/2025.8.19 补充分析")
workdir <- "E:/2. 项目工作/1. 东方膳食模式评分/10分制/2025.8.19 补充分析"

#######肥胖#######
data <- as.data.frame(read_sas("compare.sas7bdat")) 
data <- na.omit(data[, c("ivqid", "ob", "overweight", "hp", "hypertension", "scorea", "ur_gt","A1", "age", "med")])
data$ob<-as.factor(data$ob)
data$ob<-relevel(data$ob, ref="BMI<28")

#接着为后续程序设定数据环境，也就是打包数据，这一步在预测模型中也常做
dd <- datadist(data)
options(datadist='dd')

#拟合logistic回归的限制性立方样条
fit_ob3 <- lrm(ob~rcs(scorea,3)+ur_gt+A1+age,data=data)
fit_ob4 <- lrm(ob~rcs(scorea,4)+ur_gt+A1+age,data=data)
fit_ob5 <- lrm(ob~rcs(scorea,5)+ur_gt+A1+age,data=data)

AIC(fit_ob3)
AIC(fit_ob4)
AIC(fit_ob5)

an<-anova(fit_ob3)

plot(Predict(fit_ob3, scorea,fun=exp), anova=an, pval=T)

OR<-Predict(fit_ob3, scorea,fun=exp,ref.zero = TRUE)

Pic_ob_o <- ggplot(OR,anova=an, pval=T)+
  geom_line(data=OR, aes(scorea,yhat),linetype=1,size=1,alpha = 0.7,colour="#0070b9")+
  geom_ribbon(data=OR, aes(scorea,ymin = lower, ymax = upper),alpha = 0.1,fill="#0070b9")+
  geom_vline(aes(xintercept=5.0), colour="#d40e8c", linetype=2, size=0.7)+
  geom_hline(yintercept=1, colour="grey", linetype=2, size=0.7)+
  theme_classic()+
  labs(title = "Obesity", x="Score", y="OR (95%CI)")+
  scale_x_continuous(limits = c(0, 10),
                     breaks = c(1,2,3,4,5,6,7,8,9,10))+
  scale_y_continuous(limits = c(0, 1.4),
                     breaks = c(0,0.2,0.4,0.6,0.8,1.0,1.2,1.4))+
  geom_text(aes(x=5,y=1,label='Score=5'),
            vjust=-1.5,hjust=-0.1,size=4)

Pic_ob_o


#拟合logistic回归的限制性立方样条
fit_ob3 <- lrm(ob~rcs(med,3)+ur_gt+A1+age,data=data)
fit_ob4 <- lrm(ob~rcs(med,4)+ur_gt+A1+age,data=data)
fit_ob5 <- lrm(ob~rcs(med,5)+ur_gt+A1+age,data=data)

AIC(fit_ob3)
AIC(fit_ob4)
AIC(fit_ob5)

an<-anova(fit_ob3)

plot(Predict(fit_ob3, med,fun=exp), anova=an, pval=T)

OR<-Predict(fit_ob3, med,fun=exp,ref.zero = TRUE)

Pic_ob_med <- ggplot(OR,anova=an, pval=T)+
  geom_line(data=OR, aes(med,yhat),linetype=1,size=1,alpha = 0.7,colour="#0070b9")+
  geom_ribbon(data=OR, aes(med,ymin = lower, ymax = upper),alpha = 0.1,fill="#0070b9")+
  geom_vline(aes(xintercept=3.0), colour="#d40e8c", linetype=2, size=0.7)+
  geom_hline(yintercept=1, colour="grey", linetype=2, size=0.7)+
  theme_classic()+
  labs(title = "Obesity", x="Score", y="OR (95%CI)")+
  scale_x_continuous(limits = c(0, 9),
                     breaks = c(1,2,3,4,5,6,7,8,9))+
  scale_y_continuous(limits = c(0, 1.4),
                     breaks = c(0,0.2,0.4,0.6,0.8,1.0,1.2,1.4))+
  geom_text(aes(x=3,y=1,label='Score=3'),
            vjust=-1.5,hjust=-0.1,size=4)

Pic_ob_med


#######超重#######
data$overweight<-as.factor(data$overweight)
data$overweight<-relevel(data$overweight, ref="BMI<24")

#拟合logistic回归的限制性立方样条
fit_overweight3 <- lrm(overweight~rcs(scorea,3)+ur_gt+A1+age,data=data)
fit_overweight4 <- lrm(overweight~rcs(scorea,4)+ur_gt+A1+age,data=data)
fit_overweight5 <- lrm(overweight~rcs(scorea,5)+ur_gt+A1+age,data=data)

AIC(fit_overweight3)
AIC(fit_overweight4)
AIC(fit_overweight5)

an<-anova(fit_overweight4)

plot(Predict(fit_overweight4, scorea,fun=exp), anova=an, pval=T)

OR<-Predict(fit_overweight4, scorea,fun=exp,ref.zero = TRUE)

Pic_overweight_o <- ggplot(OR,anova=an, pval=T)+
  geom_line(data=OR, aes(scorea,yhat),linetype=1,size=1,alpha = 0.7,colour="#0070b9")+
  geom_ribbon(data=OR, aes(scorea,ymin = lower, ymax = upper),alpha = 0.1,fill="#0070b9")+
  geom_vline(aes(xintercept=6.6), colour="#d40e8c", linetype=2, size=0.7)+
  geom_hline(yintercept=1, colour="grey", linetype=2, size=0.7)+
  theme_classic()+
  labs(title = "Overweight and Obesity", x="Score", y="OR (95%CI)")+
  scale_x_continuous(limits = c(0, 10),
                     breaks = c(1,2,3,4,5,6,7,8,9,10))+
  scale_y_continuous(limits = c(0, 2.0),
                     breaks = c(0,0.2,0.4,0.6,0.8,1.0,1.2,1.4))+
  geom_text(aes(x=6.7,y=1,label='Score=6.6'),
            vjust=-1.5,hjust=-0.1,size=4)

Pic_overweight_o



#拟合logistic回归的限制性立方样条
fit_overweight3 <- lrm(overweight~rcs(med,3)+ur_gt+A1+age,data=data)
fit_overweight4 <- lrm(overweight~rcs(med,4)+ur_gt+A1+age,data=data)
fit_overweight5 <- lrm(overweight~rcs(med,5)+ur_gt+A1+age,data=data)

AIC(fit_overweight3)
AIC(fit_overweight4)
AIC(fit_overweight5)

an<-anova(fit_overweight4)

plot(Predict(fit_overweight4, med,fun=exp), anova=an, pval=T)

OR<-Predict(fit_overweight4, med,fun=exp,ref.zero = TRUE)

Pic_overweight_med <- ggplot(OR,anova=an, pval=T)+
  geom_line(data=OR, aes(med,yhat),linetype=1,size=1,alpha = 0.7,colour="#0070b9")+
  geom_ribbon(data=OR, aes(med,ymin = lower, ymax = upper),alpha = 0.1,fill="#0070b9")+
  geom_vline(aes(xintercept=3), colour="#d40e8c", linetype=2, size=0.7)+
  geom_hline(yintercept=1, colour="grey", linetype=2, size=0.7)+
  theme_classic()+
  labs(title = "Overweight and Obesity", x="Score", y="OR (95%CI)")+
  scale_x_continuous(limits = c(0, 9),
                     breaks = c(1,2,3,4,5,6,7,8,9))+
  scale_y_continuous(limits = c(0, 2.0),
                     breaks = c(0,0.2,0.4,0.6,0.8,1.0,1.2,1.4))+
  geom_text(aes(x=3,y=0.94,label='Score=3'),
            vjust=-1.5,hjust=-0.1,size=4)

Pic_overweight_med


#######高血压#######
data$hypertension<-as.factor(data$hypertension)
data$hypertension<-relevel(data$hypertension, ref="1")

#拟合logistic回归的限制性立方样条
fit_hypertension3 <- lrm(hypertension~rcs(scorea,3)+ur_gt+A1+age,data=data)
fit_hypertension4 <- lrm(hypertension~rcs(scorea,4)+ur_gt+A1+age,data=data)
fit_hypertension5 <- lrm(hypertension~rcs(scorea,5)+ur_gt+A1+age,data=data)

AIC(fit_hypertension3)
AIC(fit_hypertension4)
AIC(fit_hypertension5)

an<-anova(fit_hypertension5)

plot(Predict(fit_hypertension5, scorea,fun=exp), anova=an, pval=T)

OR<-Predict(fit_hypertension5, scorea,fun=exp,ref.zero = TRUE)

Pic_hypertension_o <- ggplot(OR,anova=an, pval=T)+
  geom_line(data=OR, aes(scorea,yhat),linetype=1,size=1,alpha = 0.7,colour="#0070b9")+
  geom_ribbon(data=OR, aes(scorea,ymin = lower, ymax = upper),alpha = 0.1,fill="#0070b9")+
  geom_vline(aes(xintercept=6.45), colour="#d40e8c", linetype=2, size=0.7)+
  geom_hline(yintercept=1, colour="grey", linetype=2, size=0.7)+
  theme_classic()+
  labs(title = "Hypertension", x="Score", y="OR (95%CI)")+
  scale_x_continuous(limits = c(0, 10),
                     breaks = c(1,2,3,4,5,6,7,8,9,10))+
  scale_y_continuous(limits = c(0, 1.4),
                     breaks = c(0,0.2,0.4,0.6,0.8,1.0,1.2,1.4))+
  geom_text(aes(x=6.5,y=1,label='Score=6.45'),
            vjust=-1.5,hjust=-0.1,size=4)

Pic_hypertension_o


#拟合logistic回归的限制性立方样条
fit_hypertension3 <- lrm(hypertension~rcs(med,3)+ur_gt+A1+age,data=data)
fit_hypertension4 <- lrm(hypertension~rcs(med,4)+ur_gt+A1+age,data=data)
fit_hypertension5 <- lrm(hypertension~rcs(med,5)+ur_gt+A1+age,data=data)

AIC(fit_hypertension3)
AIC(fit_hypertension4)
AIC(fit_hypertension5)

an<-anova(fit_hypertension3)

plot(Predict(fit_hypertension3, med,fun=exp), anova=an, pval=T)

OR<-Predict(fit_hypertension3, med,fun=exp,ref.zero = TRUE)

Pic_hypertension_med <- ggplot(OR,anova=an, pval=T)+
  geom_line(data=OR, aes(med,yhat),linetype=1,size=1,alpha = 0.7,colour="#0070b9")+
  geom_ribbon(data=OR, aes(med,ymin = lower, ymax = upper),alpha = 0.1,fill="#0070b9")+
  geom_vline(aes(xintercept=3), colour="#d40e8c", linetype=2, size=0.7)+
  geom_hline(yintercept=1, colour="grey", linetype=2, size=0.7)+
  theme_classic()+
  labs(title = "Hypertension", x="Score", y="OR (95%CI)")+
  scale_x_continuous(limits = c(0, 9),
                     breaks = c(1,2,3,4,5,6,7,8,9))+
  scale_y_continuous(limits = c(0.4, 1.4),
                     breaks = c(0.4,0.6,0.8,1.0,1.2,1.4))+
  geom_text(aes(x=3,y=1,label='Score=3'),
            vjust=-1.5,hjust=-0.1,size=4)

Pic_hypertension_med



Pic_o <- (Pic_ob_o+Pic_overweight_o+Pic_hypertension_o)
Pic_med <- (Pic_ob_med+Pic_overweight_med+Pic_hypertension_med)

Pic_o
Pic_med

ggsave(Pic_ob_med, file = "E:/2. 项目工作/1. 东方膳食模式评分/10分制/2025.8.19 补充分析/地中海膳食得分和肥胖的OR值.jpg", width = 15, height = 15, units = "cm")
ggsave(Pic_overweight_med, file = "E:/2. 项目工作/1. 东方膳食模式评分/10分制/2025.8.19 补充分析/地中海膳食得分和超重OR值.jpg", width = 15, height = 15, units = "cm")
ggsave(Pic_hypertension_med, file = "E:/2. 项目工作/1. 东方膳食模式评分/10分制/2025.8.19 补充分析/地中海膳食得分和高血压的OR值.jpg", width = 15, height = 15, units = "cm")
ggsave(Pic_med, file = "E:/2. 项目工作/1. 东方膳食模式评分/10分制/2025.8.19 补充分析/地中海膳食得分和3个结局的OR值.jpg", width = 30, height = 12, units = "cm")
ggsave(Pic_o, file = "E:/2. 项目工作/1. 东方膳食模式评分/10分制/2025.8.19 补充分析/江南膳食得分和3个结局的OR值.jpg", width = 30, height = 12, units = "cm")








###############################################################
# 拟合逻辑回归模型
model_ob <- glm(ob ~ scorea+age+ur_gt+A1, data = data, family = binomial())

S <- coefficients(summary(model_ob))

scorea <- seq(min(data$scorea), max(data$scorea))
dt <- data.frame(scorea = scorea)
dt$OR <- exp(dt$scorea * S[2, 1])
dt$OR_lwr <- exp(dt$scorea * S[2, 1] - qnorm(0.975) * dt$scorea * S[2, 2])
dt$OR_upr <- exp(dt$scorea * S[2, 1] + qnorm(0.975) * dt$scorea * S[2, 2])

Pic_ob <- ggplot(data = dt, aes(x = scorea)) + 
  geom_line(aes(y = OR, color = "OR"), linetype = 1, color="#00008B") + 
  geom_line(aes(y = OR_lwr, color = ""), linetype = 0) + 
  geom_line(aes(y = OR_upr, color = ""), linetype = 0) + 
  geom_ribbon(aes(ymin = OR_lwr, ymax = OR_upr), fill = "#ADD8E6", alpha = 0.3) +
  labs(title = "", x = "得分", y = "OR值", color = "") +
  theme_minimal() +
  theme(
    legend.position = c(0.8, 0.8),
    plot.title = element_text(hjust = 0.5),  # 标题居中
    axis.text = element_text(size = 12),  # 坐标轴文本大小
    axis.title = element_text(size = 14),  # 坐标轴标题大小
    legend.text = element_text(size = 12),  # 图例文本大小
    legend.title = element_blank(),  # 去除图例标题
    panel.grid = element_blank()  # 去掉网格线
  )

Pic_ob

model_overweight <- glm(overweight ~ scorea+age+ur_gt+A1, data = data, family = binomial())
data$overweight <- ifelse(data$overweight == 0, 1, 0)

S <- coefficients(summary(model_overweight))

scorea <- seq(min(data$scorea), max(data$scorea))
dt <- data.frame(scorea = scorea)
dt$OR <- exp(dt$scorea * S[2, 1])
dt$OR_lwr <- exp(dt$scorea * S[2, 1] - qnorm(0.975) * dt$scorea * S[2, 2])
dt$OR_upr <- exp(dt$scorea * S[2, 1] + qnorm(0.975) * dt$scorea * S[2, 2])

Pic_overweight <- ggplot(data = dt, aes(x = scorea)) + 
  geom_line(aes(y = OR, color = "OR"), linetype = 1, color="#00008B") + 
  geom_line(aes(y = OR_lwr, color = ""), linetype = 0) + 
  geom_line(aes(y = OR_upr, color = ""), linetype = 0) + 
  geom_ribbon(aes(ymin = OR_lwr, ymax = OR_upr), fill = "#ADD8E6", alpha = 0.3) +
  labs(title = "", x = "得分", y = "OR值", color = "") +
  theme_minimal() +
  theme(
    legend.position = c(0.2, 0.8),
    plot.title = element_text(hjust = 0.5),  # 标题居中
    axis.text = element_text(size = 12),  # 坐标轴文本大小
    axis.title = element_text(size = 14),  # 坐标轴标题大小
    legend.text = element_text(size = 12),  # 图例文本大小
    legend.title = element_blank(),  # 去除图例标题
    panel.grid = element_blank()  # 去掉网格线
  )

Pic_overweight

model_hypertension <- glm(hypertension ~ scorea+age+ur_gt+A1, data = data, family = binomial())

S <- coefficients(summary(model_hypertension))

scorea <- seq(min(data$scorea), max(data$scorea))
dt <- data.frame(scorea = scorea)
dt$OR <- exp(dt$scorea * S[2, 1])
dt$OR_lwr <- exp(dt$scorea * S[2, 1] - qnorm(0.975) * dt$scorea * S[2, 2])
dt$OR_upr <- exp(dt$scorea * S[2, 1] + qnorm(0.975) * dt$scorea * S[2, 2])

Pic_hypertension <- ggplot(data = dt, aes(x = scorea)) + 
  geom_line(aes(y = OR, color = "OR"), linetype = 2) + 
  geom_line(aes(y = OR_lwr, color = "OR_lower"), linetype = 2) + 
  geom_line(aes(y = OR_upr, color = "OR_upper"), linetype = 2) + 
  labs(color = "") + 
  theme(legend.position = c(0.2, 0.8))

Pic_hypertension


