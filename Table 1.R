library(haven)
library(dplyr)
library(broom)

data_path <- "E:/2. 项目工作/1. 东方膳食模式评分/10分制/2025.8.19 补充分析/compare_20251113.sas7bdat"
data <- read_sas(data_path)


data <- data %>%
  mutate(overweight = factor(overweight, levels = c("BMI<24", "BMI≥24")),
         ob = factor(ob, levels = c("BMI<28", "BMI≥28")),
         hp = factor(hp, levels = c("正常", "高血压")),
         scorea_q = factor(scorea_q, levels = c(0, 1, 2)),
         A1 = factor(A1),
         ur_gt = factor(ur_gt))

model <- glm(formula = overweight ~ scorea_q + A1 + age + ur_gt,
  data = data,
  family = binomial(link = "logit"))
or_results <- tidy(model, conf.int = TRUE, exponentiate = TRUE)
or_results

model <- glm(formula = ob ~ scorea_q + A1 + age + ur_gt,
             data = data,
             family = binomial(link = "logit"))
or_results <- tidy(model, conf.int = TRUE, exponentiate = TRUE)
or_results

model <- glm(formula = ob ~ scorea_q + A1 + age + ur_gt,
             data = data,
             family = binomial(link = "logit"))
or_results <- tidy(model, conf.int = TRUE, exponentiate = TRUE)
or_results

model <- glm(formula = hp ~ scorea_q + A1 + age + ur_gt,
             data = data,
             family = binomial(link = "logit"))
or_results <- tidy(model, conf.int = TRUE, exponentiate = TRUE)
or_results

model <- glm(formula = overweight ~ scorea + A1 + age + ur_gt,
             data = data,
             family = binomial(link = "logit"))
or_results <- tidy(model, conf.int = TRUE, exponentiate = TRUE)
or_results

model <- glm(formula = ob ~ scorea + A1 + age + ur_gt,
             data = data,
             family = binomial(link = "logit"))
or_results <- tidy(model, conf.int = TRUE, exponentiate = TRUE)
or_results

model <- glm(formula = hp ~ scorea + A1 + age + ur_gt,
             data = data,
             family = binomial(link = "logit"))
or_results <- tidy(model, conf.int = TRUE, exponentiate = TRUE)
or_results

