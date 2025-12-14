library(tidyverse)
library(rstatix)
library(car)

df <- read.csv("C:/Users/Emmanuel/Downloads/Alzheimers Mice Data.csv")

df$AD_Status <- as.factor(df$AD_Status)
df$Treatment <- as.factor(df$Treatment)

print("-------------------------------------------------")
print("ANALYSIS: TRAINING DAY ERRORS")
print("-------------------------------------------------")

# 1. Descriptive Statistics
print("--- Descriptive Statistics ---")
train_stats <- df %>%
  group_by(AD_Status, Treatment) %>%
  get_summary_stats(Training, type = "mean_sd")
print(train_stats)

# 2. Assumption: Outliers
print("--- Outlier Check ---")
train_outliers <- df %>%
  group_by(AD_Status, Treatment) %>%
  identify_outliers(Training)
print(train_outliers)

# 3. Assumption: Normality (Shapiro-Wilk)
print("--- Normality Check (Shapiro-Wilk) ---")
model_train <- aov(Training ~ AD_Status * Treatment, data = df)
print(shapiro_test(residuals(model_train)))

# 4. Assumption: Homogeneity of Variance (Levene's Test)
print("--- Homogeneity Check (Levene's Test) ---")
print(leveneTest(Training ~ AD_Status * Treatment, data = df))

# 5. 2-Way ANOVA
print("--- 2-Way ANOVA Results ---")
summary(model_train)

library(lsr)
print("--- Effect Size (Eta Squared) ---")
print(etaSquared(model_train, type = 2))


print("-------------------------------------------------")
print("ANALYSIS: MEMORY DAY ERRORS")
print("-------------------------------------------------")

# 1. Descriptive Statistics
mem_stats <- df %>%
  group_by(AD_Status, Treatment) %>%
  get_summary_stats(Memory, type = "mean_sd")
print(mem_stats)

# 2. Assumption: Outliers
mem_outliers <- df %>%
  group_by(AD_Status, Treatment) %>%
  identify_outliers(Memory)
print("--- Outliers Detected ---")
print(mem_outliers)

# 3. Assumption: Normality
model_mem <- aov(Memory ~ AD_Status * Treatment, data = df)
print("--- Normality Check ---")
print(shapiro_test(residuals(model_mem)))

# 4. Assumption: Homogeneity
print("--- Homogeneity Check ---")
print(leveneTest(Memory ~ AD_Status * Treatment, data = df))

# 5. 2-Way ANOVA
print("--- 2-Way ANOVA Results ---")
summary(model_mem)

# 6. Effect Size
print("--- Effect Size ---")
print(etaSquared(model_mem, type = 2))