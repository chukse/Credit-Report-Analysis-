setwd("C:/Users/Chuks/Documents")

# Load necessary libraries
library(dplyr)

df <- read.csv("creditBalance.csv", stringsAsFactors = TRUE)
knitr::kable(head(df, 3))
summary(df)

df_quant <- df %>% select(-Gender, -Married, -Student)
mahalanobis_distances <- mahalanobis(df_quant, colMeans(df_quant, na.rm = TRUE), cov(df_quant, use = "complete"))
indices <- order(mahalanobis_distances, decreasing = TRUE)[1:10]
indices

library(DescTools)
lof <- LOF(df_quant, round(0.2 * nrow(df_quant)))
indices_lof <- order(lof, decreasing = TRUE)[1:10]
indices_lof

library(isotree)
forest <- isolation.forest(df_quant, nthreads = 1)
scores <- predict(forest, df_quant, type = "score")
indices_if <- order(scores)[1:10]
indices_if

library(ggplot2)
ggplot(df, aes(x = Balance)) +
  geom_histogram(fill = "red", bins = 30) +
  labs(title = "Histogram of Balance", x = "Balance", y = "Frequency") +
  theme_minimal()

library(dplyr)
library(tidyr)
df_quant_long <- df %>%
  select(Income, Limit, Rating, Cards, Age, Education) %>%
  gather(key = "variable", value = "value")

ggplot(df_quant_long, aes(x = value)) +
  geom_histogram(bins = 30, fill = "red", color = "black") +
  facet_wrap(~ variable, scales = "free") +
  labs(title = "Faceted Histograms of Quantitative Predictors", x = "Value", y = "Frequency") +
  theme_minimal()

df_cat <- df %>%
  select(Gender, Married, Student) %>%
  gather(key = "variable", value = "value")

ggplot(df_cat, aes(x = value)) +
  geom_bar(fill = 'green', color = "black") +
  facet_wrap(~ variable) +
  labs(title = "Faceted Bar Charts of Categorical Predictors", x = "Category", y = "Count") +
  theme_minimal()

num.var <- df_cat %>% distinct(variable) %>% nrow()
ggplot(data = df_cat, mapping = aes(x = value, y = rep(df$Balance, num.var))) +
  geom_boxplot(fill = "green", color = "black") +
  facet_wrap(~ variable, scales = "free") +
  labs(title = "Boxplots of Balance by Categorical Predictors", x = "Category", y = "Balance") +
  theme_minimal()

ggplot(data = df_quant_long, mapping = aes(x = value, y = rep(df$Balance, num.var))) +
  geom_point(col = "blue", alpha = 0.5) +
  facet_wrap(~ variable, scales = "free_x") +
  labs(title = "Scatter Plots of Balance vs Quantitative Predictors", x = "Predictor Value", y = "Balance") +
  theme_minimal()

library(corrplot)
corr_matrix <- cor(df_quant, use = "complete.obs")
corrplot(corr_matrix, method = "color", type = "upper", tl.col = "black", tl.srt = 45, title = "Correlation Matrix of Quantitative Predictors")

library(mice)
df.missing <- df
df.missing$Balance[202] <- NA  # Introduce missing value

set.seed(36641)
imp <- mice(df.missing, m = 100, method = c(rep("", 9), "pmm"))

bal <- as.numeric(imp$imp$Balance[, 1:100])
table(bal)
