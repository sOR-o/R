# Load necessary libraries
library(ggplot2)
library(dplyr)
library(reshape2)
library(readr)

# Load the data
data <- read_csv("bank.csv")

# Explicitly specify column types for conversion
data <- data %>%
  mutate(
    across(c(age, balance, day, duration, campaign, pdays, previous), as.numeric),
    across(c(job, marital, education, default, housing, loan, contact, month, poutcome, deposit), as.factor)
  )

# Replace 'unknown' with NA for character/factor columns
unknown_columns <- c("job", "marital", "education", "default", "housing", "loan", "contact", "month", "poutcome", "deposit")

for (col in unknown_columns) {
  if ("unknown" %in% levels(data[[col]])) {
    data[[col]] <- fct_explicit_na(data[[col]], na_level = "unknown")
  } else {
    data[[col]] <- factor(data[[col]], levels = c(levels(data[[col]]), "unknown"))
    data[[col]] <- fct_explicit_na(data[[col]], na_level = "unknown")
  }
}

# Visualizations

# Bar graph of Marital Status with colors by deposit
ggplot(data, aes(x=marital, fill=deposit)) +
  geom_bar(position="fill") +
  theme_minimal() +
  labs(title="Bar Graph of Marital Status by Deposit", x="Marital Status", y="Proportion") +
  scale_fill_manual(values = c("blue", "red")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Stacked bar graph of Education by Deposit
ggplot(data, aes(x=education, fill=deposit)) +
  geom_bar(position="stack") +
  theme_minimal() +
  labs(title="Stacked Bar Graph of Education by Deposit", x="Education Level", y="Count") +
  scale_fill_manual(values = c("blue", "red")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Boxplot of Balance by Education with colors by deposit
ggplot(data, aes(x=education, y=balance, fill=deposit)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title="Boxplot of Balance by Education and Deposit", x="Education Level", y="Balance") +
  scale_fill_manual(values = c("blue", "red")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Scatter plot of Age vs. Balance with colors by deposit
ggplot(data, aes(x=age, y=balance, color=deposit)) +
  geom_jitter(width = 0.2) +
  theme_minimal() +
  labs(title="Scatter Plot of Age vs. Balance by Deposit", x="Age", y="Balance") +
  scale_color_manual(values = c("blue", "red"))

# Density plot of Age by Deposit
ggplot(data, aes(x=age, fill=deposit)) +
  geom_density(alpha=0.5) +
  theme_minimal() +
  labs(title="Density Plot of Age by Deposit", x="Age", y="Density") +
  scale_fill_manual(values = c("blue", "red"))

# Heatmap of Correlation Matrix
# Prepare data for heatmap
heatmap_data <- data %>%
  select(age, balance, duration, campaign, pdays, previous) %>%
  mutate_all(~as.numeric(.))

# Correlation matrix
cor_matrix <- cor(heatmap_data, use="complete.obs")

# Heatmap
ggplot(melt(cor_matrix), aes(Var1, Var2, fill=value)) +
  geom_tile() +
  scale_fill_gradient2(low="blue", high="red", mid="white", midpoint=0) +
  theme_minimal() +
  labs(title="Heatmap of Correlation Matrix", x="", y="")
