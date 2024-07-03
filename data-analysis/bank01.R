# Load necessary libraries
library(ggplot2)
library(reshape2)
library(dplyr)
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

# Boxplot of Age by Job
ggplot(data, aes(x=job, y=age)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title="Boxplot of Age by Job", x="Job", y="Age") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Scatter plot of Age vs. Balance
ggplot(data, aes(x=age, y=balance)) +
  geom_jitter(width = 0.2) +
  theme_minimal() +
  labs(title="Scatter Plot of Age vs. Balance", x="Age", y="Balance")

# Histogram of Age
ggplot(data, aes(x=age)) +
  geom_histogram(binwidth=5, fill="blue", color="black", alpha=0.7) +
  theme_minimal() +
  labs(title="Histogram of Age", x="Age", y="Frequency")

# Bar graph of Job
ggplot(data, aes(x=job)) +
  geom_bar(fill="purple", color="black", alpha=0.7) +
  theme_minimal() +
  labs(title="Bar Graph of Job", x="Job", y="Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

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
