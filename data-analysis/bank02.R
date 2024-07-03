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

# Bar graph of Marital Status
ggplot(data, aes(x=marital)) +
  geom_bar(fill="blue", color="black", alpha=0.7) +
  theme_minimal() +
  labs(title="Bar Graph of Marital Status", x="Marital Status", y="Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Bar graph of Education
ggplot(data, aes(x=education)) +
  geom_bar(fill="green", color="black", alpha=0.7) +
  theme_minimal() +
  labs(title="Bar Graph of Education", x="Education Level", y="Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Bar graph of Default Status
ggplot(data, aes(x=default)) +
  geom_bar(fill="purple", color="black", alpha=0.7) +
  theme_minimal() +
  labs(title="Bar Graph of Default Status", x="Default Status", y="Count")

# Bar graph of Housing Loan Status
ggplot(data, aes(x=housing)) +
  geom_bar(fill="orange", color="black", alpha=0.7) +
  theme_minimal() +
  labs(title="Bar Graph of Housing Loan Status", x="Housing Loan", y="Count")

# Bar graph of Contact Method
ggplot(data, aes(x=contact)) +
  geom_bar(fill="red", color="black", alpha=0.7) +
  theme_minimal() +
  labs(title="Bar Graph of Contact Method", x="Contact Method", y="Count")

# Boxplot of Age by Marital Status
ggplot(data, aes(x=marital, y=age)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title="Boxplot of Age by Marital Status", x="Marital Status", y="Age") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Scatter plot of Age vs. Balance
ggplot(data, aes(x=age, y=balance)) +
  geom_jitter(width = 0.2) +
  theme_minimal() +
  labs(title="Scatter Plot of Age vs. Balance", x="Age", y="Balance")

# Histogram of Campaign
ggplot(data, aes(x=campaign)) +
  geom_histogram(binwidth=1, fill="purple", color="black", alpha=0.7) +
  theme_minimal() +
  labs(title="Histogram of Campaign", x="Number of Contacts", y="Frequency")

# Histogram of Previous Campaign Contacts
ggplot(data, aes(x=previous)) +
  geom_histogram(binwidth=1, fill="orange", color="black", alpha=0.7) +
  theme_minimal() +
  labs(title="Histogram of Previous Campaign Contacts", x="Number of Contacts", y="Frequency")

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
