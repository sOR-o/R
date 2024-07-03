# Load necessary libraries
library(ggplot2)
library(dplyr)
library(reshape2)
library(readr)
library(gridExtra)  # Load gridExtra for grid.arrange

# Function to read and preprocess data
read_and_preprocess_data <- function(file_path) {
  # Read CSV
  data <- read_csv(file_path)
  
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
  
  return(data)
}

# Function to create boxplot
create_boxplot <- function(data, x_var, y_var, title) {
  ggplot(data, aes_string(x=x_var, y=y_var)) +
    geom_boxplot() +
    theme_minimal() +
    labs(title=title, x=x_var, y=y_var) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# Function to create scatter plot
create_scatter_plot <- function(data, x_var, y_var, title) {
  ggplot(data, aes_string(x=x_var, y=y_var)) +
    geom_jitter(width = 0.2) +
    theme_minimal() +
    labs(title=title, x=x_var, y=y_var)
}

# Function to create histogram
create_histogram <- function(data, x_var, title) {
  ggplot(data, aes_string(x=x_var)) +
    geom_histogram(binwidth=5, fill="blue", color="black", alpha=0.7) +
    theme_minimal() +
    labs(title=title, x=x_var, y="Frequency")
}

# Function to create bar graph
create_bar_graph <- function(data, x_var, title) {
  ggplot(data, aes_string(x=x_var)) +
    geom_bar(fill="purple", color="black", alpha=0.7) +
    theme_minimal() +
    labs(title=title, x=x_var, y="Count") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# Function to create heatmap
create_heatmap <- function(data) {
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
}

# Function to create bar graph with colored bars by deposit
create_colored_bar_graph <- function(data, x_var, color_var, title) {
  ggplot(data, aes_string(x=x_var, fill=color_var)) +
    geom_bar(position="stack") +
    theme_minimal() +
    labs(title=title, x=x_var, y="Count") +
    scale_fill_manual(values = c("blue", "red")) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# Function to create scatter plot with colors by deposit
create_colored_scatter_plot <- function(data, x_var, y_var, color_var, title) {
  ggplot(data, aes_string(x=x_var, y=y_var, color=color_var)) +
    geom_jitter(width = 0.2) +
    theme_minimal() +
    labs(title=title, x=x_var, y=y_var) +
    scale_color_manual(values = c("blue", "red"))
}

# Read and preprocess data
data <- read_and_preprocess_data("bank.csv")

# Create and display visualizations
plot1 <- create_boxplot(data, "job", "age", "Boxplot of Age by Job")
plot2 <- create_scatter_plot(data, "age", "balance", "Scatter Plot of Age vs. Balance")
plot3 <- create_histogram(data, "age", "Histogram of Age")
plot4 <- create_bar_graph(data, "job", "Bar Graph of Job")
plot5 <- create_heatmap(data)
plot6 <- create_colored_bar_graph(data, "marital", "deposit", "Bar Graph of Marital Status by Deposit")
plot7 <- create_colored_scatter_plot(data, "age", "balance", "deposit", "Scatter Plot of Age vs. Balance by Deposit")

# Display plots
grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, plot7, ncol=2)
