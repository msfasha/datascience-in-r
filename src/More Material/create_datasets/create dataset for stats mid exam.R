# Load necessary libraries
library(lubridate)
library(ggplot2)

# Set seed for reproducibility
set.seed(123)

# Generate a sequence of dates for the year 2023
dates <- seq(as.Date("2023-01-01"), as.Date("2023-12-31"), by="day")

# Define branches
branches <- c("Amman", "Zarqa", "Irbid")

# Randomly assign distributions to branches
set.seed(42) # Setting seed for reproducibility of distribution assignment
distributions <- sample(c("Normal", "Right Skewed", "Left Skewed"))
names(distributions) <- branches

# Function to generate sales data based on distribution type
generate_sales_data <- function(mean, sd, distribution, length) {
  if (distribution == "Normal") {
    rnorm(length, mean, sd)
  } else if (distribution == "Right Skewed") {
    exp(rnorm(length, mean, sd)) # Exponential transformation of normal data
  } else { # Left Skewed
    max(exp(rnorm(length, mean, sd))) - exp(rnorm(length, mean, sd)) + min(exp(rnorm(length, mean, sd))) # Inverse transformation
  }
}

# Generate sales data for each branch
sales_data_list <- list()
for (branch in branches) {
  average_sales <- sample(seq(100, 500, by=50), 1)
  std_dev_sales <- sample(seq(5, 30, by=5), 1)
  distribution_type <- distributions[branch]
  sales_data <- generate_sales_data(average_sales, std_dev_sales, distribution_type, length(dates))
  sales_data_list[[branch]] <- data.frame(Date = dates, Sales = round(sales_data), Branch = branch)
  print(paste(branch, "Distribution:", distribution_type, "Mean:", average_sales, "SD:", std_dev_sales))
}

# Combine all branch data
all_branches_sales <- do.call(rbind, sales_data_list)

# Preview data
head(all_branches_sales)
