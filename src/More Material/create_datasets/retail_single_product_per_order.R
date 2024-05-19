# Load necessary libraries
library(lubridate)
library(dplyr)

# Set seed for reproducibility
set.seed(123)

# Define parameters
product_lines <- c("Health and beauty", "Electronic accessories", "Home and lifestyle", "Sports and travel", "Food and beverages", "Fashion accessories")
counties <- c("Amman", "Zarqa", "Balqa", "Madaba", "Ajloun", "Jerash", "Irbid", "Ramtha", "Mafraq", "Karak", "Tafila", "Ma'an", "Aqaba")
county_populations <- c(4000000, 1000000, 500000, 200000, 150000, 200000, 1500000, 150000, 300000, 250000, 100000, 50000, 150000)
names(county_populations) <- counties
customer_types <- c("customer", "one-time-buyer")
customer_genders <- c("male", "female")
payment_methods <- c("Visa", "CliQ", "Cash")

# Function to generate a weighted random date-time
random_datetime <- function(start_date, end_date) {
  # Define weights based on the year - higher weights for recent years, except 2020 and 2021
  years <- year(start_date):year(end_date)
  weights <- seq_along(years)  # Linearly increasing weights
  weights[years == 2020] <- weights[years == 2020] / 2  # Halve the weight for 2020
  weights[years == 2021] <- weights[years == 2021] / 2  # Halve the weight for 2021
  
  # Select a year based on the weights
  selected_year <- sample(years, 1, prob = weights)
  
  # Generate a random date within the selected year
  start_of_year <- make_datetime(selected_year, 1, 1)
  end_of_year <- make_datetime(selected_year + 1, 1, 1) - seconds(1)
  
  sec <- as.numeric(difftime(end_of_year, start_of_year, units = "secs"))
  as.POSIXct(start_of_year + runif(1, min = 0, max = sec), origin = "1970-01-01")
}

# Function to generate a single record
generate_record <- function(county, invoice_number) {
  purchase_datetime <- random_datetime(as.POSIXct("2015-01-01"), Sys.time())
  
  # Increase probability of selecting "Fashion accessories" around New Year
  if (month(purchase_datetime) == 12 || month(purchase_datetime) == 1) {
    product_lines_weights <- ifelse(product_lines == "Fashion accessories", 2, 1)
    product_line <- sample(product_lines, 1, prob = product_lines_weights)
  } else {
    product_line <- sample(product_lines, 1)
  }
  
  unit_price <- runif(1, min = 5, max = 100)
  
  customer_type <- sample(customer_types, 1)
  
  # Members tend to buy more quantities than non-members, based on a normal distribution
  if (customer_type == "member") {
    quantity <- round(max(1, rnorm(1, mean = 6, sd = 2)))  # Normal distribution with higher mean for members
  } else {
    quantity <- round(max(1, rnorm(1, mean = 3, sd = 2)))  # Normal distribution with lower mean for non-members
  }
  
  total_price <- unit_price * quantity
  tax_amount <- total_price * 0.16
  
  # Skew gender probability based on branch location
  if (county %in% c("Karak", "Tafila", "Ma'an")) {
    customer_gender <- ifelse(runif(1) < 0.6, "male", "female")  # Higher probability for males
  } else if (county == "Amman") {
    customer_gender <- ifelse(runif(1) < 0.6, "female", "male")  # Higher probability for females
  } else {
    customer_gender <- sample(customer_genders, 1)
  }
  
  # Adjusting for specific conditions (e.g., gender preference for certain product lines)
  if (product_line == "Health and beauty" && runif(1) > 0.4) {
    customer_gender <- "female"
  }
  if (product_line == "Sports and travel" && runif(1) > 0.4) {
    customer_gender <- "male"
  }
  payment_method <- sample(payment_methods, 1)
  # Higher probability of 'visa' and 'clique' payments in Amman and Aqaba
  if (county %in% c("Amman", "Aqaba") && runif(1) > 0.3) {
    payment_method <- sample(c("visa", "clique"), 1)
  }
  
  return(data.frame(
    invoice_id = sprintf("%s%03d", substr(tolower(county), 1, 3), invoice_number),
    branch_name = county,
    purchase_date = as.Date(purchase_datetime),
    purchase_time = format(purchase_datetime, "%H:%M:%S"),
    product_line = product_line,
    unit_price = unit_price,
    quantity = quantity,
    tax_amount = tax_amount,
    customer_type = customer_type,
    customer_gender = customer_gender,
    payment_method = payment_method
  ))
}

# Generate the dataset
total_records <- 10000
total_population <- sum(county_populations)
records <- list()

# Creating records for each county, proportional to its population
for (county in names(county_populations)) {
  num_records <- round((county_populations[county] / total_population) * total_records)
  for (invoice_number in 1:num_records) {
    records[[length(records) + 1]] <- generate_record(county, invoice_number)
  }
}

# Combine all records into a data frame
retail_dataset <- do.call(rbind, records)

# Display the first few rows
# head(retail_dataset)


write.csv(retail_dataset, "retail_single_product.csv", row.names = FALSE)