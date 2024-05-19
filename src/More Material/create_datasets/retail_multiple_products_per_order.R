library(lubridate)
library(dplyr)

# Set seed for reproducibility
set.seed(123)

# Define branches and their populations
branches <- c("Irbid", "Ajloun", "Jerash", "Mafraq", "Balqa", "Amman", "Zarqa", "Madaba", "Karak", "Tafilah", "Ma'an", "Aqaba")
populations <- c(2050300, 204000, 274500, 637000, 569500, 4642000, 1581000, 219100, 366700, 111500, 183500, 217900)
names(populations) <- branches

# Define Products
# Create a data frame from the provided data
products_dataframe <- data.frame(
  "Sequence.Number" = 1:60,
  "Main.Category" = c(
    rep("Health and Beauty", 10),
    rep("Electronic Accessories", 10),
    rep("Home and Lifestyle", 10),
    rep("Sports and Travel", 10),
    rep("Food and Beverages", 10),
    rep("Fashion Accessories", 10)
  ),
  "Sub.Category" = c(
    "Skincare Products", "Hair Care Products", "Makeup and Cosmetics",
    "Fragrances and Perfumes", "Personal Care Appliances", "Dietary Supplements",
    "Men's Grooming Products", "Bath and Body Products", "Oral Care Products",
    "Health and Wellness Devices", "Mobile Phone Accessories", "Computer Accessories", "Audio and Video Accessories", "Camera Accessories", "Wearable Technology", "Charging Accessories", "Gaming Accessories", "Car Electronics", "Home Office Equipment", "Electronic Storage Devices","Home Decor", "Kitchenware", "Bedding and Linens", "Bath Accessories","Furniture", "Gardening Tools and Decor", "Lighting","Storage and Organization", "Home Improvement Tools", "Cleaning Supplies","Athletic Apparel", "Fitness Equipment", "Outdoor Gear", "Sports Accessories","Travel Accessories", "Water Sports Equipment", "Team Sports Equipment","Cycling Accessories", "Winter Sports Gear", "Fitness Trackers and Technology","Snacks and Confectionery", "Beverages", "Dairy Products", "Baked Goods","Canned and Jarred Foods", "Condiments and Sauces", "Grains and Pasta",
    "Health Foods and Organic Products", "Frozen Foods", "Specialty Foods",
    "Jewelry", "Handbags and Wallets", "Watches", "Scarves and Wraps",
    "Hats and Caps", "Belts", "Sunglasses", "Hair Accessories",
    "Ties and Cufflinks", "Gloves and Mittens"
  )
)

customer_types <- c("customer", "one-time-buyer")
customer_genders <- c("male", "female")
payment_methods <- c("Visa", "CliQ", "Cash")

# Total number of orders and proportion per branch
total_orders <- 200
orders_per_branch <- round((populations / sum(populations)) * total_orders)


##############################################################
# Functions Definitions
##############################################################

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
  random_date_time <- as.POSIXct(start_of_year + runif(1, min = 0, max = sec), origin = "1970-01-01")
  
  return(random_date_time)  # Explicit return statement
}

# Function to determine the number of products per order
get_num_products <- function(is_customer) {
  if (is_customer) {
    max(1, round(rnorm(1, mean = 8, sd = 3)))
  } else {
    max(1, round(rnorm(1, mean = 5, sd = 6)))
  }
}

# Function to get customer id from a random pool of 1000 customers
get_customer_id <- function() {
 cid <- sample(1:1000,1)
 
 return (cid)
}

# Function to generate multiple records of products purchases for a given order
get_products <- function(branch, order_id) {
  
  # use probability to determine if this order concerns a customer or a non-customer
  is_customer <- runif(1) < 0.5  # 50% chance
  if (is_customer)
    customer_id <- get_customer_id()
  else
    customer_id <- NA
  
  # determine the number of products/records to create based on customer's status (customer/non-customer)
  num_products <- get_num_products(is_customer)
  
  # create a random list containing the products id for this order
  products_list <- sample(1:nrow(products_dataframe), size = num_products, replace = FALSE)
  
  # create a dataframe that will contain the records
  df <- data.frame()
  
  for (product in products_list)
  {
    
    purchase_datetime <- random_datetime(as.POSIXct("2015-01-01"), Sys.time())
    
    # # Increase probability of selecting "Fashion accessories" around New Year
    # if (month(purchase_datetime) == 12 || month(purchase_datetime) == 1) {
    #   product_lines_weights <- ifelse(product_lines == "Fashion accessories", 2, 1)
    #   product_line <- sample(product_lines, 1, prob = product_lines_weights)
    # } else {
    #   product_line <- sample(product_lines, 1)
    # }
    
    # # Adjusting for specific conditions (e.g., gender preference for certain product lines)
    # if (product_line == "Health and beauty" && runif(1) > 0.4) {
    #   customer_gender <- "female"
    # }
    # if (product_line == "Sports and travel" && runif(1) > 0.4) {
    #   customer_gender <- "male"
    # }
    # 
    
    unit_price <- runif(1, min = 5, max = 100)
    
    customer_type <- is_customer
    
    # Members tend to buy more quantities than non-members, based on a normal distribution
    if (is_customer) {
      quantity <- round(max(1, rnorm(1, mean = 6, sd = 2)))  # Normal distribution with higher mean for members
    } else {
      quantity <- round(max(1, rnorm(1, mean = 3, sd = 2)))  # Normal distribution with lower mean for non-members
    }
    
    total_price <- unit_price * quantity
    tax_amount <- total_price * 0.16
    
    # Skew gender probability based on branch location
    if (branch %in% c("Karak", "Tafila", "Ma'an")) {
      customer_gender <- ifelse(runif(1) < 0.6, "male", "female")  # Higher probability for males
    } else if (branch == "Amman") {
      customer_gender <- ifelse(runif(1) < 0.6, "female", "male")  # Higher probability for females
    } else {
      customer_gender <- sample(customer_genders, 1)
    }
    
    
    payment_method <- sample(payment_methods, 1)
    # If branch is Amman or Aqaba, give higher probability of 'Visa' and 'CliQ' payments
    if (branch %in% c("Amman", "Aqaba") && runif(1) > 0.3) {
      payment_method <- sample(c("Visa", "CliQ"), 1)
    }
    
    # Step 1: Create a vector for the new row
    record <- c(order_id = order_id,
    branch_name = branch,
    customer_type = is_customer,
    customer_id = customer_id,
    purchase_date = as.Date(purchase_datetime),
    purchase_time = format(purchase_datetime, "%H:%M:%S"),
    product_line = product,
    unit_price = unit_price,
    quantity = quantity,
    tax_amount = tax_amount,
    customer_gender = customer_gender,
    payment_method = payment_method)
    
    # Step 2: Convert the vector into a dataframe row
    new_row_as_df <- as.data.frame(t(record))
    
    # Step 3: Append the row to the existing dataframe
    df <- rbind(df, new_row_as_df)
    # Add the record to the dataframe
    # df <- rbind(df, record)
  }
  return (df)
}

##############################################################
# Entry Point
##############################################################

# Generate data for each branch
retail_multi_products <- data.frame()

print("Start executing...")
for (branch in names(orders_per_branch)) { # loop for every branch
  print(paste("Generating records for: ", branch))
  for (order_id in 1:orders_per_branch[branch]) { # loop for every order for the current branch
    order_products <- get_products(branch, paste(branch,order_id))
    retail_multi_products <- rbind(retail_multi_products, order_products)
  }
}

# Display a sample of the dataset
View(retail_multi_products)

write.csv(retail_multi_products, "retail_multi_products.csv", row.names = FALSE)
