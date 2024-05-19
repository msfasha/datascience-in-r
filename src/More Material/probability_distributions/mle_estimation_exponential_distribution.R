# Suppose you have data representing the time to failure of components, 
# and you assume that these times follow an exponential distribution. 
# You can use MLE to estimate the rate parameter of the exponential distribution.

# ChatGPT 4

# Load necessary library
library(stats4)

# Sample data representing time to failure of components
# In a real scenario, this would be your actual data
failure_time_data <- rexp(100, rate = 0.01) # simulated data

# Define the negative log-likelihood function for the exponential distribution
neg_log_likelihood_exponential <- function(rate) {
  -sum(dexp(failure_time_data, rate = rate, log = TRUE))
}

# Initial parameter estimate (guess)
start_param_exponential <- c(rate = 1/mean(failure_time_data))  # Initial guess can be 1/mean of the data

# Perform the optimization using MLE
mle_result_exponential <- mle(neg_log_likelihood_exponential, start = start_param_exponential, method = "L-BFGS-B")

# Extract the estimated parameter
estimated_param_exponential <- coef(mle_result_exponential)

# Output the estimated parameter
estimated_param_exponential

