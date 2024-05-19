# Suppose you have data representing the lifetimes of a set of mechanical components, 
# and you want to model these lifetimes using a Weibull distribution. 
# You can use MLE to estimate the shape and scale parameters of the Weibull distribution.
# ChatGPT 4

# Load necessary library
library(stats4)

# Sample data representing lifetimes of components
# In a real scenario, this would be your actual data
lifetime_data <- rweibull(100, shape = 1.5, scale = 500) # simulated data

# Define the negative log-likelihood function for the Weibull distribution
neg_log_likelihood_weibull <- function(shape, scale) {
  if (shape <= 0 || scale <= 0) return(Inf)
  -sum(dweibull(lifetime_data, shape = shape, scale = scale, log = TRUE))
}

# Initial parameter estimates (guesses)
start_params_weibull <- c(shape = 1.5, scale = 300)  # Adjust these based on your data

# Add lower bounds to ensure positive parameters
lower_bounds <- c(shape = 1e-6, scale = 1e-6)

# Perform the optimization using MLE
mle_result_weibull <- mle(neg_log_likelihood_weibull, start = start_params_weibull, method = "L-BFGS-B", lower = lower_bounds)

# Extract the estimated parameters
estimated_params_weibull <- coef(mle_result_weibull)

# Output the estimated parameters
print(estimated_params_weibull)

d <- dweibull(lifetime_data, shape = 1.5, scale = 500)
plot(lifetime_data,d)
