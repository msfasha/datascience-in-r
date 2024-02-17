# Plot using the barplot function
# Set the average rate (lambda) for the Poisson distribution
lambda <- 30

# Create a sequence of numbers from 0 to, for example, twice the lambda
x <- 0:(2 * lambda)

# Calculate the probability mass function for these values
y <- dpois(x, lambda)

# Plot the PMF as a barplot
barplot(y, names.arg = x, main = paste("Poisson Distribution with lambda =", lambda), xlab = "Number of events", ylab = "Probability")

# If necessary, install ggplot2 using install.packages("ggplot2")
library(ggplot2)

# Plot using the ggplot function
# Set lambda for the Poisson distribution
library(ggplot2)
lambda <- 30

# Create a data frame with the sequence and corresponding probabilities
data <- data.frame(
  'events' = 0:(2 * lambda),
  'probability' = dpois(0:(2 * lambda), lambda)
)

# Plot using ggplot2
ggplot(data, aes(x = events, y = probability)) +
  geom_bar(stat = "identity") +
  ggtitle(paste("Poisson Distribution with lambda =", lambda)) +
  xlab("Number of events") +
  ylab("Probability")
