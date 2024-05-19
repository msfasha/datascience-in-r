# Load necessary libraries
library(ggplot2)
library(stats)

# Simulate exam scores (for example purposes, in real scenarios this would be actual data)
set.seed(123)
exam_scores <- rnorm(1000, mean = 75, sd = 10)

# Ensure scores are within the range 0-100
exam_scores <- pmax(pmin(exam_scores, 100), 0)

# Perform KDE
exam_score_density <- density(exam_scores)

# Function for the estimated density
density_function <- approxfun(exam_score_density$x, exam_score_density$y, method = "linear")

# Integrate the density function over the range 70 to 85
probability_between_70_and_85 <- integrate(density_function, lower = 70, upper = 85)$value

# Print the estimated probability
probability_between_70_and_85

# My Question: isn't the density in the code above a function itself?
# Now, `density_function` is a true function in the programming sense. You can pass a value to `density_function`,
# and it will return an interpolated density value.
# This can be particularly useful for integrating the density over a specific range or for other calculations 
# where you need density values at points not explicitly calculated by the original `density()` call.
# Yes, in the R code example I provided earlier for Kernel Density Estimation (KDE), the density() function does indeed 
# return a function-like object that represents the estimated probability density function (PDF) of your data.
# The density() function in R computes the kernel density estimate and returns an object that contains several components, including:
# x: The points at which the density is estimated.
# y: The estimated density values at these points.
# This object can be treated like a function in the sense that it gives you the estimated density values over a range of points. 
# However, it's not a function in the strictest programming sense 
# it doesn't take arguments and return values like a conventional function. Instead, you get a discrete set of x (points) and y 
# (density values) which together approximate the continuous density function.
# If you need a function that can be used to calculate the density at any given point, you can create an 
# approximation function from this object using approxfun(). 
# This function takes the x and y components and creates an actual function that interpolates the density values between the points:
# density_obj <- density(data)  KDE
# density_function <- approxfun(density_obj$x, density_obj$y, method = "linear")
# Now, density_function is a true function in the programming sense. You can pass a value to density_function, and it will return an interpolated density value.
# This can be particularly useful for integrating the density over a specific range or for other calculations where you need density values at points not explicitly
# calculated by the original density() call.