# Generated by ChatGPT 4 based on an image
# Set alpha level
alpha <- 0.05

# Create a sequence of x values for the normal distribution
x <- seq(-4, 4, length=100)

# Create a density of the standard normal distribution
y <- dnorm(x)

# Plot the normal distribution
plot(x, y, type="n", xlab="", ylab="", main="")

# Draw the curve
lines(x, y)

# Shade the rejection regions
x_reject_left <- seq(min(x), qnorm(alpha/2), length=100)
x_reject_right <- seq(qnorm(1 - alpha/2), max(x), length=100)
y_reject_left <- dnorm(x_reject_left)
y_reject_right <- dnorm(x_reject_right)

# Fill the rejection regions with color
polygon(c(x_reject_left, rev(x_reject_left)), c(rep(0, length(y_reject_left)), rev(y_reject_left)), col="red")
polygon(c(x_reject_right, rev(x_reject_right)), c(rep(0, length(y_reject_right)), rev(y_reject_right)), col="red")

# Add text for critical values and regions
text(qnorm(alpha/2), 0, "Critical Value", pos=3, cex=0.7)
text(qnorm(1 - alpha/2), 0, "Critical Value", pos=4, cex=0.7)
text(0, max(y)/2, "Do not reject H0", cex=0.7)
text(min(x_reject_left), y_reject_left[1]/2, "Reject H0", srt=90, cex=0.7)
text(max(x_reject_right), y_reject_right[1]/2, "Reject H0", srt=90, cex=0.7)

# Add text for alpha/2 and 1-alpha
text(qnorm(alpha/2), 0, expression(alpha/2), pos=1, cex=0.7, offset=1)
text(qnorm(1 - alpha/2), 0, expression(alpha/2), pos=1, cex=0.7, offset=1)
text(0, max(y)/2, expression(1-alpha), cex=0.7)

# Draw dashed lines for critical values
abline(v=qnorm(alpha/2), lty=2)
abline(v=qnorm(1 - alpha/2), lty=3)
