# Create sample data
x <- 1:10
y <- c(3, 6, 4, 9, 8, 2, 7, 5, 1, 10)
categories <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J")
values <- c(8, 3, 6, 9, 4, 7, 2, 10, 1, 5)
data <- data.frame(x, y, categories, values)


par(mfrow = c(3, 2))

# Scatter Plot
plot(x, y, type = "p", col = "blue", main = "Scatter Plot", xlab = "X-axis", ylab = "Y-axis")

# Line Plot
plot(x, y, type = "l", col = "red", main = "Line Plot", xlab = "X-axis", ylab = "Y-axis")

# Bar Plot
barplot(data$values, names.arg = data$categories, col = "green", main = "Bar Plot", xlab = "Categories", ylab = "Values")

# Histogram
hist(y, breaks = 5, col = "purple", main = "Histogram", xlab = "Values", ylab = "Frequency")

x <- c(1, 2, 3, 4, 5)
y <- c(10, 8, 6, 4, 2)
plot(x, y, type = "p", col = "blue", main = "Scatter Plot", xlab = "X-axis", ylab = "Y-axis")

# Adding a line
lines(x, y, col = "red")