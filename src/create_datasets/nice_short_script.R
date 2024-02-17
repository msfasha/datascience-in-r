# Create a data frame with scores and corresponding student majors
# set.seed(123)  # for reproducible results
data <- data.frame(
  score = c(rnorm(30, mean = 75, sd = 10),  # scores for major A
            rnorm(30, mean = 80, sd = 10),  # scores for major B
            rnorm(30, mean = 85, sd = 10)), # scores for major C
  major = factor(rep(c("A", "B", "C"), each = 30))  # corresponding majors
)


hist(data$score,30)
