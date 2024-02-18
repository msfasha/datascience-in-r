library(car)

# Load the SLID data
data(SLID)

# Assuming 'age' is the numeric variable you want to bin, replace 'age' with your numeric variable
# Create bins for the numeric data
SLID$age_bins <- cut(SLID$age,
                     breaks = 4, # for example, creating 4 bins
                     labels = c("Young", "Mid-age", "Senior", "Elderly"),
                     include.lowest = TRUE)


# Plotting the bar chart for age_bins
ggplot(SLID, aes(x = age_bins)) +
  geom_bar(fill = "steelblue", color = "black") +
  labs(title = "Age Group Distribution",
       x = "Age Groups",
       y = "Count") +
  theme_minimal()


# OR using barplot function

# Assuming 'age_bins' is already created in your dataset
age_bins_freq <- table(SLID$age_bins)

# Plotting the bar chart
barplot(age_bins_freq, 
        main = "Age Group Distribution", 
        xlab = "Age Groups", 
        ylab = "Count", 
        col = "steelblue", 
        border = "black")


