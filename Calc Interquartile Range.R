install.packages(mtcars)
library(ggplot2)

summary(mtcars)

print(mtcars)
cars <- data.frame(mtcars)
print(cars)

# Create a box plot to visualize outliers
ggplot(mtcars, aes(y = mpg)) +
  geom_boxplot(fill="lightblue", color="black", outlier.colour="red", outlier.shape=1) +
  labs(title="Box Plot of MPG in mtcars Dataset", y="Miles per Gallon (mpg)")

# Calculate the interquartile range (IQR). IQR is a measure of statistical dispersion. 
# It's calculated as the difference between the 75th percentile and the 25th percentile. 
IQR_mpg = IQR(cars$mpg)

# Calculate the first and third quartiles
Q1 = quantile(mtcars$mpg, 0.25)
Q3 = quantile(mtcars$mpg, 0.75)

# Calculate the lower and upper bounds for outliers
lower_bound = Q1 - 1.5 * IQR_mpg
upper_bound = Q3 + 1.5 * IQR_mpg

# Identify the outlier values
outliers = subset(mtcars$mpg, mtcars$mpg < lower_bound | mtcars$mpg > upper_bound)

# Print the outlier values
outliers