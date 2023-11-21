### Calculate Statistical Significance and Confidence Intervals Below ###

# Load necessary libraries
library(ggplot2)
library(dplyr)

# Define the data
visitors_A <- 80000
conversions_A <- 1600
Confidence_Level <- 0.90
rate_A <- conversions_A / visitors_A

visitors_B <- 80000
conversions_B <- 1700
rate_B <- conversions_B / visitors_B

# Pooled conversion rate
overall_rate <- (conversions_A + conversions_B) / (visitors_A + visitors_B)

# Z-Score for p-value calculation
z_score <- (rate_B - rate_A) / sqrt(overall_rate * (1 - overall_rate) * (1/visitors_A + 1/visitors_B))

# P-Value for a one-sided test
p_value <- pnorm(z_score, lower.tail = FALSE)

# Z-Score for 90% Confidence Level (One-Sided)
z_score_CL <- qnorm(Confidence_Level)

# Confidence Interval for Variation A
se_A <- sqrt(rate_A * (1 - rate_A) / visitors_A)
ci_lower_A <- rate_A - z_score_CL * se_A
ci_upper_A <- rate_A + z_score_CL * se_A

# Confidence Interval for Variation B
se_B <- sqrt(rate_B * (1 - rate_B) / visitors_B)
ci_lower_B <- rate_B - z_score_CL * se_B
ci_upper_B <- rate_B + z_score_CL * se_B

# Print the results
cat("P-Value: ", p_value, "\n")
cat("90% Confidence Interval for Variation A: [", ci_lower_A, ", ", ci_upper_A, "]\n")
cat("90% Confidence Interval for Variation B: [", ci_lower_B, ", ", ci_upper_B, "]\n")






### Creating CI Graph###

# Define labels and errors for the confidence intervals
labels <- c('Control', 'Variant 1')
conversion_rates <- c(rate_A, rate_B)
ci_lower_bounds <- c(ci_lower_A, ci_lower_B)
ci_upper_bounds <- c(ci_upper_A, ci_upper_B)
errors <- c(ci_upper_A - rate_A, ci_upper_B - rate_B)

# Create a data frame for plotting
data <- data.frame(
  Variation = factor(labels, levels = rev(labels)),
  ConversionRate = conversion_rates,
  CIError = errors,
  Lower = ci_lower_bounds,
  Upper = ci_upper_bounds
)

# Create the horizontal bar plot with confidence intervals
p <- ggplot(data, aes(x = Variation, y = ConversionRate)) +
  geom_bar(stat = "identity", aes(fill = Variation), position = position_dodge()) +
  geom_errorbar(aes(ymin = ConversionRate - CIError, ymax = ConversionRate + CIError), width = 0.2) +
  coord_flip() +  # Flip the coordinates to make the bars horizontal
  xlab("Conversion Rate") +
  ylab("") +
  ggtitle("Confidence Intervals for Conversion Rates") +
  theme_minimal() +
  theme(legend.position = "none") # Remove legend

# Adding the confidence intervals as text on the bars
p <- p + geom_text(aes(x = Variation, y = ConversionRate + CIError,
                       label = paste(sprintf('%.1f%%', Lower * 100), '-', sprintf('%.1f%%', Upper * 100))),
                   position = position_dodge(0.9), hjust = -0.2)

# Print the plot
print(p)





# Assuming normal distributions for both variations, we define the means and standard deviations
# Here we use the conversion rates and the standard error as a proxy for standard deviation
mean_A <- rate_A
sd_A <- sqrt(rate_A * (1 - rate_A) / visitors_A)
mean_B <- rate_B
sd_B <- sqrt(rate_B * (1 - rate_B) / visitors_B)

# Define the range for the x-axis
x_range <- seq(min(mean_A - 3 * sd_A, mean_B - 3 * sd_B), max(mean_A + 3 * sd_A, mean_B + 3 * sd_B), length.out = 1000)

# Create the plot
p <- ggplot(data.frame(x = x_range), aes(x)) +
  stat_function(fun = dnorm, args = list(mean = mean_A, sd = sd_A), colour = "grey") +
  stat_function(fun = dnorm, args = list(mean = mean_B, sd = sd_B), geom = "area", fill = "green", alpha = 0.5) +
  stat_function(fun = dnorm, args = list(mean = mean_B, sd = sd_B), colour = "green") +
  annotate("text", x = mean_A, y = 0, label = paste0("CR A: ", round(mean_A * 100, 2), "%"), vjust = -1, hjust = 0) +
  annotate("text", x = mean_B, y = 0, label = paste0("CR B: ", round(mean_B * 100, 2), "%"), vjust = -1, hjust = 1) +
  geom_vline(xintercept = mean_B - qnorm(0.975) * sd_B, linetype = "dashed") +
  geom_vline(xintercept = mean_B + qnorm(0.975) * sd_B, linetype = "dashed") +
  annotate("text", x = mean_B, y = dnorm(mean_B, mean_B, sd_B), label = "95%", vjust = 2) +
  labs(title = "The expected distributions of variation A and B.") +
  theme_minimal()

# Print the plot
print(p)
