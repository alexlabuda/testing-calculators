# Install and load necessary package
if (!require("stats")) install.packages("stats")
library(stats)

# Observed numbers
observed_A <- 1000
observed_B <- 889
total_observed <- observed_A + observed_B

# Expected proportions (assuming 50/50 split)
expected_proportion_A <- 0.5
expected_proportion_B <- 0.5

# Observed proportions
observed_proportion_A <- observed_A / total_observed
observed_proportion_B <- observed_B / total_observed

# Standard Error calculation
SE <- sqrt(expected_proportion_A * (1 - expected_proportion_A) / total_observed)

# SRM calculation for each group
SRM_A <- (observed_proportion_A - expected_proportion_A) / SE
SRM_B <- (observed_proportion_B - expected_proportion_B) / SE

# Calculate p-values
p_value_A <- 2 * pnorm(-abs(SRM_A))
p_value_B <- 2 * pnorm(-abs(SRM_B))

# Output the SRM values and p-values
cat("SRM for Variation A:", SRM_A, "\n")
cat("SRM for Variation B:", SRM_B, "\n")
cat("p-value for Variation A:", p_value_A, "\n")
cat("p-value for Variation B:", p_value_B, "\n")

# Check for SRM detection
if (p_value_A > 0.01) {
  cat("No SRM Detected for Variation A\n")
} else {
  cat("SRM Detected for Variation A\n")
}

if (p_value_B > 0.01) {
  cat("No SRM Detected for Variation B\n")
} else {
  cat("SRM Detected for Variation B\n")
}