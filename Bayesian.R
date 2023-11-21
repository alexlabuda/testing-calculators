install.packages("BayesFactor")
library(BayesFactor)

# Example data: Group A (control) and Group B (treatment)
# Successes and Failures
n_A <- 10000  # Total number in group A
s_A <- 1984   # Successes in group A
n_B <- 10000  # Total number in group B
s_B <- 2024   # Successes in group B

# Using a uniform prior Beta(1,1)
prior <- c(1, 1)

# Calculating posterior distributions
post_A <- c(s_A + prior[1], n_A - s_A + prior[2])
post_B <- c(s_B + prior[1], n_B - s_B + prior[2])

# Simulating from the posterior distributions
set.seed(123)
sim_A <- rbeta(10000, post_A[1], post_A[2])
sim_B <- rbeta(10000, post_B[1], post_B[2])

# Probability that B is better than A
prob_B_better_than_A <- mean(sim_B > sim_A)

# Output the result
print(prob_B_better_than_A)

# Create a sequence of conversion rates
x <- seq(0, 1, length.out = 1000)

# Calculate densities
density_A <- dbeta(x, post_A[1], post_A[2])
density_B <- dbeta(x, post_B[1], post_B[2])

# Create a data frame for plotting
df <- data.frame(x, Group_A = density_A, Group_B = density_B)

# Plotting
ggplot(df, aes(x)) +
  geom_line(aes(y = Group_A, colour = "Group A"), size = 1) +
  geom_line(aes(y = Group_B, colour = "Group B"), size = 1) +
  labs(title = "Probability Density Functions of Conversion Rates",
       x = "Conversion Rate", y = "Density") +
  scale_colour_manual("", 
                      breaks = c("Group A", "Group B"),
                      values = c("Group A" = "blue", "Group B" = "orange")) +
  theme_minimal() +
  xlim(0.18, 0.22) # Adjust the x-axis range here
