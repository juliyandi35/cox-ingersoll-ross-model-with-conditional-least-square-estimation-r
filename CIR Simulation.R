# Load required libraries
library(ggplot2)

# Function for CIR interest rate Monte Carlo simulation with standard normal shocks
cir_simulation <- function(theta, alpha, sigma, steps) {
  dt <- 1  # Time increment
  
  r <- numeric(steps)  # Vector to store simulated interest rates
  r[1] <- theta  # Initial interest rate
  
  for (i in 2:steps) {
    # Generate standard normal shock term
    dW <- rnorm(1, mean = 0, sd = 1)
    
    # Calculate next interest rate using CIR model equation
    gamma <- sqrt(alpha^2 + 2 * sigma^2)
    psi <- (gamma + alpha) * dt
    
    z <- dW^2
    r[i] <- r[i - 1] + (alpha * (theta - r[i - 1]) * dt) + (sigma * sqrt(r[i - 1]) * sqrt(dt) * sqrt(z) / sqrt(psi))
  }
  
  return(r)
}

# Set model parameters
theta <- 0.01  # Long-term mean or equilibrium interest rate
alpha <- 0.05  # Speed of mean reversion
sigma <- 0.1  # Volatility

# Set number of simulation steps
steps <- 1000

# Perform CIR Monte Carlo simulation with standard normal shocks
simulated_rates <- cir_simulation(theta, alpha, sigma, steps)

# Create time vector
time <- 1:steps

# Plot simulated interest rates
plot_data <- data.frame(Time = time, Rate = simulated_rates)
ggplot(plot_data, aes(x = Time, y = Rate)) +
  geom_line() +
  xlab("Time") +
  ylab("Interest Rate") +
  ggtitle("CIR Interest Rate Monte Carlo Simulation")