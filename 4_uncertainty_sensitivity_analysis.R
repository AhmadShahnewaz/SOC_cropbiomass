# clear the R environment at the beginning
rm(list = ls())
setwd("C:/Users/shahn/Downloads/EnvEng/Summer_24/2. Ag-eco Modelling/SOC dynamics based on crop biomass")
source("1_function_crop.R")
source("2_biomass_10years.R")
source("3_function_SOC.R")

# Define the SOC model function
function_SOC <- function(IB, numberofYears, h, k1, k2) {
  # Initial carbon in the young & old pool (g C m-2)
  Yinit <- 0
  Oinit <- 0
  
  # Number of days and time step
  dt <- 1
  numSteps <- numberofYears/dt + 1
  time <- seq(0, numberofYears, dt)
  
  # Initialize the array for the young and old pool
  Ct_young <- rep(NA, numSteps)
  Ct_old <- rep(NA, numSteps)
  
  # Store the carbon content at t = 0
  Ct_young[1] <- Yinit
  Ct_old[1] <- Oinit
  
  # Euler method implementation
  for (i in 2:numSteps) {
    dY <- (IB[i] - ((1 - h) * k1 * Ct_young[i - 1]) - (h * k1 * Ct_young[i - 1])) * dt
    dO <- ((h * k1 * Ct_young[i - 1]) - (k2 * Ct_old[i - 1])) * dt
    Ct_young[i] <- Ct_young[i - 1] + dY
    Ct_old[i] <- Ct_old[i - 1] + dO
  }
  
  Ct_total <- Ct_young + Ct_old
  return(Ct_total)
}

numberofYears <- length(biomass$year)

# Uncertainty Analysis
num_simulations <- 1000
set.seed(4036290) # any random number

# Generate parameter sets between a reasonable ranges
h_values <- runif(num_simulations, min = 0.1, max = 0.3)
k1_values <- runif(num_simulations, min = 0.6, max = 1.0)
k2_values <- runif(num_simulations, min = 0.03, max = 0.07)

# Store SOC results for total biomass
SOC_results <- matrix(NA, nrow = num_simulations, ncol = numberofYears + 1)

for (i in 1:num_simulations) {
  SOC_results[i, ] <- function_SOC(biomass$total, numberofYears, h_values[i], k1_values[i], k2_values[i])
}

# soc_results is a matrix where rows represent different simulations and columns represent time steps

# summary of data frame
results_summary <- data.frame(
  year = 0:numberofYears,
  mean = apply(SOC_results, 2, mean, na.rm = TRUE),
  sd = apply(SOC_results, 2, sd, na.rm = TRUE),
  q10 = apply(SOC_results, 2, quantile, 0.1, na.rm = TRUE),
  q90 = apply(SOC_results, 2, quantile, 0.9, na.rm = TRUE)
)

# Plot the results
ggplot(results_summary, aes(x = year)) +
  geom_line(aes(y = mean), color = "blue", size = 1) +
  geom_ribbon(aes(ymin = q10, ymax = q90), alpha = 0.2) +
  labs(title = "Uncertainty Analysis of SOC Model",
       y = "Soil Organic Carbon (g C m-2)",
       x = "Time (years)") +
  theme_minimal()

# Sensitivity Analysis
correlations <- data.frame(
  yr = seq(0, numberofYears, 1),
  h = apply(SOC_results, 2, function(y) cor(y, h_values)),
  k1 = apply(SOC_results, 2, function(y) cor(y, k1_values)),
  k2 = apply(SOC_results, 2, function(y) cor(y, k2_values))
)

# Plot sensitivity analysis
par(mfrow = c(1, 3))
plot(correlations$yr, correlations$h, type = "l", col = "red", ylim = c(-1, 1), ylab = "Correlation", xlab = "Time (years)", main = "Sensitivity of h on SOC")
plot(correlations$yr, correlations$k1, type = "l", col = "blue", ylim = c(-1, 1), ylab = "Correlation", xlab = "Time (years)", main = "Sensitivity of k1 on SOC")
plot(correlations$yr, correlations$k2, type = "l", col = "chocolate", ylim = c(-1, 1), ylab = "Correlation", xlab = "Time (years)", main = "Sensitivity of k2 on SOC")


