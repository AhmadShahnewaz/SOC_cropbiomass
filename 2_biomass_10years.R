# set directory and source the function
setwd("C:/Users/shahn/Downloads/EnvEng/Summer_24/2. Ag-eco Modelling/SOC dynamics based on crop biomass")
source("1_function_crop.R")

# Load weather data
weather <- read.csv("weather_all_sites_all_years.csv")
weather <- subset(weather, idsite == 1, select = c(year, Tmax, Tmin, I))

# Parameters
Tbase <- 7.0      # Tbase  : the baseline temperature for growth (degC)
RUEmax <- 1.85    # RUE : radiation use efficiency (g.MJ-1)
K <- 0.7          # K : extinction coefficient (-)
alpha <- 0.00243  #alpha : the relative rate of leaf area index increase for small values of leaf area index ((degC.day)-1)
LAImax <- 7.0     #LAImax : maximum leaf area index (m2 leaf/m2 soil)
TTM <- 1200       #TTM :  temperature sum for crop maturity (degC.day)
TTL <- 700        #TTL : temperature sum at the end of leaf area increase (degC.day)
sdate <- 1        # sdate : sowing date
#ldate <-         # ldate : last date


# plotting and creating dataframe for biomass over 10 years
years <- 2001:2010
biomass <- data.frame(year = years, total = numeric(length(years)), stover = numeric(length(years)), root = numeric(length(years)))

for (i in 1:length(years)) {
  #specify year and last sowing date
  weather_year <- subset(weather, year == years[i])
  ldate <- nrow(weather_year)
  #run result through the function
  results <- function_maizemodel_partition(Tbase, RUEmax, K, alpha, LAImax, TTM, TTL, weather_year, sdate, ldate)
  
  #plot with legend
  plot(results$day, results$B, 
       xlab = paste0("Day of Year (Year:", years[i], ")"), 
       ylab = "Biomass", col = "red", type = "l")
  lines(results$day, results$stover_mass, col = "blue")
  lines(results$day, results$root_mass, col = "green")
  legend("topleft", legend = c("Total Biomass", "Stover Biomass", "Root Biomass"),
         col = c("red", "blue", "green"), lty = 1, cex = 0.8)
  
  #save file
  png(filename = paste0("biomass_plot_", years[i], ".png"), width = 800, height = 600)
  # Close the PNG device
  dev.off()
  
  biomass$total[i] <- max(results$B)
  biomass$stover[i] <- max(results$stover_mass)
  biomass$root[i] <- max(results$root_mass)
}

print(biomass)


# Open a PNG device for combined plot
png(filename = "combined_biomass_plot.png", width = 1600, height = 2000)

# Set up the plotting area in a 5x2 grid
par(mfrow = c(5, 2), mar = c(4, 4, 2, 1))

for (i in 1:length(years)) {
  weather_year <- subset(weather, year == years[i])
  ldate <- nrow(weather_year) 
  results <- function_maizemodel_partition(Tbase, RUEmax, K, alpha, LAImax, TTM, TTL, weather_year, sdate, ldate)
  
  plot(results$day, results$B, 
       xlab = "Day of Year", 
       ylab = "Biomass", 
       col = "red", 
       type = "l", 
       ylim = c(0, max(results$B, results$stover_mass, results$root_mass)),
       main = paste("Year:", years[i]))
  
  lines(results$day, results$stover_mass, col = "blue")
  lines(results$day, results$root_mass, col = "green")
  
  # Adding legend for the first plot
  if (i == 1) {
    legend("topright", legend = c("Total Biomass", "Stover Mass", "Root Mass"),
           col = c("red", "blue", "green"), lty = 1, cex = 0.8)
  }
  
  biomass$total[i] <- max(results$B)
  biomass$stover[i] <- max(results$stover_mass)
  biomass$root[i] <- max(results$root_mass)
}

# Close the PNG device
dev.off()

print(biomass)
