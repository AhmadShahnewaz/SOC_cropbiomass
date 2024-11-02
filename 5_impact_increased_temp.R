# clear the R environment at the beginning
rm(list = ls())

# set location and sourcing all the functions
setwd("C:/Users/shahn/Downloads/EnvEng/Summer_24/2. Ag-eco Modelling/SOC dynamics based on crop biomass")
source("1_function_crop.R")
source("2_biomass_10years.R")
source("3_function_SOC.R")

weather_hightemp <- weather

# increased temperature by 2 degrees
weather_hightemp$Tmax <- weather_hightemp$Tmax + 2
weather_hightemp$Tmin <- weather_hightemp$Tmin + 2

years <- 2001:2010

# dataframe with increased temperature
biomass_ht <- data.frame(year = years, total = numeric(length(years)), stover = numeric(length(years)), root = numeric(length(years)))

# loop over the years through crop model to simulate biomass
for (i in 1:length(years)) {
  #specify year and last sowing date
  weather_year_ht <- subset(weather_hightemp, year == years[i])
  ldate <- nrow(weather_year_ht)
  #run result through the function
  results_ht <- function_maizemodel_partition(Tbase, RUEmax, K, alpha, LAImax, TTM, TTL, weather_year_ht, sdate, ldate)
  
  #plot with legend
  plot(results_ht$day, results_ht$B, 
       xlab = paste0("Day of Year (Year:", years[i], ")"), 
       ylab = "Biomass", col = "red", type = "l")
  lines(results_ht$day, results_ht$stover_mass, col = "blue")
  lines(results_ht$day, results_ht$root_mass, col = "green")
  legend("topleft", legend = c("Total Biomass", "Stover Biomass", "Root Biomass"),
         col = c("red", "blue", "green"), lty = 1, cex = 0.8)
  
  #save file
  #png(filename = paste0("biomass_plot_", years[i], ".png"), width = 800, height = 600)
  # Close the PNG device
  #dev.off()
  
  biomass_ht$total[i] <- max(results_ht$B)
  biomass_ht$stover[i] <- max(results_ht$stover_mass)
  biomass_ht$root[i] <- max(results_ht$root_mass)
}

print(biomass_ht)

numberofYears <- length(biomass_ht$year)

# dataframe from output values of SOC function 
df_ICBM_fun_ht <- data.frame(
  yr = seq(0, numberofYears, 1),
  Ct = function_SOC(biomass_ht$total, h = 0.2, k1 = 0.8, k2 = 0.05),
  Cs = function_SOC(biomass_ht$stover, h = 0.15, k1 = 0.6, k2 = 0.03),
  Cr = function_SOC(biomass_ht$root, h = 0.25, k1 = 0.9, k2 = 0.06)
)

library(ggplot2)

# combined data frame
SOC_comp_df <- rbind(
  data.frame(yr = df_ICBM_fun$yr, Ct = df_ICBM_fun$Ct, source = "Base Model"),
  data.frame(yr = df_ICBM_fun_ht$yr, Ct = df_ICBM_fun_ht$Ct, source = "Higher temp Model")
)


# Plot the impact of SOC on increased temp
SOC_comp <- ggplot(data = SOC_comp_df, aes(x = yr, y = Ct, color = source)) +
  geom_line() +
  labs(title = "Comparison of Soil Organic Carbon Over Time",
       y = expression(paste("Soil Organic Carbon (g C ", m^2, ")")),
       x = "Year") +
  theme_classic() +
  scale_color_manual(name = "legend",
                     values = c("Base Model" = "black", "Higher temp Model" = "red"))+
  theme(legend.position=c(0.15,0.825), legend.text = element_text(size=10),
        legend.background = element_rect(colour = "black", linetype = "solid"))

SOC_comp
ggsave(filename = "SOC comparison plot.png", plot = SOC_comp,
       width = 10, height = 6, dpi = 300)
