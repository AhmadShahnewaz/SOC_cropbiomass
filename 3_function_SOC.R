#function to simulate Soil organic carbon (SOC) model 

function_SOC <- function(IB, #different biomass input
                         numberofYears, 
                         h, 
                         k1, 
                         k2)
  
{
  #daily biomass(c) input (g C m-2 day-1)
  #IB =   
  # coefficient for total 
  h = 0.2 # The humification coefficient
  k1 = 0.8 # The decay constant for the young pool
  k2 = 0.05 # The decay constant for the old pool
  
  # The initial carbon in the young & old pool (g C m-2)
  Yinit = 0 
  Oinit = 0
  
  # number of days and time step
  numberofYears <- length(biomass$year)
  dt <- 1
  numSteps <- numberofYears/dt + 1
  time <- seq(0,numberofYears,dt)
  
  #Initialize the array for the young and old pool
  Ct_young <- rep(NA,numSteps) 
  Ct_old <- rep(NA,numSteps)
  ## Store the carbon content at t = 0
  Ct_young[1] <- Yinit
  Ct_old[1] <- Oinit
  
  #The model is implemented in a for-loop, in which the Euler method is applied:
  # A loop from the second time step onwards
  
  for(i in 2:numSteps){
    
    #daily change in the carbon of young pool and old pool
    dY <- (IB[i] - ((1 -h) * k1 * Ct_young[i-1]) - (h * k1 * Ct_young[i-1])) * dt
    dO <- ((h * k1 * Ct_young[i-1]) - (k2 * Ct_old[i-1])) * dt
    
    Ct_young[i] <- Ct_young[i-1] + dY
    Ct_old[i] <- Ct_old[i-1] + dO
    
    #total carbon
    Ct_total <- Ct_young + Ct_old
  }
  
  return(Ct_total)
  
}

#creating dataframe and plot of Soil organic carbon (SOC) from total, stover and root biomass, over 10 years

numberofYears <- length(biomass$year)
df_ICBM_fun <- data.frame(
  yr = seq(0, numberofYears, 1),
  Ct = function_SOC(biomass$total, h = 0.2, k1 = 0.8, k2 = 0.05),
  Cs = function_SOC(biomass$stover, h = 0.15, k1 = 0.6, k2 = 0.03),
  Cr = function_SOC(biomass$root, h = 0.25, k1 = 0.9, k2 = 0.06)
)

library(ggplot2)
ggplot() +
  geom_line(data = df_ICBM_fun, aes(x = yr, y = Ct, color = "SOC from Total Biomass")) +
  geom_line(data = df_ICBM_fun, aes(x = yr, y = Cs, color = "SOC from Stover")) +
  geom_line(data = df_ICBM_fun, aes(x = yr, y = Cr, color = "SOC from Roots")) +
  labs(title="Simulated carbon using the ICBM model",
       y=expression(paste("Soil organic Carbon (g C ", mˆ-2, ")")),
       x="Time (years)") +
  theme_classic() +
  scale_color_manual(name = NULL, values = c("SOC from Total Biomass" = "cadetblue" ,
                                             "SOC from Stover" = "chocolate",
                                             "SOC from Roots" = "blue"
  )) +
  theme(legend.position=c(0.15,0.825), legend.text = element_text(size=10),
        legend.background = element_rect(colour = "black", linetype = "solid"))

