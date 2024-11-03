# clear the R environment at the beginning
rm(list = ls())

# set Project location to access files if no R project is created
getwd()
setwd()

# Function to simulate maize growth with partitioning
function_maizemodel_partition <- function(Tbase, RUE, K, alpha, LAImax, TTM, TTL, weather_year, sdate, ldate) {
  
  # placement of thermal temp, Total biomass and Leaf area index
  TT <- rep(NA, ldate)    
  B <- rep(NA, ldate)
  LAI <- rep(NA, ldate)
  
  # placement of new structures for stover, grains and roots
  stover_mass <- rep(NA, ldate)
  grains_mass <- rep(NA, ldate)
  root_mass <- rep(NA, ldate)
  
  # Initial values
  TT[sdate] <- 0
  B[sdate] <- 1
  LAI[sdate] <- 0.01
  stover_mass[sdate] <- 0.01
  grains_mass[sdate] <- 0
  root_mass[sdate] <- 0
  
  for (day in sdate:(ldate - 1)) {
    # daily ther
    dTT <- max((weather_year$Tmin[day] + weather_year$Tmax[day]) / 2 - Tbase, 0)
    
    # conditions for Total biomass
    if (TT[day] <= TTM) {
      dB <- RUE * (1 - exp(-K * LAI[day])) * weather_year$I[day]
    } else {
      dB <- 0
    }
    
    # conditions for dLAI and biomass partitioning
    if (TT[day] <= TTL) {
      dLAI <- alpha * dTT * LAI[day] * max(LAImax - LAI[day], 0)
      d_stover <- 0.79 * dB
      d_grains <- 0.01 * dB
      d_roots <- 0.2 * dB
      
    } else if (TT[day] <= TTM) {
      dLAI <- 0
      d_stover <- 0
      d_grains <- dB * 0.99
      d_roots <- dB*0.01
    } else {
      dLAI <- 0
      d_stover <- 0
      d_grains <- 0
      d_roots <- 0
    }
    
    TT[day + 1] <- TT[day] + dTT
    B[day + 1] <- B[day] + dB
    LAI[day + 1] <- LAI[day] + dLAI
    stover_mass[day + 1] <- stover_mass[day] + d_stover
    grains_mass[day + 1] <- grains_mass[day] + d_grains
    root_mass[day + 1] <- root_mass[day] + d_roots
  }
  
  # a dataframe containing all the calculated values
  return(data.frame(day = sdate:ldate, 
                    TT = TT[sdate:ldate], 
                    LAI = LAI[sdate:ldate], 
                    B = B[sdate:ldate], 
                    stover_mass = stover_mass[sdate:ldate], 
                    grains_mass = grains_mass[sdate:ldate],
                    root_mass = root_mass[sdate:ldate])
         )
}

