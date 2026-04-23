#'
#'
#'
#'
read_clean_DO_files = function(filepath = NULL,...){
  
  
}


#'
#'
#'
read_clean_depth_files = function(filepath = NULL){
  
  
}

#'
#'
#'
read_clean_cond_files = function(filepath = NULL){
  
  
}

#' @title estimate_DO_sat
#' @description
#' This function calculates the dissolved oxygen saturation point with a correction for 
#' both temperature in Celsius and atmospheric pressure. Atmospheric pressure is assumed 
#' to be given in hectoPascals, as this is what the data from the meterological station
#' returns. 
#' @param temp_C This is the temperature measured in degrees Celsius
#' @param patm_hPa This is the station atmospheric pressure in hectoPascals.
#' If we convert to kPa we will need to change the 1013.25 in the ratio to kPa.
#'
#'
estimate_DO_sat = function(temp_C, patm_hPa = 1013.25){
    # Convert inputs
    T <- temp_C + 273.15  # Convert Celsius to Kelvin
    patm_atm <- patm_hPa / 1013.25 #Determine the ratio of observed atmospheric pressure to standard pressure
    
    # Benson & Krause (1984) coefficients (freshwater, mg O2/L)
    A1 <- -139.34411
    A2 <-  1.575701e5
    A3 <- -6.642308e7
    A4 <-  1.243800e10
    A5 <- -8.621949e11
    
    # ln(C*) at 1 atm
    lnC <- A1 +
      A2 / T +
      A3 / T^2 +
      A4 / T^3 +
      A5 / T^4
    
    C_star_1atm <- exp(lnC)  # mg/L
    
    # Scale by atmospheric pressure
    C_star <- C_star_1atm * patm_atm
    
    return(C_star)
}
