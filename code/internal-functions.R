#'
#' read_clean_DO_files
#' @description
#' This function cleans and reads in data files from PME miniDOts. First it removes metadata
#' from the top of the file and then extracts header columns columns for column names, and finally
#' collects the data and sets the column names for the file. 
#' In the future, this could be modified to read and bind multiple files.
#' @param filepath a single file path to data file location
#'
#'
read_clean_DO_files = function(filepath = NULL,...){
  # read in the column header row and remove whitespace
  xHead = read.table(filepath, skip = 7, header = FALSE, sep = ",", nrows=1) %>% unlist %>% trimws()
  # read in the units, remove whitespace, and punctuation
  xUnits = read.table(filepath, skip = 8, header= FALSE, sep = ",", nrows = 1) %>% unlist %>% trimws() %>%  gsub("[[:punct:]]", "",.)
  # read in the data columns
  xData = read.table(filepath, skip = 9, header = FALSE, sep = ",")
  
  #combine header and units
  xHeaderFull = paste(xHead,xUnits, sep = "_")
  
  # set the column names for the data
  names(xData) = xHeaderFull
  
  # optional: clean names
  xData= janitor::clean_names(xData)
  
  #return the data object
  return(xData)
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
