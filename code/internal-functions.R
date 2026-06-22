
library(magrittr)
library(janitor)
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
  xHead = read.table(filepath, skip = 7, header = FALSE, sep = ",", nrows=1) %>% unlist() %>% trimws()
  # read in the units, remove whitespace, and punctuation
  xUnits = read.table(filepath, skip = 8, header= FALSE, sep = ",", nrows = 1) %>% unlist() %>% trimws() %>%  gsub("[[:punct:]]", "",.)
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

# Practice with this function




# 1. Define the GitHub URL
github_url <- "https://raw.githubusercontent.com/EBGC-group/Urban-Streams/refs/heads/main/data/2026-03-16_South%20Hickory%20DO.TXT"

# 2. Create a local name for the file inside your working directory
local_filename <- "2026-03-16_South_Hickory_DO.txt"

# 3. Securely download the online text file to your local computer
download.file(url = github_url, destfile = local_filename, mode = "wb")

# 4. Pass the brand new local file straight to your function
clean_data <- read_clean_DO_files(filepath = local_filename)

read_clean_DO_files("https://raw.githubusercontent.com/EBGC-group/Urban-Streams/refs/heads/main/data/2026-03-16_South%20Hickory%20DO.TXT")

## What we need to do next (so that the estimate DO sat can be run)
  # get the temperature data and the pressure data lined up with the DO data. (time intervals)

# Reading in the met_DMA file

library(readr)
library(dplyr)
mettest<-read_csv(file="https://raw.githubusercontent.com/EBGC-group/Urban-Streams/refs/heads/main/data/met_DMA.csv")

#It is hourly. We can thus interpolate between the hours to get ten minute interval estimates 

#HOWEVER, we also know that we are missing some of the hourly measurements that should be there. At some point, the sensor stopped taking atmospheric measurements. But, we previously determined
#that there is a linear relationship (corr) between atmospheric pressure and sea pressure. Basically, it is the simple 1:1 relationship but shifted a bit (we are not at sea level)

mettest[1:100,] |> select(atmos_pres, sea_pres) |>plot()

#So, we just need to get a line fit through this data and then we can use that to predict the values we have missing 

library(lme4)
pres_model<-lm(atmos_pres~sea_pres, data=mettest, na.action.na.exclude)


summary(pres_model)

library(ggplot2)

# Format: ggplot(data, aes(x = independent_variable, y = dependent_variable))
ggplot(mettest, aes(x = sea_pres, y =atmos_pres)) +
  geom_point() +                          # Adds the data points
  geom_smooth(method = "lm", se = TRUE)   # Adds the slope line (se = TRUE adds confidence intervals)



#NOW: need to code so that I can have all the NA in the pres column filled with the predicted values here 



predicted_values_atmos <- predict(pres_model, newdata = mettest)
# generating predicted values for all atmos pressure values, using the model

mettest$predicted_atmos <- ifelse(is.na(mettest$atmos_pres), predicted_values_atmos, mettest$atmos_pres)
# making a new column, replacing NA values from original atmos pressure with new predicted values


# Why it works: ifelse() automatically matches the row indices. If row 5 of atmos_pres is missing, it will seamlessly grab row 5 of pres_model$fitted.values.



#THEN...

# Interpolating:

mettest |> select(atmos_pres,date) |>plot()
mettest_filter<-mettest[1:24,]
mettest_filter |> select(atmos_pres, date) |> plot()
mettest_filter_2 <-mettest[49:72,]
mettest_filter_2 |>select(atmos_pres, date) |>plot()

# Looks like it wont be linear, but if we want to use linear the functions would be: 

?approx() # or 
?approxfun()

#Becuase it seems nonlinear in pattern, spline interpolation can give us the flexibility we need 
?spline()
?splinefun()

# test it out:

vector_atmos_pres<-mettest |>select(atmos_pres) |> as.vector()
atmos_double_unlist<-unlist(vector_atmos_pres) |> as.double()
vector_date<-mettest |> select (date) |> as.vector()
date_double_unlist<-unlist(vector_date) |> as.double()

# splinefun
sf1<-splinefun(date_double_unlist, atmos_double_unlist, method="natural")
sf1

#splinefun returns a function which will perform cubic spline interpolation of the given data points
##However, it does not let me specify the intervals at which I want data points interpolated


# spline

s1_atmos_p_hourly<-spline(date_double_unlist, atmos_double_unlist, method="natural", n=5*length(date_double_unlist)-5, xmin = min(date_double_unlist), xmax=(max(date_double_unlist)))
s1_atmos_p_hourly
#spline just returns the list of the interpolated values. We can mesh this with the table of DO values later.  n= 5*length(date_double_unlist)-5 becuase 
#we want 5 equally spaced intervals for interpolation between each existing time that we already have.


plot(s1) # unsure why it is giving negative values for some of the pressures...
mettest |> select(date, atmos_pres) |>plot()  


# Now, lets write a function that combines all changes to the met dma file 
 
edit_atmos<-function(filepath = NULL,...){
# Check if filepath is provided
  if (is.null(filepath)) {
    stop("Please provide a valid file path or URL.")
  }
  
  # Read the data directly from the path/URL
  # (Change read.csv to your preferred file reader like readLines or read.table)
  met_dma <- read.csv(filepath, ...)
  
  # Perform your edits on the data here
  # ...
  
  return(data)
#read in file
pres_model<-lm(atmos_pres~sea_pres, data=met_dma, na.action.na.exclude)
#make model
predicted_values_atmos <- predict(pres_model, newdata = met_dma)
#get predict
met_dma$predicted_atmos <- ifelse(is.na(met_dma$atmos_pres), predicted_values_atmos, met_dma$atmos_pres)
#add column
vector_atmos_pres<-met_dma |>select(atmos_pres) |> as.vector()
atmos_double_unlist<-unlist(vector_atmos_pres) |> as.double()
vector_date<-met_dma |> select (date) |> as.vector()
date_double_unlist<-unlist(vector_date) |> as.double()
#correctly format everything from the atmospheric pressure dataset
s1_atmos_p_hourly<-spline(date_double_unlist, atmos_double_unlist, method="natural", n=5*length(date_double_unlist)-5, xmin = min(date_double_unlist), xmax=(max(date_double_unlist))) }
## get the interpolation using spline 

# Testing
edit_atmos(filepath ="https://raw.githubusercontent.com/EBGC-group/Urban-Streams/refs/heads/main/data/met_DMA.csv")

?read.csv

  


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
