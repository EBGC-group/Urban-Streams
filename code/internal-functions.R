
# Metabolism Functions

# The functions below can be used to 
# - load and clean DO files (all current DO files) - Load, interpolate, clean the atmospheric pressure data - combine the two datasets - estimate DO saturation


# below packages must be loaded
library(magrittr)
library(janitor)
library(dplyr)
library(gh)
library(tidyr)
library(purrr)
library(readr)
library(lubridate)



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
  
  col_types = readr::cols(.default = "c")
  
  #return the data object
  return(xData)
}

#below, slightly modified version that assumes you are conducting on an exisiting data frame within R, not a filepath:

clean_DO_df <- function(df) {
  
  # First row = header names
  xHead <- df[1, ] %>% unlist() %>% trimws()
  
  # Second row = units
  xUnits <- df[2, ] %>% unlist() %>% trimws() %>% gsub("[[:punct:]]", "", .)
  
  # Remaining rows = data
  xData <- df[-c(1, 2), ]
  
  # Combine header + units
  xHeaderFull <- paste(xHead, xUnits, sep = "_")
  
  # Assign names
  names(xData) <- xHeaderFull
  
  # Clean names
  xData <- janitor::clean_names(xData)
  
  # Return cleaned data
  return(xData)
}



#' 
#' 
#' download_github_folder_DO
#' @description
#' This Function downloads all DO files from a GitHub folder and puts them in a local storage location.
#' @param owner The owner of the GitHub Repo. In this case, it will be EBGC-group
#' @param repo name of the repo. In this case, it will be Urban-Streams
#' @param folder folder where the data is. Right now, that is data/DOdata. In the future, it should be seperated by site
#' @param branch branch within repo. Will always be main
#' @param dest The local destination where the files will be downloaded. This is on the user's computer. Must be defined prior to using this function. Assistance is in workflow.
#' 
download_github_folder_DO <- function(owner, repo, folder, branch = "main", dest = ".") {
url <- sprintf(
  "https://api.github.com/repos/%s/%s/contents/%s?ref=%s",
  owner, repo, folder, branch
)

res <- jsonlite::fromJSON(url)

files <- res[res$type == "file", ]

if (nrow(files) == 0) stop("No files found.")

for (i in seq_len(nrow(files))) {
  message("Downloading: ", files$name[i])
  download.file(files$download_url[i],
                destfile = file.path(dest, files$name[i]),
                mode = "wb")
}

invisible(files$name)
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

clean_DO<-read_clean_DO_files("https://raw.githubusercontent.com/EBGC-group/Urban-Streams/refs/heads/main/data/2026-03-16_South%20Hickory%20DO.TXT")

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
  
#read in file
pres_model<-lm(atmos_pres~sea_pres, data=met_dma, na.action=na.exclude)
#make model
predicted_values_atmos <- predict(pres_model, newdata = met_dma)
#get predict
met_dma$predicted_atmos <- ifelse(is.na(met_dma$atmos_pres), predicted_values_atmos, met_dma$atmos_pres)
#add column
vector_atmos_pres<-met_dma |>select(predicted_atmos) |> as.vector()
atmos_double_unlist<-unlist(vector_atmos_pres) |> as.double()

vector_date<-met_dma$date
date_double_unlist<-as.double(as.POSIXct(vector_date, format = "%Y-%m-%dT%H:%M:%OSZ", tz="UTC"))
#correctly format everything from the atmospheric pressure dataset. This includes changing the text dates back into date format to prevent NA later on
valid_indices <- !is.na(date_double_unlist) & !is.na(atmos_double_unlist)
date_clean <- date_double_unlist[valid_indices]
atmos_clean <- atmos_double_unlist[valid_indices]
# ensure no NA coercion (unsure why there would be introduced NA, but got an error w/o this. Copliot wrote this step)
if (length(date_clean) == 0) {
  stop("Error: No valid data found. Check if your date column name is exactly 'date' and matches the 'date_format'.")
}
# preventing error. Copilot wrote this
s1_atmos_p_hourly<-spline(x=date_clean, y=atmos_clean, method="natural", n=6*length(date_clean)-6, xmin = min(date_clean), xmax=(max(date_clean))) 
return(s1_atmos_p_hourly) }
## get the interpolation using spline 

# Testing
splines_atmos<-edit_atmos(filepath ="https://raw.githubusercontent.com/EBGC-group/Urban-Streams/refs/heads/main/data/met_DMA.csv")
head(splines_atmos)

#Now, we need a function that can combine these with the DO data so there are atmos pressure values at each time
# The DO files all need to be combined first. 
# They are not automatically updated. new data gets put in github when that data is downloaded from the monitor, and all previous data downloads are seperate files. So, we need it to go into the project folder and combine all existing DO files


#first, need to get all files from github

#1. pulling from github
#written with copilot
download_github_folder_DO <- function(owner, repo, folder, branch = "main", dest = ".") {
  url <- sprintf(
    "https://api.github.com/repos/%s/%s/contents/%s?ref=%s",
    owner, repo, folder, branch
  )
  
  res <- jsonlite::fromJSON(url)
  
  files <- res[res$type == "file", ]
  
  if (nrow(files) == 0) stop("No files found.")
  
  for (i in seq_len(nrow(files))) {
    message("Downloading: ", files$name[i])
    download.file(files$download_url[i],
                  destfile = file.path(dest, files$name[i]),
                  mode = "wb")
  }
  
  invisible(files$name)
}
  
  
  
#using the function below. 
#To load the files onto a local location, you need to create that local folder.

# 2. creating folder to load files in
dir.create("Do_files_local", showWarnings = FALSE)

all_current_DO_files<-download_github_folder_DO(owner="EBGC-group", repo = "Urban-Streams", folder="data/DOdata", branch="main", dest="Do_files_local")
# you need to indicate a local folder you want the files downloaded to with dest=. use the one you created 


# getting the data pulled from downloads, and then combined into one df

# do not include first slash if not on mac. if on pc, use C:/Users/...

data_path <- "/Users/myahardee/Documents/GradPrograms/ENVS/UNT/Proposal/Research/GITHUB/Urban-Streams/Do_files_local"

dir(data_path)
#run this to make sure path is right 

#if for some reason it is not, run this to get your path
file.choose()



combined_current_DO_files <- 
  list.files(data_path, pattern = "\\.TXT$", full.names = TRUE) %>% 
  map_df(~ readr::read_csv(
    .x,
    col_types = readr::cols(.default = "c"),   # all columns as character
    trim_ws = TRUE                             # remove leading/trailing spaces
  ) %>% 
    mutate(source_file = basename(.x)))




combined_current_DO_files %>% glimpse()

readLines("/Users/myahardee/Documents/GradPrograms/ENVS/UNT/Proposal/Research/GITHUB/Urban-Streams/Do_files_local/2026-03-16_South Hickory DO.TXT", n = 20)


#now, we still have errors with how the file is showing up. colums are not seperated, and there is the stuff at the top of the file we need to get rid of. So lets use the code at the beginning of the document to clean (clean_DO_df).


cleaned_DO <- combined_current_DO_files %>%
  group_split(source_file) %>% 
  map_df(~ clean_DO_df(.x) %>% mutate(source_file = unique(.x$source_file)))

cleaned_combined_DO<-clean_DO_df(combined_current_DO_files)


# issue: looks like all files have to be cleaned and then binded together for them to show up correctly. With this fix:


cleaned_DO_list <- 
  list.files(data_path, pattern = "\\.TXT$", full.names = TRUE) %>%
  map(~ read_clean_DO_files(.x) %>% mutate(source_file = basename(.x)))

combined_current_DO_files <- bind_rows(cleaned_DO_list)



#this gets us the combined existing DO files

#Now, we need to combine this with the atmos pressure data file, so that we can then have pressures for all the do measurements

# this will involve 
  #1. getting the dates from the splines_atmos back into y-m-d h:m:s format
  #2. Making sure the times actually line up so that the columns will line up with each other 
  #3. putting the two datasets together
  

#1. 
splines_atmos$x |> as.POSIXct(format = "%Y-%m-%dT%H:%M:%OSZ", tz="UTC")

# this fixed it, but now we see that there are only 4 time steps between each hour, ie an estimate every 12 minutes. 
#we need one every ten. so we need to re-write the formula for the splines.

# I changed the formula to give it 6 values between each value. BUT, it seems like it is not exactly ten minutes, 
# and the offset gets worse as the time progresses.


# So, I can try this change that copilot suggested and see if that does anything 

# 2. Define the exact 10-minute target sequence 
# We span from the first to the last hour in 10-minute intervals (600 seconds)
original_times<-mettest$date
target_times_atmos <- seq(from = min(original_times), 
                    to   = max(original_times), 
                    by   = 600) # 600 seconds = 10 minutes

#then perform spline 

splines_atmos_2<- spline(x=as.numeric(original_times), y=mettest$atmos_pres, xout = as.numeric(target_times_atmos)) 

# now get data frame 

formatted_dates<-splines_atmos_2$x |> as.POSIXct(format = "%Y-%m-%dT%H:%M:%OSZ", tz="UTC") 
splines_2_df<-data.frame(date = formatted_dates, atmos_pressure_interp=splines_atmos_2$y)
# great, this works, so we will go with this
head(splines_2_df)

# using: splines_2_df

#2

#Now, in order to combine the atmos pres data with the DO, I have to get the times to line up. It shows that the current
#DO data I have is taking measurements every ten minutes, but started off of an even 10 (ie. it started at 51 min)  
# The code has to shift these numbers backwards or forwards, and also has to do so for multiple downloads that may have started recording at a different offset.


?lag()
# this code could prove useful some other time from the dplyr package, but not here, as it lags based on
# a number of observations 

?round.Date()
?round(.leap.seconds)

# probably not the most useful

# LUBRIDATE package could be useful here though
#(from stack overflow)

# 

DO_dates_seq<-seq(as.POSIXct(min(combined_current_DO_files$utc_date_time_none)), as.POSIXct(max(combined_current_DO_files$utc_date_time_none)), by=600) #seconds in 10 min
DO_dates_rounded<-round_date(DO_dates_seq, unit="10 mins")

# ok, now we need to combine this with the combined_current_DO_files dataset, making it a new column

Do_dates_and_adjusteddf<- data.frame(DO_dates_seq, DO_dates_rounded) 

Do_dates_and_adjusteddf

# join with the combined current DO files df

? full_join()


combined_current_DO_files$utc_date_time_none <- as.POSIXct(combined_current_DO_files$utc_date_time_none, 
                             format = "%Y-%m-%d %H:%M:%S", 
                             tz = "UTC")


joined_by<- join_by(DO_dates_seq==utc_date_time_none)

combined_current_DO_files_join<-left_join( Do_dates_and_adjusteddf, combined_current_DO_files, joined_by) |> select(2:10)
#joining and only keeping needed columns
head(combined_current_DO_files_join)

# now it is ready to combine with atmos data

# 3. combine with atmos

# using the splines_2_df for the pressure data, and then merging with the combined_current_DO_files_join. need to make a join by

combined_current_DO_files_join$DO_dates_rounded<-as.POSIXct(combined_current_DO_files_join$DO_dates_rounded)
splines_2_df$date<-as.POSIXct(splines_2_df$date)
combined_current_DO_files_join$central_standard_time_none<-as.POSIXct(combined_current_DO_files_join$central_standard_time_none)



combine_atmos_and_DO<- full_join(combined_current_DO_files_join, splines_2_df, by=joined_by_2)
  
combine_atmos_and_DO



# (Use "America/Chicago" to handle Central Time properly)
splines_2_df <- splines_2_df %>%
  mutate(date = with_tz(date, tzone = "America/Chicago"))

combined_current_DO_files_join <- combined_current_DO_files_join %>%
  mutate(DO_dates_rounded = force_tz(DO_dates_rounded, tzone = "America/Chicago"))
#setting time zones so line up works

joined_by_2 <- join_by(DO_dates_rounded == date)
  
combine_atmos_and_DO <- full_join(
  combined_current_DO_files_join, 
  splines_2_df, 
  by = joined_by_2)

# WE CAN MAKE functions that do steps and then combine all

# First, pull DO from Github

#
#

# to use the below function, you first need to define where you want the files to be placed locally on the computer.
##. creating folder to load files in
dir.create("Do_files_local", showWarnings = FALSE)

#using function to load all files into the folder 
all_current_DO_files<-download_github_folder_DO(owner="EBGC-group", repo = "Urban-Streams", folder="data/DOdata", branch="main", dest="Do_files_local")
# you need to indicate a local folder you want the files downloaded to with dest=. use the one you created 


#
download_github_folder_DO <- function(owner, repo, folder, branch = "main", dest = ".") {
url <- sprintf(
  "https://api.github.com/repos/%s/%s/contents/%s?ref=%s",
  owner, repo, folder, branch
)

res <- jsonlite::fromJSON(url)

files <- res[res$type == "file", ]

if (nrow(files) == 0) stop("No files found.")

for (i in seq_len(nrow(files))) {
  message("Downloading: ", files$name[i])
  download.file(files$download_url[i],
                destfile = file.path(dest, files$name[i]),
                mode = "wb")
}

invisible(files$name)
}

#Next, combine the DO files together and clean them, and round the times to the nearest ten minutes so it lines up with the atmos
#

## you have to define your data path first OR just write it into the function directly 
data_path_1 <- "/Users/myahardee/Documents/GradPrograms/ENVS/UNT/Proposal/Research/GITHUB/Urban-Streams/Do_files_local"

cleaned_DO_list <- function(data_path) {combined_current_DO_files<-
  {list.files(data_path, pattern = "\\.TXT$", full.names = TRUE) %>%
  map(~ read_clean_DO_files(.x) %>% mutate(source_file = basename(.x))) %>% data.frame %>% bind_rows()
  }
  combined_current_DO_files$utc_date_time_none <- as.POSIXct(combined_current_DO_files$utc_date_time_none, 
                                                           format = "%Y-%m-%d %H:%M:%S", 
                                                           tz = "UTC")
  #making sure time is correct format
  DO_dates_seq<-seq(min(combined_current_DO_files$utc_date_time_none, na.rm = T), max(combined_current_DO_files$utc_date_time_none, na.rm=T), by=600) #seconds in 10 min
  DO_dates_rounded<-round_date(DO_dates_seq, unit="10 mins")
  Do_dates_and_adjusteddf<- data.frame(DO_dates_seq, DO_dates_rounded) %>% mutate(DO_dates_rounded = with_tz(DO_dates_rounded, tzone = "America/Chicago"))
  
   #getting it into central time will help with joining with atmos data later
  
  joined_by<- join_by(DO_dates_seq==utc_date_time_none)
  
  combined_current_DO_files_join<-left_join( Do_dates_and_adjusteddf, combined_current_DO_files, joined_by) |> select(2:10)
  #joining and only keeping needed columns
  
  return(combined_current_DO_files_join)
}


testDO<-cleaned_DO_list(data_path = data_path_1)


#Next, use the function below to get the atmos data and clean it up (predict and interpolate)

# this is an edited version of my original function, which fixes my issue of the interpolation not being at the right time steps


edit_atmos <- function(filepath = NULL, ...) {
  # Check if filepath is provided
  if (is.null(filepath)) {
    stop("Please provide a valid file path or URL.")
  }
  
  # Read the data directly from the path/URL
  met_dma <- read.csv(filepath, ...)
  
  # Convert text dates to POSIXct object first (used to establish the true timeline boundaries)
  vector_date_posix <- as.POSIXct(met_dma$date, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
  
  # ----------------------------------------------------------------------
  # CRITICAL FIX: Base the timeline sequence on the FULL dataset boundaries
  # ----------------------------------------------------------------------
  target_times <- seq(
    from = min(vector_date_posix, na.rm = TRUE), 
    to   = max(vector_date_posix, na.rm = TRUE), 
    by   = 600  # 600 seconds = 10 minutes
  )
  
  # Read in file and build the predictive model
  pres_model <- lm(atmos_pres ~ sea_pres, data = met_dma, na.action = na.exclude)
  predicted_values_atmos <- predict(pres_model, newdata = met_dma)
  
  # Replace missing atmospheric pressure with predicted values
  met_dma$predicted_atmos <- ifelse(is.na(met_dma$atmos_pres), predicted_values_atmos, met_dma$atmos_pres)
  vector_atmos_pres <- met_dma$predicted_atmos
  
  # Filter out rows with missing data ONLY for the spline model inputs
  valid_indices <- !is.na(vector_date_posix) & !is.na(vector_atmos_pres)
  date_clean_posix <- vector_date_posix[valid_indices]
  atmos_clean <- vector_atmos_pres[valid_indices]
  
  # Error handling for empty data
  if (length(date_clean_posix) == 0) {
    stop("Error: No valid data found. Check if your date column name is exactly 'date' and matches the format.")
  }
  
  # Perform natural spline evaluation across the full target range
  s1_atmos_p_hourly <- spline(
    x = as.numeric(date_clean_posix), 
    y = atmos_clean, 
    method = "natural", 
    xout = as.numeric(target_times)
  ) 
  
  # Convert the numeric x output back to a readable POSIXct datetime object 
  s1_atmos_p_hourly$x <- as.POSIXct(s1_atmos_p_hourly$x, origin = "1970-01-01", tz = "UTC")
  
  atmos_splines_df<-data.frame(date = s1_atmos_p_hourly$x, atmos_pressure_interp=s1_atmos_p_hourly$y)
  atmos_splines_df<- splines_2_df %>%
    mutate(date = with_tz(date, tzone = "America/Chicago"))
  
  # make a data frame. convert times so alining with other data sets later is easier 
  
  return(atmos_splines_df) 
}


splines_2_df <- splines_2_df %>%
  mutate(date = with_tz(date, tzone = "America/Chicago"))

# Testing
test_atmos<-edit_atmos(filepath ="https://raw.githubusercontent.com/EBGC-group/Urban-Streams/refs/heads/main/data/met_DMA.csv")



#after this, you should be able to join the two files together, and then perform analysis.

# example: 


joined_by_2 <- join_by(DO_dates_rounded == date)

combine_atmos_and_DO <- full_join(
  testDO, test_atmos,
  by = joined_by_2)


# Then, below, we can use the function to get the saturation estimates and combine with our data.


  
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
