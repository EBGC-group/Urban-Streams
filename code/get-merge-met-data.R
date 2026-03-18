# This script is used to automate the updating of meteorological data. Currently, the script downloads data for:
# Denton Municipal Airport. Station ID:USW00003991
# ...add other stations as necessary...

# load relevant packages
library(here)
here::i_am('code/get-merge-met-data.R')
library(magrittr)
library(dplyr)
library(tidyr)
library(readr)
library(worldmet)
'%ni%' <- Negate('%in%')

# determine what the current year is to ensure we download the minimal amount of data given the constraints of the functions 
current_year = format(Sys.Date(), "%Y")

# load the current data file in /data folder

current_met_DMA = read_csv(here::here("data/met_DMA.csv"))

# download the updated meteorological data and subset columns

new_met_DMA = import_ghcn_hourly(station = "USW00003991",
                             year = current_year,
                             extra = TRUE) %>% 
  dplyr::select(station_id, station_name, date, air_temp, atmos_pres, sea_pres, altimeter, precip)

# merge the old and new data sets. Identify new date-times and append them to the old file

updated_met_DMA = new_met_DMA[which(new_met_DMA$date %ni% current_met_DMA$date),]

write_csv(updated_met_DMA,
            file = here::here("data/met_DMA.csv"),
            quote = "none",
            append = TRUE)

# create a timestamp of last successful run

saveRDS(format(Sys.Date(), "%Y-%m-%d"), here::here("data/met_DMA_timestamp.rds"))



