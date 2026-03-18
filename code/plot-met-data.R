library(here)
here::i_am('code/plot-met-data.R')
library(magrittr)
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
theme_set(theme_minimal())

# read in the meteorological data from Denton Municipal Airport
met_data <- read_csv(here('data/met_DMA.csv'))

# # convert sea pressure to local atmospheric pressure
# met_data = met_data %>% 
#   dplyr::mutate(est_pres = (sea_pres))

met_data %>% 
  ggplot()+
  geom_line(aes(x = date, y = sea_pres))
