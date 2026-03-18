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

atmos_lm = lm(atmos_pres ~ sea_pres, data = met_data)
atmos_pred = data.frame(est_pres = predict(atmos_lm, newdata = data.frame(sea_pres = met_data$sea_pres)))

met_data = met_data %>% 
  bind_cols(atmos_pred)


met_data %>% 
  ggplot()+
  geom_line(aes(x = date, y = sea_pres))+
  geom_line(aes(x = date, y = est_pres), color = 'blue')
