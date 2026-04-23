## merge depth data for class

depth = read_csv(file = "C:/Users/jj0895/OneDrive - UNT System/Ecobiogeochemistry Lab Group-Mya Hardee - Mya Hardee/Data/South Hickory/South Hickory Depth.csv") %>% 
  mutate(date = as.POSIXct(Time_CST, format = "%m/%d/%Y %H:%M"))

atmo_pres = read_csv(here::here("data/met_DMA.csv")) %>% 
  select(date, atmos_pres, sea_pres)

coef(lm(atmos_pres~sea_pres, data = atmo_pres))

x = left_join(depth, atmo_pres, by = 'date') %>%
  mutate(sea_pres = approx(date, sea_pres, date)$y) %>% 
  mutate(atmos_pres_corr = (39.936+0.9382665*sea_pres)/10) %>% 
  rename(h2o_pres = `Absolute Pressure _kPa`) %>% 
  mutate(gauge_pres = (h2o_pres - atmos_pres_corr)*1000) %>% 
  mutate(h2o_dens = 1000*(1-((Temperature_C+288.9414)/(508929.2*(Temperature_C+68.12936)))*(Temperature_C - 3.9863)^2),
         depth = gauge_pres/h2o_dens+0.17)
                                                      
                                                      
x %>% 
  ggplot()+
  geom_line(aes(x = date, y = h2o_pres), color = 'blue')+
  geom_line(aes(x = date, y = atmos_pres_corr), color = 'red')

x %>% 
  ggplot()+
  geom_line(aes(x = date, y = depth))

x %>% 
  ggplot()+
  geom_line(aes(x= date, y = Temperature_C))

y = x %>% select(`Central Standard Time` = "date", Temperature_degC = "Temperature_C", depth_m = "depth" )

write.csv(y, file = 'C:/Users/jj0895/OneDrive - UNT System/Ecobiogeochemistry Lab Group-Mya Hardee - Mya Hardee/Data/South Hickory/South Hickory Depth-temp.csv',
          quote = FALSE, row.names = FALSE)
