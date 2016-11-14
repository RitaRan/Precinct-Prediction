library(dplyr)
library(ggplot2)
library(sf)
library(stringr)
library(magrittr)

load("/data/nyc_parking/NYParkingViolations.Rdata")

## Get pluto data

pluto = st_read("/data/nyc_parking/pluto_manhattan/MNMapPLUTO.shp") %>%
  select(Address, geometry)

pluto_xy = cbind(
  select(pluto, Address),
  st_centroid(pluto) %>% 
    unlist() %>% 
    matrix(ncol=2,byrow=TRUE)
) %>% 
  setNames(c("address","long","lat")) %>%
  tbl_df()

ggplot(pluto_xy, aes(x=long,y=lat)) + 
  geom_point(alpha=0.1,size=0.1) +
  theme_bw()


## Merge data

valid_precincts = c(1, 5, 6, 7, 9, 10, 13, 14, 17, 18, 19, 20, 22, 23, 24, 25, 26, 28, 30, 32, 33, 34)

nyc_man = nyc %>%
  mutate(address = paste(House.Number, Street.Name)) %>%
  filter(Violation.Precinct %in% valid_precincts) %>%
  select(address, precinct = Violation.Precinct)

# Cleanup

nyc_man %<>% mutate(address = tolower(address))%>%
  mutate(address = str_replace(address," st$"," street"))%>%
  mutate(address = str_replace(address," dr$"," drive"))%>%
  mutate(address = str_replace(address,"boulevard","blvd"))%>%
  mutate(address = str_replace(address,"( pl|plc|plac|place+|plce)($| )"," place"))%>%
  mutate(address = str_replace(address,"( plz$)"," plaza"))%>%
  mutate(address = str_replace(address," (ave|av)$"," avenue"))%>%
  mutate(address = str_replace(address," (ave|av) "," avenue "))%>%
  mutate(address = str_replace(address,"avenue of (the americ|the ameri|the america|americas|american|america)",
                               "avenue of the amer"))%>%
  mutate(address = str_replace(address,"[1](st| st) street","1 street"))%>%
  mutate(address = str_replace(address,"[1](st| st) avenue","1 avenue"))%>%
  mutate(address = str_replace(address,"[2](nd| nd) ","2 "))%>%
  mutate(address = str_replace(address,"[3](rd| rd) ","3 "))%>%
  mutate(address = str_replace(address," st ", "street"))%>%
  mutate(address = str_replace(address," e "," east "))%>%
  mutate(address = str_replace(address," w "," west "))%>%
  mutate(address = str_replace(address,"([0-9]+|[0-9]+ )(th) street", 
                               na.omit(paste0(str_match(address,"([0-9]+|[0-9]+ )th street")[,2]," street"))))%>%
  mutate(address = str_replace(address,"([0-9]+|[0-9]+ )(th) avenue", 
                               na.omit(paste0(str_match(address,"([0-9]+|[0-9]+ )th avenue")[,2]," avenue"))))%>%
  mutate(address = str_replace(address,"([0-9]+|[0-9]+ )th$",
                               na.omit(paste0(str_match(address,"([0-9]+|[0-9]+ )th$")[,2]," street"))))

pluto_xy %<>% mutate(address = tolower(address))%>%
  mutate(address = str_replace(address," (boulevard|bl$)"," blvd"))%>%
  mutate(address = str_replace(address," plce"," place"))

combined = inner_join(nyc_man, pluto_xy)

#not_man = anti_join(nyc_man,pluto_xy)
#not_pluto = anti_join(pluto_xy, nyc_man)

ggplot(combined, aes(x=long,y=lat,color=factor(precinct))) + 
  geom_point(size=0.1) +
  theme_bw()

save(combined, file="precinct.Rdata")
