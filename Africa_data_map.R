library(utils)
library(httr)
library(tidyverse)
library(magrittr)
library(sf)
library(leaflet)
library(RColorBrewer)
library(classInt)
library(broom)
library(viridis)

setwd("C:/temp/covid-19/Africa/")
#download the dataset from the ECDC website to a local temporary file
GET("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", authenticate(":", ":", type="ntlm"), write_disk(tf <- tempfile(fileext = ".csv")))

#read the Dataset sheet into “R”. The dataset will be called "data".
data <- read.csv(tf, stringsAsFactors = F)

#African countries
african_countries <- read.csv("africa_country_iso3.csv")
african_iso3_codes <- african_countries$ISO3 

#filter data to only include Africa continent
africa_data <- data %>% 
  filter(countryterritoryCode %in% african_iso3_codes)


#Convert date to actual date
africa_data$dateRep <- as.Date(africa_data$dateRep, "%d/%m/%Y")

#Calculate cumulative cases & deaths 
africa_data %<>% 
  group_by(countryterritoryCode) %>% 
  arrange(countryterritoryCode,dateRep) %>% 
  mutate(cumCases = cumsum(cases), cumDeaths=cumsum(deaths))

africa_data %<>% left_join(african_countries, by=c("countryterritoryCode"="ISO3"))

#test plot
ggplot(africa_data %>% filter(dateRep > "2020-03-01"), aes(x=dateRep, y=cumCases)) + 
  geom_line(col="DarkBlue", size=1.05) + 
  facet_wrap(~countriesAndTerritories) +
  scale_y_log10()

maxima <- africa_data %>% 
  group_by(countryterritoryCode) %>% 
  filter(cumCases==max(cumCases),.preserve = T) %>% 
  top_n(1,dateRep)

#create map with latest cumulative cases
cases_by_country <- sf::st_read("Africa.geojson") %>%
  st_transform(crs = st_crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

cases_by_country <- cbind(cases_by_country, st_coordinates(st_centroid(cases_by_country)))

cases_by_country %<>% left_join(maxima[,c(9:13)], by=c("CODE"="Map_Code"))

cases_by_country$cumCases[is.na(cases_by_country$cumCases)] <- 0

cases_by_country$cases_popup <- paste(cases_by_country$COUNTRY, cases_by_country$cumCases, "cases", sep = " ")

breaks <- classIntervals(cases_by_country$cumCases, n = 9, style = "jenks")$brks
pal <- colorBin(palette = "Blues", domain = NULL, bins = breaks, na.color = "#FFFFFF")

leaflet(data=filter(cases_by_country, cumCases!=0)) %>%
  #setView(-3, 54.3, zoom = 5) %>% 
  addTiles(urlTemplate = "",
           attribution = 'Copyright Scottish Government, contains Ordnance Survey data © Crown copyright and database right (2019)', options = providerTileOptions(minZoom = 1, maxZoom = 13)) %>%
  addPolygons(data = filter(cases_by_country, cumCases == 0), fillColor = "#FFFFFF", fillOpacity = 1, weight = 1, color = "#bdbdbd", label = ~cases_popup, labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "15px", direction = "auto"), highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE)) %>% 
  addPolygons(fillColor = ~pal(cumCases), fillOpacity = 0.8, smoothFactor = 0.5, stroke = TRUE, weight = 1, color = "#bdbdbd", opacity = 1, label = ~cases_popup, labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "15px", direction = "auto"), highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE)) %>%
  addLegend(pal=pal, values=~cumCases, position = "bottomleft", opacity = 0.8, title = "Latest total cumulative cases")



#Other approach, not finished yet...
library(geojsonio)
africa <- geojson_read("Africa1.geojson", what="sp")

africa@data %<>% left_join(maxima[,c(9:13)], by=c("ISO_A3"="countryterritoryCode"))

africa@data$cumCases[is.na(africa@data$cumCases)] <- 0

library(cartography)
breaks <- classIntervals(africa@data$cumCases, n = 9, style = "jenks", na.rm=T)$brks
pal <- brewer.pal(9, name = "Blues")
choroLayer(spdf = africa, var = "cumCases", method="fisher-jenks", nclass = 9, legend.title.txt = "Cumulative Cases")
labelLayer(spdf=africa, txt = "cumCases",col= "black", cex = 0.7,halo = TRUE, bg = "white", r = 0.1, show.lines = T, overlap = F)
title("Latest Cumulative Cases per Country")

africa@data$cumCases

#Dt calculations
Dt_Cases = africa_data %>% group_by(countriesAndTerritories) %>% arrange(countriesAndTerritories,dateRep) %>% mutate(Dt=7*log(2)/log(nth(cumCases,-1)/nth(cumCases,-8))) %>% summarise(Dt=max(Dt))
Dt_Deaths = africa_data %>% group_by(countriesAndTerritories) %>% arrange(countriesAndTerritories,dateRep) %>% mutate(Dt=7*log(2)/log(nth(cumDeaths,-1)/nth(cumDeaths,-8))) %>% summarise(Dt=max(Dt))

