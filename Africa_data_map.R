library(utils)
library(httr)
library(tidyverse)
library(magrittr)
library(sf)

#download the dataset from the ECDC website to a local temporary file
GET("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", authenticate(":", ":", type="ntlm"), write_disk(tf <- tempfile(fileext = ".csv")))

#read the Dataset sheet into “R”. The dataset will be called "data".
data <- read.csv(tf)

#African countries
african_countries <- read.csv("africa_country_iso3.csv")
african_iso3_codes <- as.character(african_countries$ISO3) 

#filter data to only include Africa continent
africa_data <- data %>% filter(countryterritoryCode %in% african_iso3_codes)

#Convert date to actual date
africa_data$dateRep <- as.Date(africa_data$dateRep, "%d/%m/%Y")

#Calculate cumulative cases & deaths 
africa_data %<>% group_by(countryterritoryCode) %>% arrange(countryterritoryCode,dateRep) %>% mutate(cumCases = cumsum(cases), cumDeaths=cumsum(deaths))

#test plot
ggplot(africa_data %>% filter(dateRep > "2020-03-01"), aes(x=dateRep, y=cumCases, colour=countriesAndTerritories)) + geom_line(size=1.05) + theme(legend.position = "none")

#create map with latest cumulative cases
cases_by_country <- sf::st_read("Africa.geojson") %>%
  st_transform(crs = st_crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
