library(utils)
library(httr)
library(tidyverse)
library(magrittr)
library(sf)
library(RColorBrewer)
library(classInt)
library(geojson)
library(cartography)
library(magick)


#download the dataset from the ECDC website to a local temporary file
GET("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", authenticate(":", ":", type="ntlm"), write_disk(tf <- tempfile(fileext = ".csv")))

#read the Dataset sheet into “R”. The dataset will be called "data".
data <- read.csv(tf, stringsAsFactors = F)

#WHO Africa countries
WHO_africa_countries <- read_csv("WHO_country_list.csv")

#filter data to only include Africa continent
africa_data <- data %>% 
  filter(countryterritoryCode %in% WHO_africa_countries$ISO3)

names(africa_data)[1] <- "dateRep"
#Convert date to actual date
africa_data$dateRep<- as.Date(africa_data$dateRep, "%d/%m/%Y")

#Calculate cumulative cases & deaths 
africa_data %<>% 
  group_by(countryterritoryCode) %>% 
  arrange(countryterritoryCode,dateRep) %>% 
  mutate(cumCases = cumsum(cases), cumDeaths=cumsum(deaths))

africa_data %<>% ungroup()

africa_data %<>% right_join(WHO_africa_countries, by=c("countryterritoryCode"="ISO3"))
africa_data %<>% mutate(popData2018=ifelse(countryterritoryCode=="ERI",6050000,popData2018))


africa_case_wide <- africa_data[,c(1,5,13)] %>% pivot_wider(names_from = Country, values_from = cases)
africa_deaths_wide <- africa_data[,c(1,6,13)] %>% pivot_wider(names_from = Country, values_from = deaths)

temp <- africa_data %>% select(-1) %>% replace(is.na(.), 0)
temp$dateRep <- africa_data$dateRep
africa_data <- temp

africa_data$dateRep[is.na(africa_data$dateRep)] <- as.Date("2020-01-01")

maxima <- africa_data %>% 
  group_by(Country) %>% 
  filter(cumCases==max(cumCases, na.rm = F),.preserve = T) %>% 
  top_n(1,dateRep)


#Maps for reports
library(geojsonio)
africa <- geojson_read("Africa1.geojson", what="sp")

#import WHO data and link to map-data
who_data <- read_csv("Africa_2020-04-02.csv")
africa@data %<>% left_join(who_data, by=c("ISO_A3"="countryterritoryCode"))

#Cases
breaks <- classIntervals(africa@data$total_cases, n = 8, style = "jenks", na.rm=T)$brks
breaks <- c(0,breaks)
breaks[2]<-1
palblue <- brewer.pal(9, name = "Blues")
palblue[1]<-"#FFFFFF"
png(filename = "Map_cum_cases.png", width=1920, height=1240, pointsize = 22)
choroLayer(spdf = africa, var = "total_cases", colNA = "grey", legend.nodata = "Non WHO-Africa country",
           breaks=breaks, col=palblue,legend.title.txt = "Cumulative Cases", legend.title.cex = 1,
           legend.values.cex = 1, legend.pos = c(-30,-40))
dev.off()

#Cases per pop
breaks <- classIntervals(africa@data$CaseperPop, n = 8, style = "jenks", na.rm=T)$brks
breaks <- c(0,breaks)
breaks[2]<-0.000001
palblue <- brewer.pal(9, name = "Blues")
palblue[1]<-"#FFFFFF"
png(filename = "Map_cases_10k_pop.png", width=1920, height=1240, pointsize = 22)
choroLayer(spdf = africa, var = "CaseperPop", colNA = "grey", legend.nodata = "Non WHO-Africa country",
           breaks=breaks, col=palblue,legend.title.txt = "Cum. cases per 10k pop.", legend.title.cex = 1, 
           legend.values.cex = 1, legend.values.rnd = 3, legend.pos = c(-30,-40))
dev.off()


#Deaths
breaks <- classIntervals(africa@data$total_deaths, n = 6, style = "jenks", na.rm=T)$brks
breaks[2]<-1
palred <- brewer.pal(7, name = "Reds")
palred[1]<-"#FFFFFF"
png(filename = "Map_cum_deaths.png", width=1920, height=1240, pointsize = 22)
choroLayer(spdf = africa, var = "total_deaths", colNA = "grey", legend.nodata = "Non WHO-Africa country",
           breaks=breaks, col=palred,legend.title.txt = "Cumulative Deaths", legend.title.cex = 1, 
           legend.values.cex = 1, legend.values.rnd = 3, legend.pos = c(-30,-40))
dev.off()

#Deaths per pop
breaks <- classIntervals(africa@data$DeathsperPop, n = 6, style = "jenks", na.rm=T)$brks
breaks <- c(0,breaks)
breaks[2]<-0.0000001
palred <- brewer.pal(7, name = "Reds")
palred[1]<-"#FFFFFF"
png(filename = "Map_deaths_10k_pop.png", width=1920, height=1240, pointsize = 22)
choroLayer(spdf = africa, var = "DeathsperPop", colNA = "grey", legend.nodata = "Non WHO-Africa country",
           breaks=breaks, col=palred,legend.title.txt = "Cum. deaths per 10k pop.", legend.title.cex = 1, 
           legend.values.cex = 1, legend.values.rnd = 3, legend.pos = c(-30,-40))
dev.off()


#Dt maps
#Cases
who_dt_data <- read_csv("Africa_Dts.csv")
africa@data %<>% left_join(who_dt_data, by=c("ISO_A3"="countryterritoryCode"))

breaks <- classIntervals(africa@data$Dt_cases, n = 9, style = "jenks", na.rm=T)$brks
breaks[2]<-0.00001
palgreen <- brewer.pal(9, name = "Greens")
palgreen <- rev(palgreen)
palgreen[1]<-"#FFFFFF"
png(filename = "Map_dt_cases.png", width=1920, height=1240, pointsize = 22)
choroLayer(spdf = africa, var = "Dt_cases", colNA = "grey", legend.nodata = "Non WHO-Africa country",
           breaks=breaks, col=palgreen,legend.title.txt = "Doubling time cases (days)", legend.title.cex = 1, 
           legend.values.cex = 1, legend.values.rnd = 3, legend.pos = c(-30,-40))
dev.off()

#Deaths
breaks <- classIntervals(africa@data$Dt_deaths, n = 6, style = "jenks", na.rm=T)$brks
breaks[2]<-0.00001
palgreen <- brewer.pal(7, name = "Greens")
palgreen <- rev(palgreen)
palgreen[1]<-"#FFFFFF"
png(filename = "Map_dt_deaths.png", width=1920, height=1240, pointsize = 22)
choroLayer(spdf = africa, var = "Dt_deaths", colNA = "grey", legend.nodata = "Non WHO-Africa country",
           breaks=breaks, col=palgreen,legend.title.txt = "Doubling time deaths (days)", legend.title.cex = 1, 
           legend.values.cex = 1, legend.values.rnd = 3, legend.pos = c(-30,-40))
dev.off()


#crop images and create 2x6 plot
#This assumes images are 1920x1240, will centre-crop to 1080x960 
#Read images
image1 <- image_read("Map_cum_Cases.png")
image2 <- image_read("Map_cases_10k_pop.png")
image3 <- image_read("Map_cum_deaths.png")
image4 <- image_read("Map_deaths_10k_pop.png")
image5 <- image_read("Map_dt_cases.png")
image6 <- image_read("Map_dt_deaths.png")

#Crop images
image1_crop <- image_crop(image1, "1080x960+420+140")
image2_crop <- image_crop(image2, "1080x960+420+140")
image3_crop <- image_crop(image3, "1080x960+420+140")
image4_crop <- image_crop(image4, "1080x960+420+140")
image5_crop <- image_crop(image5, "1080x960+420+140")
image6_crop <- image_crop(image6, "1080x960+420+140")

#save to 3x2 plot-
png(file = "Maps_WHO_Africa.png", width=1080*2, height=960*3, pointsize=22)
par(mai=rep(0,4)) # no margins
layout(matrix(1:6, ncol=2, byrow=TRUE))
plot(NA, xlim=0:1, ylim=0:1, bty="n", axes=0, xaxs = 'i', yaxs='i')
rasterImage(image1_crop, 0, 0, 1,1)
plot(NA, xlim=0:1, ylim=0:1, bty="n", axes=0, xaxs = 'i', yaxs='i')
rasterImage(image2_crop, 0, 0, 1,1)
plot(NA, xlim=0:1, ylim=0:1, bty="n", axes=0, xaxs = 'i', yaxs='i')
rasterImage(image3_crop, 0, 0, 1,1)
plot(NA, xlim=0:1, ylim=0:1, bty="n", axes=0, xaxs = 'i', yaxs='i')
rasterImage(image4_crop, 0, 0, 1,1)
plot(NA, xlim=0:1, ylim=0:1, bty="n", axes=0, xaxs = 'i', yaxs='i')
rasterImage(image5_crop, 0, 0, 1,1)
plot(NA, xlim=0:1, ylim=0:1, bty="n", axes=0, xaxs = 'i', yaxs='i')
rasterImage(image6_crop, 0, 0, 1,1)
dev.off()


#Map with countries in WHO-Africa for front page.
africa@data$WHOCountry <- ifelse(is.na(africa@data$Country),0,1)
typoLayer(spdf = africa, var = "WHOCountry", col = c("skyblue", "white"), legend.pos = "n", )
#legendTypo(title.txt = "", col = c("skyblue","white"),  categ = c("Country included", "Country not included"),  nodata = FALSE, pos = c(-20,-30))


#Dt calculations
Dt_Cases = africa_data %>% group_by(countriesAndTerritories) %>% arrange(countriesAndTerritories,dateRep) %>% mutate(Dt=7*log(2)/log(nth(cumCases,-1)/nth(cumCases,-8))) %>% summarise(Dt=max(Dt))
Dt_Deaths = africa_data %>% group_by(countriesAndTerritories) %>% arrange(countriesAndTerritories,dateRep) %>% mutate(Dt=7*log(2)/log(nth(cumDeaths,-1)/nth(cumDeaths,-8))) %>% summarise(Dt=max(Dt))

