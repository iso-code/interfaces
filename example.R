if (!require(pacman)) install.packages('pacman')
library(pacman)
pacman::p_load(curl, tidyverse, tidyjson, jsonlite,install=TRUE)
library(Interfaces)

#read datafile from hub
hub<-"rawlanuv"
query <- "messwerte.zip"
data<-get_rawdata_nrw(hub,query)

#read metadata from hub
query <- "pegeldaten.zip"
metadata<-get_rawdata_nrw(hub,query,"pegel_stationen.txt")

data$time<-ymd_hms(data$time)
metadata$station_no[1]
test<-data %>% filter(station_no %in% metadata$station_no[1])
