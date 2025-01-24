if (!require(pacman)) install.packages('pacman')
library(pacman)
pacman::p_load(curl, tidyverse, tidyjson, jsonlite, devtools, install=TRUE)
install_github("iso-code/interfaces")
library(Interfaces)

#read datafile from hub
hub<-"rawlanuv"
query <- "pegel"
descr <- "pegel_stand"
data<-get_rawdata_nrw(hub,query,descr)

#read metadata from hub
descr<-"pegel_stationen"
metadata<-get_rawdata_nrw(hub,query,"pegel_stationen.txt")

data$time<-ymd_hms(data$time)
metadata$station_no[1]
test<-data %>% filter(station_no %in% metadata$station_no[1])
