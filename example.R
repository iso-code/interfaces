

hub <- "https://hydrologie.nrw.de/lanuv/data/downloads/"

#read datafile from hub
query <- "messwerte.zip"
data<-get_rawdata_nrw(hub,query)

#read metadata from hub
query <- "pegeldaten.zip"
metadata<-get_rawdata_nrw(hub,query,"pegel_stationen.txt")

data$time<-ymd_hms(data$time)
metadata$station_no[1]
test<-data %>% filter(station_no %in% metadata$station_no[1])
