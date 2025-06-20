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


##############################
#Example für OpenData hydro
page_url = "https://www.opengeodata.nrw.de/produkte/umwelt_klima/wasser/oberflaechengewaesser/hydro/q/"
zip_names <- list_hydrodata_files_nrw(page_url)

#nur zum Update des Datensatzes notwendig, Änderungen selten (jährlich)
#meta_data<-create_metadata_nrw(page_url,zip_names)
#saveRDS(meta_data,"meta_data.rds")
meta_data<-readRDS("meta_data.rds")

# Beispiel-Datumsbereich (ersetze durch input$plot_range in Shiny)
date_range <- as.Date(c("1964-11-01", "2022-11-01"))
ezg <- "Weserzufluesse"

file_index<-find_station_files_in_metadata(meta_data, station_id = NULL, station_name = c("Ahmsen","Welz"), date_range[1], date_range[2])
startjahr<-date_range[1]
endjahr<- date_range[2]
dataset<-load_filtered_station_data(page_url, file_index, 2000, 2022)

###################################

# shape oder csv der Metadaten des Datensatzes
page_url = "https://www.opengeodata.nrw.de/produkte/umwelt_klima/wasser/oberflaechengewaesser/hydro/"
zip_names <- list_hydrodata_files_nrw(page_url)

data<-download_hydrodata_nrw(
  hub = page_url,
  query = zip_names[1]
)


