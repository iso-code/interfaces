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
# Install and load the required packages
page_url = "https://www.opengeodata.nrw.de/produkte/umwelt_klima/wasser/oberflaechengewaesser/hydro/q/"

zip_names <- list_hydrodata_files_nrw(page_url)
# Beispiel-Datumsbereich (ersetze durch input$plot_range in Shiny)
date_range <- as.Date(c("2020-01-01", "2022-12-31"))
ezg <- "Weserzufluesse"
# Funktion zum Filtern der ZIP-Namen nach Datumsbereich
all_data <-download_and_bind_timeseries(page_url, zip_names, ezg, date_range)

#zip_names_filtered <- zip_names[grepl(datumsbereich, zip_names)]
#zip_names_filtered <- zip_names_filtered[grepl(ezg, zip_names_filtered)]

# shape des Datensatzes
page_url = "https://www.opengeodata.nrw.de/produkte/umwelt_klima/wasser/oberflaechengewaesser/hydro/"
zip_names <- list_hydrodata_files_nrw(page_url)

data<-download_hydrodata_nrw(
  hub = page_url,
  query = zip_names[1]
)


page_url = "https://www.opengeodata.nrw.de/produkte/umwelt_klima/wasser/oberflaechengewaesser/hydro/q/"
dd<-find_and_bind_station_timeseries(page_url, zip_names, station_id = NULL,"Ahmsen", "1998", "2005")


descr_names_list <- descr_names(page_url, zip_names_filtered)

# Funktion zum Erstellen der Metadaten
metadata <- create_metadata_nrw(page_url, zip_names)
saveRDS(metadata, file = "metadata_nrw.rds")

find_station_files_in_metadata(metadata, station_id = "Ahmsen", startjahr = 2000, endjahr = 2005)
