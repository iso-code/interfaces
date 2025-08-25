if (!require(pacman)) install.packages('pacman')
library(pacman)
pacman::p_load(curl, tidyverse, tidyjson, jsonlite, devtools, vroom, install=TRUE)
#####################
install_github("iso-code/interfaces")
library(interfaces)

#read datafile from hub
hub<-"raw_nrw"
query <- "pegel"
descr <- "pegel_stand"
#
data<-get_rawdata_nrw(hub,query,descr)

#read metadata from hub
descr<-"pegel_stationen"
metadata<-get_rawdata_nrw(hub,query,"pegel_stationen.txt")

data$time<-ymd_hms(data$time)
metadata$station_no[1]
test<-data %>% filter(station_no %in% metadata$station_no[1])


##############################
#Example für OpenData hydro
page_url = check_hub("verified_level_nrw")

#nur zum Update des Datensatzes notwendig, Änderungen selten (vierteljährlich)
#page_tree<-get_pagetree(page_url)
#saveRDS(page_tree,"page_tree_W.rds")
page_tree<-readRDS("page_tree_W.rds")

# Beispiel-Datumsbereich (ersetze durch input$plot_range in Shiny)
date_range <- as.Date(c("1964-11-01", "2022-11-01"))
#ezg <- "Weserzufluesse"
#get_nrw_verified_levels()
stations<-c("Ahmsen","Welz")

file_index<-find_station_files_in_metadata(page_tree, station_name = stations, startyear=2000, endyear=2022)
dataset<-load_filtered_station_data(page_url, file_index, 2000, 2022)

###################################

page_url = "https://www.opengeodata.nrw.de/produkte/umwelt_klima/wasser/oberflaechengewaesser/hydro/"
zip_names <- get_pagetree(page_url)

meta_data<-download_hydrodata_nrw(
  hub = page_url,
  query = zip_names$zip[1]
)


