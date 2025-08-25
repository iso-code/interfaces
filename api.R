if(.Platform$OS.type=="unix") .libPaths("/home/lanuv.nrw.de/gaj/custom-r-libs")
library(plumber)
library(devtools)
install_github("iso-code/interfaces")
library(Interfaces)

#* @apiTitle NRW Hydro Data API

#* Get raw NRW data
#* @param hub
#* @param query
#* @param descr
#* @get /rawdata
function(hub = "raw_nrw", query = "pegel", descr = "pegel_stand") {
  data <- get_rawdata_nrw(hub, query, descr)
  return(data)
}

#* Get metadata
#* @param hub
#* @param query
#* @param descr
#* @get /metadata
function(hub = "raw_nrw", query = "pegel", descr = "pegel_stationen.txt") {
  metadata <- get_rawdata_nrw(hub, query, descr)
  return(metadata)
}

#* Get filtered station data
#* @param station_name
#* @param startyear
#* @param endyear
#* @get /stationdata
function(station_name = "Ahmsen", startyear = 2000, endyear = 2022) {
  page_url <- "https://www.opengeodata.nrw.de/produkte/umwelt_klima/wasser/oberflaechengewaesser/hydro/w/"
  if(file.exists("page_tree_W.rds")){ 
    page_tree<-readRDS("page_tree_W.rds")
    } else {page_tree <- get_pagetree(page_url)}
  file_index <- find_station_files_in_metadata(
    page_tree,
    station_id = NULL,
    station_name = station_name,
    startyear = as.numeric(startyear),
    endyear = as.numeric(endyear)
  )
  dataset <- load_filtered_station_data(page_url, file_index, as.numeric(startyear), as.numeric(endyear))
  return(dataset)
}