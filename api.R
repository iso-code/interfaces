library(plumber)
library(devtools)
library(interfaces)

#* @apiTitle NRW Hydro Data API

#* Get metadata
#* @param hub
#* @param query
#* @param descr
#* @get /metadata_raw_nrw
function(hub = "raw_nrw", query = "pegel", descr = "pegel_stationen.txt") {
  metadata <- get_rawdata_nrw(hub, query, descr)
  return(metadata)
}

#* Get raw NRW data
#* @param hub
#* @param query
#* @param descr
#* @get /rawdata
function(hub = "raw_nrw", query = "pegel", descr = "pegel_stand") {
  data <- get_rawdata_nrw(hub, query, descr)
  return(data)
}


#* Get filtered station data
#* @param st_name
#* @param st_id
#* @param startyear
#* @param endyear
#* @get /stationdata
function(st_name = "Ahmsen", startyear = 2000, endyear = 2022) {
  page_url = check_hub("verified_level_nrw")
  if(file.exists("page_tree_W.rds")){ 
    page_tree<-readRDS("page_tree_W.rds")
    } else {page_tree <- get_pagetree(page_url)}
  
  file_index <- find_station_files_in_metadata(
    page_tree,
    st_id=st_id,
    st_name = st_name,
    startyear = as.numeric(startyear),
    endyear = as.numeric(endyear)
  )
  dataset <- load_filtered_station_data(page_url, file_index, as.numeric(startyear), as.numeric(endyear))
  return(dataset)
}
