check_hub <- function(hub) {
  # Identify default hubs
  default_hubs <- list(
    "rawlanuv" = "https://hydrologie.nrw.de/lanuv/data/downloads/"
  )

  # Hub selection
  if (!is.character(hub) | nchar(hub) == 0) {
    stop(
      "`hub` argument must be a character- either a URL or one of the following defaults: ",
      paste(c("", names(default_hubs)), collapse = "\n"),
      "\nSee https://github.com/iso-code/interfaces for more information.")
  }

  if (!hub %in% names(default_hubs)) {
    # Non-default KiWIS URL
    api_url <- hub
  }else{
    api_url <- default_hubs[[which(names(default_hubs) == hub)]]
  }

  return(api_url)
}

check_query_lanuv <- function(query)
{
  default_query <-list(
    "pegel" = "pegeldaten.zip",
    "hydromet" = "niederschlagsdaten.zip"
  )

  # query selection
  if (!is.character(query) | nchar(query) == 0) {
    stop(
      "`hub` argument must be a character one of the following defaults: ",
      paste(c("", names(default_query)), collapse = "\n"),
      "\nSee https://github.com/iso-code/interfaces for more information.")
  }

  if (!query %in% names(default_query)) {
    # Non-default file
    query <- query
  }else{
    query <- default_query[[which(names(default_query) == query)]]
  }

  return(query)

}

check_descr_lanuv <- function(descr){

  default_descr <-list(
    "pegel_stand" = "pegel_messwerte.txt",
    "pegel_stationen" = "pegel_stationen.txt",
    "pegel_max" = "pegel_tagesmaxima.txt",
    "pegel_min" = "pegel_tagesmittelwerte.txt",
    "hydrmet_N" = "nieder_messwerte.txt",
    "hydrmet_stationen" = "nieder_stationen.txt",
    "hydrmet_tag" = "nieder_tageswerte.txt"
  )

  # query selection
  if (!is.character(descr) | nchar(descr) == 0) {
    stop(
      "`descr` argument must be a character one of the following defaults: ",
      paste(c("", names(default_descr)), collapse = "\n"),
      "\nSee https://github.com/iso-code/interfaces for more information.")
  }

  if (!descr %in% names(default_descr)) {
    # Non-default file
    descr <- descr
  }else{
    descr <- default_descr[[which(names(default_descr) == descr)]]
  }

  return(descr)
}


