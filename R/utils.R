#' Check and set the data hub
#'
#' This function checks whether the input argument `hub` is one of the
#' predefined default hubs or a user-defined URL. If valid, the corresponding
#' API URL is returned.
#'
#' @param hub Character. Either a keyword for the predefined hubs
#' (`"kisters"`, `"raw_nrw"`, `"verified_level_nrw"`,
#' `"verified_runoff_nrw"`) or a custom URL.
#'
#' @return Character string containing the API URL.
#' @examples
#' check_hub("kisters")
#' check_hub("https://example.com/api/")
check_hub <- function(hub) {
  # Identify default hubs
  default_hubs <- list(
    "kisters" = "http://kiwis.kisters.de/KiWIS/KiWIS?",
    "raw_nrw" = "https://www.hochwasserportal.nrw/data/downloads/",
    "verified_level_nrw" ="https://www.opengeodata.nrw.de/produkte/umwelt_klima/wasser/oberflaechengewaesser/hydro/w/",
    "verified_runoff_nrw" = "https://www.opengeodata.nrw.de/produkte/umwelt_klima/wasser/oberflaechengewaesser/hydro/q/"
  )

  # Hub selection
  if (!is.character(hub) | nchar(hub) == 0) {
    stop(
      "`hub` argument must be a character- either a URL or one of the following defaults: ",
      paste(c("", names(default_hubs)), collapse = "\n"),
      "\nSee https://github.com/iso-code/interfaces for more information.")
  }

  if (!hub %in% names(default_hubs)) {
    # Non-default URL
    api_url <- hub
  } else {
    api_url <- default_hubs[[which(names(default_hubs) == hub)]]
  }

  return(api_url)
}

#' Check and set the default query for LANUV data
#'
#' This function checks whether the input argument `query` is one of the
#' predefined default queries or a user-defined file. If valid, the
#' corresponding file name is returned.
#'
#' @param query Character. Either `"pegel"`, `"hydromet"`, or a custom
#' file name.
#'
#' @return Character string with the query file name.
#' @examples
#' check_query_lanuv("pegel")
#' check_query_lanuv("custom_file.zip")
check_query_lanuv <- function(query) {
  default_query <- list(
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
  } else {
    query <- default_query[[which(names(default_query) == query)]]
  }

  return(query)
}

#' Check and set the default description file for LANUV data
#'
#' This function checks whether the input argument `descr` is one of the
#' predefined default descriptions or a user-defined file. If valid, the
#' corresponding file name is returned.
#'
#' @param descr Character. For example, `"pegel_stand"`, `"pegel_stationen"`,
#' `"hydrmet_N"`, `"hydrmet_tag"`, or a custom file name.
#'
#' @return Character string with the description file name.
#' @examples
#' check_descr_lanuv("pegel_stand")
#' check_descr_lanuv("my_file.txt")
check_descr_lanuv <- function(descr) {
  default_descr <- list(
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
  } else {
    descr <- default_descr[[which(names(default_descr) == descr)]]
  }

  return(descr)
}

#' List all ZIP files from a webpage
#'
#' This function fetches an HTML page and extracts the names of all linked
#' ZIP files. The output is a vector of file names.
#'
#' @param page_url Character. URL of the webpage where ZIP files are listed.
#'
#' @return Character vector of ZIP file names.
#' @examples
#' \dontrun{
#' list_page_zips("https://www.hochwasserportal.nrw/data/downloads/")
#' }
list_page_zips <- function(page_url) {
  html <- httr::GET(page_url)
  content <- httr::content(html, as = "text", encoding = "UTF-8")
  # Regex for .zip names
#  zip_names <- regmatches(content, gregexpr('"name":"([^"]+\\.zip)"', content))[[1]]
  zip_names <- regmatches(content, gregexpr('name="([^"]+\\.zip)"', content))[[1]]
  #zip_names <- gsub('.*"name":"([^"]+\\.zip)".*', '\\1', zip_names)
  zip_names <- gsub('.*name="([^"]+\\.zip)".*', '\\1', zip_names)
  zip_names <- unique(zip_names)
  return(zip_names)
}
