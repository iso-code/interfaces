if (!require(pacman)) install.packages('pacman')
library(pacman)
pacman::p_load(curl, tidyverse, tidyjson, jsonlite,install=TRUE)

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
      "\nSee xxxx for more information.")
  }
}

get_rawdata_nrw <- function(hub, query, descr) {

  check_hub(hub)

  if (missing(descr)) {
    descr <- "messwerte.txt"
  } else {
    if (!inherits(descr, "character")) {
      stop(
        "User supplied unvalid decription, must be filename within zip file"
      )
    }
  }

  loc <- paste(hub, query, sep = "")
  placeholder <- tempfile()
  curl_download(loc, destfile = placeholder)
  con1 <- unzip(placeholder, descr)
  data<-vroom::vroom(con1,delim=";", col_names=TRUE,show_col_types = F)
  unlink(placeholder)
  return(data)

}




