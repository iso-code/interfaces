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

  if (!hub %in% names(default_hubs)) {
    # Non-default KiWIS URL
    api_url <- hub
  }else{
    api_url <- default_hubs[[which(names(default_hubs) == hub)]]
  }


  return(api_url)
}

get_rawdata_nrw <- function(hub, query, descr) {

  hub<-check_hub(hub)

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
  explace <- tempfile()
  curl_download(loc, destfile = placeholder)
  con1 <- unzip(placeholder, files=descr, exdir=explace)
  data<-vroom::vroom(con1,delim=";", col_names=TRUE,show_col_types = F)

  unlink(placeholder)
  unlink(explace)

  return(data)

}




