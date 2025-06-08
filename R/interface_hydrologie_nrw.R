

get_rawdata_nrw <- function(hub, query, descr) {

  hub<-check_hub(hub)
  query<-check_query_lanuv(query)
  descr<-check_descr_lanuv(descr)

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

list_hydrodata_files_nrw <- function(page_url) {
    html <- httr::GET(page_url)
    content <- httr::content(html, as = "text", encoding = "UTF-8")
    # Debug: Zeige einen Ausschnitt des HTMLs
    # cat(substr(content, 1, 2000))
    # Robustere Regex für href mit .zip (doppelte oder einfache Anführungszeichen)
    zip_names <- regmatches(content, gregexpr('"name":"([^"]+\\.zip)"', content))[[1]]
    zip_names <- gsub('.*"name":"([^"]+\\.zip)".*', '\\1', zip_names)
    zip_names <- unique(zip_names)
    # Absolute URLs bauen
    return(zip_names)
  }

descr_names <- function(page_url, zip_names_filtered) {
  # Funktion, um die Dateinamen in den ZIPs zu extrahieren
  file_names <- list()
  for (zip_url in zip_names_filtered) {
    tmp_zip <- tempfile(fileext = ".zip")
    curl::curl_download(paste0(page_url, zip_url), tmp_zip)
    files_in_zip <- unzip(tmp_zip, list = TRUE)$Name
    file_names[[zip_url]] <- files_in_zip
    unlink(tmp_zip)
  }
  return(file_names)
}

download_hydrodata_nrw<-function(hub, query, descr) {
  hub <- check_hub(hub)
  query <- check_query_lanuv(query)
  #descr <- check_descr_lanuv(descr)

  loc <- paste(hub, query, sep = "")
  placeholder <- tempfile()
  explace <- tempfile()
  curl_download(loc, destfile = placeholder)
  con1 <- unzip(placeholder, files=descr, exdir=explace)
  data <- vroom::vroom(con1, delim=";", col_names=TRUE, show_col_types = F)

  unlink(placeholder)
  unlink(explace)

  return(data)
}

filter_zip_by_daterange <- function(zip_names, date_range) {
  jahr_von <- as.numeric(format(date_range[1], "%Y"))
  jahr_bis <- as.numeric(format(date_range[2], "%Y"))
  zip_names[sapply(zip_names, function(z) {
    bereich <- regmatches(z, regexpr("\\d{4}-\\d{4}", z))
    if (length(bereich) == 1) {
      jahr_start <- as.numeric(sub("-.*", "", bereich))
      jahr_ende <- as.numeric(sub(".*-", "", bereich))
      return(jahr_start <= jahr_bis && jahr_ende >= jahr_von)
    }
    FALSE
  })]
}

download_and_bind_timeseries <- function(page_url, zip_names, ezg, date_range) {
  # Filtere nach Einzugsgebiet und Datumsbereich
  zip_names_ezg <- zip_names[grepl(ezg, zip_names)]
  zip_names_filtered <- filter_zip_by_daterange(zip_names_ezg, date_range)
  descr_names_list <- descr_names(page_url, zip_names_filtered)
  
  all_data <- list()
  for (i in seq_along(zip_names_filtered)) {
    zip_url <- zip_names_filtered[i]
    files_in_zip <- descr_names_list[[zip_url]]
    # Hier ggf. weitere Filterung der Dateien im ZIP, z.B. nach Stationsname
    # Beispiel: alle Dateien laden
    for (file in files_in_zip) {
      tmp_zip <- tempfile(fileext = ".zip")
      curl::curl_download(paste0(page_url, zip_url), tmp_zip)
      data <- vroom::vroom(
        unz(tmp_zip, file),
        delim = ";",
        show_col_types = FALSE,
        locale = vroom::locale(decimal_mark = ",")
      )
      unlink(tmp_zip)
      all_data[[paste(zip_url, file, sep = "_")]] <- data
    }
  }
  # Alle Zeitreihen zusammenfügen
  dplyr::bind_rows(all_data)
}

