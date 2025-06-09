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

descr_names <- function(page_url, zip_names) {
  # Funktion, um die Dateinamen in den ZIPs zu extrahieren
  file_names <- list()
  for (zip_url in zip_names) {
    # Debug: Zeige den aktuellen ZIP-URL
    tmp_zip <- tempfile(fileext = ".zip")
    curl::curl_download(paste0(page_url, zip_url), tmp_zip)
    files_in_zip <- unzip(tmp_zip, list = TRUE)$Name
    file_names[[zip_url]] <- files_in_zip
    unlink(tmp_zip)
  }
  return(file_names)
}

download_hydrodata_nrw <- function(hub, query, descr = NULL) {
  placeholder <- tempfile(fileext = ".zip")
  explace <- tempfile()
  curl::curl_download(paste0(hub,query), destfile = placeholder)
  # Entpacken: alle Dateien oder nur bestimmte
  if (is.null(descr)) {
    files <- unzip(placeholder, exdir = explace)
  } else {
    files <- unzip(placeholder, files = descr, exdir = explace)
  }
  # Prüfe, ob ein Shapefile enthalten ist
  shp_file <- files[grepl("\\.shp$", files, ignore.case = TRUE)]
  if (length(shp_file) > 0) {
    # Lade das Shapefile mit sf
    if (!requireNamespace("sf", quietly = TRUE)) install.packages("sf")
    library(sf)
    data <- sf::st_read(shp_file[1], quiet = TRUE)
  } else {
    # Lade als Tabelle (CSV)
    csv_file <- files[grepl("\\.csv$", files, ignore.case = TRUE)]
    if (length(csv_file) > 0) {
      data <- vroom::vroom(csv_file[1], delim = ";", col_names = TRUE, show_col_types = FALSE)
    } else {
      data <- NULL
    }
  }
  unlink(placeholder)
  unlink(explace, recursive = TRUE)
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

