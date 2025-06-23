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

descr_names <- function(page_url, zip_names) {
  # Funktion, um die Dateinamen in den ZIPs zu extrahieren
  file_names <- list()
  for (zip_url in zip_names) {
    # Debug: Zeige den aktuellen ZIP-URL
    print(zip_url)
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

get_pagetree <- function(page_url) {
  zip_names <-  list_page_zips(page_url) 
  descrs    <-  descr_names(page_url, zip_names)
  meta <- data.frame()
  for (zip in names(descrs)) {
    files <- descrs[[zip]]
    if (length(files) == 0) next
    # Extrahiere Infos aus Dateinamen
    info <- do.call(rbind, lapply(files, function(f) {
      # Beispielname: 2718193000100_Ahrhuette-Neuhof_1980-1989_Abfluss_m3s.csv
      parts <- unlist(strsplit(f, "_"))
      jahr <- regmatches(f, regexpr("\\d{4}-\\d{4}", f))
      jahr_start <- as.numeric(sub("-.*", "", jahr))
      jahr_ende <- as.numeric(sub(".*-", "", jahr))
      data.frame(
        zip = zip,
        file = f,
        station_id = parts[1],
        station_name = parts[2],
        jahr_start = jahr_start,
        jahr_ende = jahr_ende,
        typ = ifelse(length(parts) > 4, parts[4], NA),
        einheit = ifelse(length(parts) > 5, gsub("\\.csv$", "", parts[5]), NA),
        stringsAsFactors = FALSE
      )
    }))
    meta <- rbind(meta, info)
  }
  meta
}

find_station_files_in_metadata <- function(metadata, station_id = NULL, station_name = NULL, startjahr, endjahr) {
  # Filter nach Stationsnummer oder -name
  if (!is.null(station_id)) {
    sel <- metadata$station_id == station_id
  } else if (!is.null(station_name)) {
    sel <- metadata$station_name == station_name
  } else {
    stop("Bitte entweder station_id oder station_name angeben.")
  }
  # Filter nach Jahrbereich (Überlappung)
  sel <- sel & metadata$jahr_start <= endjahr & metadata$jahr_ende >= startjahr
  # Ergebnis als Dataframe mit zip und file
  result <- metadata[sel, c("zip", "file")]
  rownames(result) <- NULL
  result
}



load_filtered_station_data <- function(page_url, file_index, startjahr, endjahr) {
  
  # Filter ZIP-Dateien und Dateien nach Jahr
  file_index_filtered <- file_index %>%
    filter(sapply(file, function(f) {
      year_range <- regmatches(f, regexpr("\\d{4}-\\d{4}", f))
      if (length(year_range) == 1) {
        jahr_start <- as.numeric(sub("-.*", "", year_range))
        jahr_ende <- as.numeric(sub(".*-", "", year_range))
        jahr_start <= endjahr && jahr_ende >= startjahr
      } else {
        FALSE
      }
    }))
  
  if (nrow(file_index_filtered) == 0) {
    warning("Keine Dateien im angegebenen Zeitraum gefunden.")
    return(NULL)
  }
  
  all_data <- list()
  
  # Gruppiere nach ZIP-Datei, lade und verarbeite alle relevanten Dateien
  zip_groups <- split(file_index_filtered$file, file_index_filtered$zip)
  
  for (zip_name in names(zip_groups)) {
    files_to_load <- zip_groups[[zip_name]]
    zip_url <- paste0(page_url, zip_name)
    
    tmp_zip <- tempfile(fileext = ".zip")
    curl::curl_download(zip_url, tmp_zip)
    
    for (file in files_to_load) {
      df <- vroom::vroom(
        unz(tmp_zip, file),
        delim = ";",
        show_col_types = FALSE,
        locale = vroom::locale(decimal_mark = ",")
      )
      
      # Dynamisch die "value"-Spalte suchen und umwandeln, falls vorhanden
      value_cols <- grep("^value", names(df), value = TRUE)
      if (length(value_cols) > 0) {
        for (colname in value_cols) {
          if(length(grep("LUECKE",df[[colname]]))!=0)
          {
             df[[colname]] <- na_if(df[[colname]], "LUECKE")
             df[[colname]] <- as.numeric(df[[colname]])
          }
        }
      }
      
      all_data[[paste(zip_name, file, sep = "_")]] <- df
    }
    
    unlink(tmp_zip)
  }
  
  if (length(all_data) == 0) return(NULL)
  
  dplyr::bind_rows(all_data)
}
