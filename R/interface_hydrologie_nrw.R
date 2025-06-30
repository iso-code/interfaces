#' Lade Rohdaten aus NRW ZIP-Datei herunter und lese sie ein
#'
#' Diese Funktion lädt eine ZIP-Datei von `hub + query` herunter,
#' entpackt eine Datei namens `descr` und liest diese CSV-Datei ein.
#'
#' @param hub Character. Basis-URL oder Pfad zum Daten-Hub.
#' @param query Character. Pfad oder Query-String, der an `hub` angehängt wird.
#' @param descr Character. Dateiname innerhalb der ZIP-Datei, der entpackt und eingelesen wird.
#'
#' @return Ein `data.frame` oder `tibble` mit den geladenen Daten.
#' @importFrom curl curl_download
#' @importFrom vroom vroom
#' @export
get_rawdata_nrw <- function(hub, query, descr) {
  hub <- check_hub(hub)
  query <- check_query_lanuv(query)
  descr <- check_descr_lanuv(descr)

  loc <- paste(hub, query, sep = "")
  placeholder <- tempfile()
  explace <- tempfile()
  curl_download(loc, destfile = placeholder)
  con1 <- unzip(placeholder, files=descr, exdir=explace)
  data <- vroom::vroom(con1, delim=";", col_names=TRUE, show_col_types=FALSE)

  unlink(placeholder)
  unlink(explace)

  return(data)
}

#' Extrahiere Dateinamen aus ZIP-Archiven auf einer Webseite
#'
#' Lädt mehrere ZIP-Dateien von der Webseite und extrahiert die darin enthaltenen Dateinamen.
#'
#' @param page_url Character. Basis-URL der Webseite, auf der die ZIP-Dateien liegen.
#' @param zip_names Character vector. Namen der ZIP-Dateien.
#'
#' @return Liste mit ZIP-Dateinamen als Keys und Vektor der enthaltenen Dateien als Werte.
#' @importFrom curl curl_download
#' @export
descr_names <- function(page_url, zip_names) {
  file_names <- list()
  for (zip_url in zip_names) {
    print(zip_url)
    tmp_zip <- tempfile(fileext = ".zip")
    curl::curl_download(paste0(page_url, zip_url), tmp_zip)
    files_in_zip <- unzip(tmp_zip, list = TRUE)$Name
    file_names[[zip_url]] <- files_in_zip
    unlink(tmp_zip)
  }
  return(file_names)
}

#' Lade Hydrodaten aus ZIP-Archiv herunter und lese Shapefile oder CSV ein
#'
#' Lädt eine ZIP-Datei von `hub + query` herunter, entpackt entweder alle Dateien oder nur
#' die angegebenen `descr`, und liest ein Shapefile oder CSV-Datei ein.
#'
#' @param hub Character. Basis-URL oder Pfad.
#' @param query Character. Dateipfad oder Query-String.
#' @param descr Character or NULL. Optional: Nur diese Dateien entpacken.
#'
#' @return Ein sf-Objekt (bei Shapefile) oder data.frame (bei CSV) oder NULL, wenn keine Datei gefunden wurde.
#' @importFrom curl curl_download
#' @importFrom vroom vroom
#' @import sf
#' @export
download_hydrodata_nrw <- function(hub, query, descr = NULL) {
  placeholder <- tempfile(fileext = ".zip")
  explace <- tempfile()
  curl::curl_download(paste0(hub, query), destfile = placeholder)
  if (is.null(descr)) {
    files <- unzip(placeholder, exdir = explace)
  } else {
    files <- unzip(placeholder, files = descr, exdir = explace)
  }
  shp_file <- files[grepl("\\.shp$", files, ignore.case = TRUE)]
  if (length(shp_file) > 0) {
    if (!requireNamespace("sf", quietly = TRUE)) install.packages("sf")
    library(sf)
    data <- sf::st_read(shp_file[1], quiet = TRUE)
  } else {
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

#' Filtere ZIP-Dateinamen nach Jahresbereich
#'
#' Filtert eine Liste von ZIP-Dateinamen, die einen Jahresbereich (z.B. "1980-1989") enthalten,
#' und gibt nur jene zurück, die mit dem angegebenen Datumsbereich überlappen.
#'
#' @param zip_names Character vector. ZIP-Dateinamen mit Jahresbereich im Namen.
#' @param date_range Date vector der Länge 2 (Start- und Enddatum).
#'
#' @return Gefilterter Character vector der ZIP-Dateinamen.
#' @export
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

#' Liste ZIP-Dateinamen von einer Seite aus
#'
#' Lädt die HTML-Seite und extrahiert alle ZIP-Dateinamen aus JSON-ähnlichen Strukturen.
#'
#' @param page_url Character. URL der Webseite.
#'
#' @return Character vector mit ZIP-Dateinamen.
#' @importFrom httr GET content
#' @export
list_page_zips <- function(page_url) {
  html <- httr::GET(page_url)
  content <- httr::content(html, as = "text", encoding = "UTF-8")
  zip_names <- regmatches(content, gregexpr('"name":"([^"]+\\.zip)"', content))[[1]]
  zip_names <- gsub('.*"name":"([^"]+\\.zip)".*', '\\1', zip_names)
  zip_names <- unique(zip_names)
  return(zip_names)
}

#' Erstelle Metadaten-Tabelle mit Informationen zu ZIP-Dateien und deren Inhalten
#'
#' Liest alle ZIP-Dateinamen einer Seite aus und extrahiert Informationen aus den Dateinamen der ZIP-Inhalte.
#'
#' @param page_url Character. URL der Seite mit ZIP-Dateien.
#'
#' @return data.frame mit Metadaten zu ZIP und enthaltenen Dateien.
#' @export
get_pagetree <- function(page_url) {
  zip_names <- list_page_zips(page_url)
  descrs <- descr_names(page_url, zip_names)
  meta <- data.frame()
  for (zip in names(descrs)) {
    files <- descrs[[zip]]
    if (length(files) == 0) next
    info <- do.call(rbind, lapply(files, function(f) {
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
  return(meta)
}

#' Finde Stationsdateien in Metadaten nach ID oder Namen und Jahrbereich
#'
#' Filtert Metadaten nach Station-ID oder -Name und einem Zeitraum (Start- und Endjahr).
#'
#' @param metadata data.frame. Metadaten mit Spalten 'station_id', 'station_name', 'jahr_start', 'jahr_ende', 'zip', 'file'.
#' @param station_id Character or NULL. Stations-ID zum Filtern.
#' @param station_name Character or NULL. Stationsname zum Filtern.
#' @param startjahr Numeric. Startjahr.
#' @param endjahr Numeric. Endjahr.
#'
#' @return data.frame mit Spalten 'zip' und 'file' der passenden Dateien.
#' @export
find_station_files_in_metadata <- function(metadata, station_id = NULL, station_name = NULL, startjahr, endjahr) {
  if (!is.null(station_id)) {
    sel <- metadata$station_id == station_id
  } else if (!is.null(station_name)) {
    sel <- metadata$station_name == station_name
  } else {
    stop("Bitte entweder station_id oder station_name angeben.")
  }
  sel <- sel & metadata$jahr_start <= endjahr & metadata$jahr_ende >= startjahr
  result <- metadata[sel, c("zip", "file")]
  rownames(result) <- NULL
  return(result)
}

#' Lade gefilterte Stationsdaten aus ZIP-Archiven und verbinde sie
#'
#' Lädt alle relevanten Dateien aus ZIP-Archiven für den angegebenen Jahrbereich,
#' liest die CSV-Daten ein und verbindet sie zu einem Datenrahmen.
#'
#' @param page_url Character. Basis-URL der Seite mit ZIP-Dateien.
#' @param file_index data.frame mit Spalten 'zip' und 'file'.
#' @param startjahr Numeric. Startjahr für Filterung.
#' @param endjahr Numeric. Endjahr für Filterung.
#'
#' @return data.frame mit zusammengefügten Daten oder NULL, wenn keine Dateien gefunden.
#' @importFrom dplyr filter bind_rows
#' @importFrom vroom vroom locale
#' @importFrom stats na_if
#' @export
load_filtered_station_data <- function(page_url, file_index, startjahr, endjahr) {
  file_index_filtered <- file_index %>%
    dplyr::filter(sapply(file, function(f) {
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
      value_cols <- grep("^value", names(df), value = TRUE)
      if (length(value_cols) > 0) {
        for (colname in value_cols) {
          if(length(grep("LUECKE", df[[colname]])) != 0) {
            df[[colname]] <- stats::na_if(df[[colname]], "LUECKE")
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
