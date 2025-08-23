#' Download and read raw NRW data from ZIP file
#'
#' This function downloads a ZIP file from `hub + query`,
#' extracts a file named `descr`, and reads this CSV file.
#'
#' @param hub Character. Base URL or path to the data hub.
#' @param query Character. Path or query string appended to `hub`.
#' @param descr Character. Filename inside the ZIP file to extract and read.
#'
#' @return A `data.frame` or `tibble` with the loaded data.
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

#' Extract file names from ZIP archives on a web page
#'
#' Downloads multiple ZIP files from the web page and extracts the contained file names.
#'
#' @param page_url Character. Base URL of the web page containing the ZIP files.
#' @param zip_names Character vector. Names of the ZIP files.
#'
#' @return List with ZIP file names as keys and vectors of contained files as values.
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

#' Download hydro data from ZIP archive and read shapefile or CSV
#'
#' Downloads a ZIP file from `hub + query`, extracts either all files or only
#' the specified `descr`, and reads a shapefile or CSV file.
#'
#' @param hub Character. Base URL or path.
#' @param query Character. File path or query string.
#' @param descr Character or NULL. Optional: Only extract these files.
#'
#' @return An sf object (for shapefile), data.frame (for CSV), or NULL if no file found.
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

#' Filter ZIP file names by year range
#'
#' Filters a list of ZIP file names containing a year range (e.g., "1980-1989"),
#' and returns only those overlapping with the specified date range.
#'
#' @param zip_names Character vector. ZIP file names with year range in the name.
#' @param date_range Date vector of length 2 (start and end date).
#'
#' @return Filtered character vector of ZIP file names.
#' @export
filter_zip_by_daterange <- function(zip_names, date_range) {
  jahr_von <- as.numeric(format(date_range[1], "%Y"))
  jahr_bis <- as.numeric(format(date_range[2], "%Y"))
  zip_names[sapply(zip_names, function(z) {
    range <- regmatches(z, regexpr("\\d{4}-\\d{4}", z))
    if (length(range) == 1) {
      year_start <- as.numeric(sub("-.*", "", range))
      year_end <- as.numeric(sub(".*-", "", range))
      return(year_start <= jahr_bis && year_end >= jahr_von)
    }
    FALSE
  })]
}

#' List ZIP file names from a web page
#'
#' Loads the HTML page and extracts all ZIP file names from JSON-like structures.
#'
#' @param page_url Character. URL of the web page.
#'
#' @return Character vector with ZIP file names.
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

#' Create metadata table with information about ZIP files and their contents
#'
#' Reads all ZIP file names from a page and extracts information from the file names of the ZIP contents.
#'
#' @param page_url Character. URL of the page with ZIP files.
#'
#' @return data.frame with metadata about ZIP and contained files.
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
      year <- regmatches(f, regexpr("\\d{4}-\\d{4}", f))
      year_start <- as.numeric(sub("-.*", "", year))
      year_end <- as.numeric(sub(".*-", "", year))
      data.frame(
        zip = zip,
        file = f,
        station_id = parts[1],
        station_name = parts[2],
        year_start = year_start,
        year_end = year_end,
        type = ifelse(length(parts) > 4, parts[4], NA),
        unit = ifelse(length(parts) > 5, gsub("\\.csv$", "", parts[5]), NA),
        stringsAsFactors = FALSE
      )
    }))
    meta <- rbind(meta, info)
  }
  return(meta)
}

#' Find station files in metadata by ID or name and year range
#'
#' Filters metadata by station ID or name and a time period (start and end year).
#'
#' @param metadata data.frame. Metadata with columns 'station_id', 'station_name', 'year_start', 'year_end', 'zip', 'file'.
#' @param station_id Character or NULL. Station ID to filter.
#' @param station_name Character or NULL. Station name to filter.
#' @param startyear Numeric. Start year.
#' @param endyear Numeric. End year.
#'
#' @return data.frame with columns 'zip' and 'file' of matching files.
#' @export
find_station_files_in_metadata <- function(metadata, station_id = NULL, station_name = NULL, startyear, endyear) {
  if (!is.null(station_id)) {
    sel <- metadata$station_id == station_id
  } else if (!is.null(station_name)) {
    sel <- metadata$station_name == station_name
  } else {
    stop("Please provide either station_id or station_name.")
  }
  sel <- sel & metadata$year_start <= endyear & metadata$year_end >= startyear
  result <- metadata[sel, c("zip", "file")]
  rownames(result) <- NULL
  return(result)
}

#' Load filtered station data from ZIP archives and combine them
#'
#' Loads all relevant files from ZIP archives for the specified year range,
#' reads the CSV data, and combines them into a data frame.
#'
#' @param page_url Character. Base URL of the page with ZIP files.
#' @param file_index data.frame with columns 'zip' and 'file'.
#' @param startyear Numeric. Start year for filtering.
#' @param endyear Numeric. End year for filtering.
#'
#' @return data.frame with combined data or NULL if no files found.
#' @importFrom dplyr filter bind_rows
#' @importFrom vroom vroom locale
#' @importFrom stats na_if
#' @export
load_filtered_station_data <- function(page_url, file_index, startyear, endyear) {
  file_index_filtered <- file_index %>%
    dplyr::filter(sapply(file, function(f) {
      year_range <- regmatches(f, regexpr("\\d{4}-\\d{4}", f))
      if (length(year_range) == 1) {
        year_start <- as.numeric(sub("-.*", "", year_range))
        year_end <- as.numeric(sub(".*-", "", year_range))
        year_start <= endyear && year_end >= startyear
      } else {
        FALSE
      }
    }))

  if (nrow(file_index_filtered) == 0) {
    warning("No files found in the specified period.")
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