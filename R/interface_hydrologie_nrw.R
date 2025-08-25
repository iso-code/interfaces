#' Download and read raw NRW data from ZIP file
#'
#' Downloads a ZIP archive from `hub + query`, extracts a file named `descr`,
#' and loads it as a CSV. Useful for accessing LANUV NRW raw datasets.
#'
#' @param hub Character. Base URL or shortcut (passed to [check_hub()]).
#' @param query Character. ZIP file name or shortcut (passed to [check_query_lanuv()]).
#' @param descr Character. File inside the ZIP to extract (passed to [check_descr_lanuv()]).
#'
#' @return A `tibble` (from [vroom::vroom()]) containing the dataset.
#' @details The ZIP archive is downloaded to a temporary file and removed afterwards.
#'
#' @examples
#' \dontrun{
#' df <- get_rawdata_nrw("raw_nrw", "pegel", "pegel_stand")
#' }
#' @importFrom curl curl_download
#' @importFrom vroom vroom
#' @export
get_rawdata_nrw <- function(hub, query, descr) {
  hub   <- check_hub(hub)
  query <- check_query_lanuv(query)
  descr <- check_descr_lanuv(descr)

  loc <- paste0(hub, query)
  tmp_zip <- tempfile(fileext = ".zip")
  tmp_dir <- tempfile()
  on.exit(unlink(c(tmp_zip, tmp_dir), recursive = TRUE), add = TRUE)

  curl::curl_download(loc, destfile = tmp_zip)
  extracted <- unzip(tmp_zip, files = descr, exdir = tmp_dir)

  vroom::vroom(extracted, delim = ";", col_names = TRUE, show_col_types = FALSE)
}

#' Extract file names from ZIP archives on a web page
#'
#' Downloads ZIP files listed on a webpage and returns the names of all files inside them.
#'
#' @param page_url Character. Base URL where the ZIP files are hosted.
#' @param zip_names Character vector. Names of the ZIP archives to inspect.
#'
#' @return A named list: each ZIP file name is a key with a vector of contained files as value.
#'
#' @examples
#' \dontrun{
#' descr_names("https://www.hochwasserportal.nrw/data/downloads/", c("file1.zip"))
#' }
#' @importFrom curl curl_download
#' @export
descr_names <- function(page_url, zip_names) {
  result <- list()

  for (zip_name in zip_names) {
    message("Processing: ", zip_name)
    tmp_zip <- tempfile(fileext = ".zip")
    on.exit(unlink(tmp_zip), add = TRUE)

    curl::curl_download(paste0(page_url, zip_name), tmp_zip)
    result[[zip_name]] <- unzip(tmp_zip, list = TRUE)$Name
  }

  result
}

#' Download hydro data from ZIP archive and read shapefile or CSV
#'
#' Downloads a ZIP archive from `hub + query`, extracts either all files or
#' the specified `descr`, and loads shapefiles as `sf` objects or CSVs as tibbles.
#'
#' @param hub Character. Base URL.
#' @param query Character. File path or query string.
#' @param descr Character or NULL. Optional: restrict extraction to these files.
#'
#' @return An `sf` object (for shapefile), a `tibble` (for CSV), or `NULL` if no suitable file is found.
#' @details ZIP files are downloaded and removed after extraction.
#'
#' @examples
#' \dontrun{
#' shp <- download_hydrodata_nrw("verified_level_nrw", "stations.zip")
#' }
#' @importFrom curl curl_download
#' @importFrom vroom vroom
#' @import sf
#' @export
download_hydrodata_nrw <- function(hub, query, descr = NULL) {
  tmp_zip <- tempfile(fileext = ".zip")
  tmp_dir <- tempfile()
  on.exit(unlink(c(tmp_zip, tmp_dir), recursive = TRUE), add = TRUE)

  curl::curl_download(paste0(hub, query), destfile = tmp_zip)

  files <- if (is.null(descr)) {
    unzip(tmp_zip, exdir = tmp_dir)
  } else {
    unzip(tmp_zip, files = descr, exdir = tmp_dir)
  }

  shp_file <- files[grepl("\\.shp$", files, ignore.case = TRUE)]
  if (length(shp_file) > 0) {
    return(sf::st_read(shp_file[1], quiet = TRUE))
  }

  csv_file <- files[grepl("\\.csv$", files, ignore.case = TRUE)]
  if (length(csv_file) > 0) {
    return(vroom::vroom(csv_file[1], delim = ";", col_names = TRUE, show_col_types = FALSE))
  }

  NULL
}

#' Filter ZIP file names by year range
#'
#' Selects ZIP file names that contain a year range (e.g., `"1980-1989"`) and
#' overlap with the given date range.
#'
#' @param zip_names Character vector. ZIP file names with year ranges.
#' @param date_range Date vector of length 2 (start and end).
#'
#' @return Filtered character vector of ZIP file names.
#' @examples
#' filter_zip_by_daterange(c("1980-1989.zip", "1990-1999.zip"), as.Date(c("1985-01-01","1992-12-31")))
#' @export
filter_zip_by_daterange <- function(zip_names, date_range) {
  year_start <- as.numeric(format(date_range[1], "%Y"))
  year_end   <- as.numeric(format(date_range[2], "%Y"))

  zip_names[sapply(zip_names, function(z) {
    range <- regmatches(z, regexpr("\\d{4}-\\d{4}", z))
    if (length(range) == 1) {
      y1 <- as.numeric(sub("-.*", "", range))
      y2 <- as.numeric(sub(".*-", "", range))
      return(y1 <= year_end && y2 >= year_start)
    }
    FALSE
  })]
}

#' List ZIP file names from a web page
#'
#' Loads an HTML page and extracts all ZIP file names from JSON-like structures.
#'
#' @param page_url Character. URL of the web page.
#'
#' @return Character vector with ZIP file names.
#'
#' @examples
#' \dontrun{
#' list_page_zips("https://www.hochwasserportal.nrw/data/downloads/")
#' }
#' @importFrom httr GET content
#' @export
list_page_zips <- function(page_url) {
  html <- httr::GET(page_url)
  content <- httr::content(html, as = "text", encoding = "UTF-8")

  zips <- regmatches(content, gregexpr('"name":"([^"]+\\.zip)"', content))[[1]]
  unique(gsub('.*"name":"([^"]+\\.zip)".*', '\\1', zips))
}

#' Create metadata table with information about ZIP files and their contents
#'
#' Collects all ZIP file names from a page and parses file names inside them
#' into structured metadata (station info, years, type, unit).
#'
#' @param page_url Character. URL of the page with ZIP files.
#'
#' @return A `data.frame` with metadata: one row per contained file.
#'
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
      # Only process if at least station_id and station_name are present
if (length(parts) >= 2) {
  # year kann dann NA sein
  year <- regmatches(f, regexpr("\\d{4}-\\d{4}", f))
  year_start <- if (length(year) == 1) as.numeric(sub("-.*", "", year)) else NA
  year_end   <- if (length(year) == 1) as.numeric(sub(".*-", "", year)) else NA
  data.frame(
    zip = zip,
    file = f,
    station_id = parts[1],
    station_name = parts[2],
    year_start = year_start,
    year_end   = year_end,
    type = ifelse(length(parts) > 4, parts[4], NA),
    unit = ifelse(length(parts) > 5, gsub("\\.csv$", "", parts[5]), NA),
    stringsAsFactors = FALSE
  )
} else {
  NULL
}
    }))

    meta <- rbind(meta, info)
  }

  meta
}

#' Find station files in metadata by ID or name and year range
#'
#' Filters metadata by station ID or station name and by time period.
#'
#' @param metadata data.frame. With columns 'station_id', 'station_name', 'year_start', 'year_end', 'zip', 'file'.
#' @param station_id Character or NULL. Station ID to filter.
#' @param station_name Character or NULL. Station name to filter.
#' @param startyear Numeric. Start year.
#' @param endyear Numeric. End year.
#'
#' @return A data.frame with columns 'zip' and 'file' of matching entries.
#' @export
find_station_files_in_metadata <- function(metadata, station_id = NULL, station_name = NULL, startyear, endyear) {
  if (!is.null(station_id)) {
    sel <- metadata %>% filter(station_id %in% !!station_id)
  } else if (!is.null(station_name)) {
    sel <- metadata %>% filter(station_name %in% !!station_name)
  } else {
    stop("Provide either station_id or station_name.")
  }

  sel <- sel %>% filter(year_start <= endyear & year_end >= startyear)
  #result <- sel[sel, c("zip", "file")]
  rownames(sel) <- NULL
  sel
}

#' Load filtered station data from ZIP archives and combine them
#'
#' Loads relevant files from ZIP archives for the specified year range,
#' reads CSVs, cleans values, and combines them into a single data frame.
#'
#' @param page_url Character. Base URL of the page with ZIP files.
#' @param file_index data.frame with columns 'zip' and 'file'.
#' @param startyear Numeric. Start year.
#' @param endyear Numeric. End year.
#'
#' @return A combined `data.frame` or `NULL` if no files found.
#'
#' @importFrom dplyr filter bind_rows
#' @importFrom vroom vroom locale
#' @importFrom stats na_if
#' @export
load_filtered_station_data <- function(page_url, file_index, startyear, endyear) {
  # filter by year overlap
  file_index_filtered <- dplyr::filter(file_index, sapply(file, function(f) {
    year_range <- regmatches(f, regexpr("\\d{4}-\\d{4}", f))
    if (length(year_range) == 1) {
      y1 <- as.numeric(sub("-.*", "", year_range))
      y2 <- as.numeric(sub(".*-", "", year_range))
      return(y1 <= endyear && y2 >= startyear)
    }
    FALSE
  }))

  if (nrow(file_index_filtered) == 0) {
    warning("No files found in the specified period.")
    return(NULL)
  }

  all_data <- list()
  zip_groups <- split(file_index_filtered$file, file_index_filtered$zip)

  for (zip_name in names(zip_groups)) {
    files_to_load <- zip_groups[[zip_name]]
#zip_name_ascii <- zip_name %>%
#  gsub("ü", "ue", ., ignore.case = FALSE) %>%
#  gsub("Ü", "Ue", ., ignore.case = FALSE) %>%
#  gsub("ö", "oe", ., ignore.case = FALSE) %>%
#  gsub("Ö", "Oe", ., ignore.case = FALSE) %>%
#  gsub("ä", "ae", ., ignore.case = FALSE) %>%
#  gsub("Ä", "Ae", ., ignore.case = FALSE) %>%
#  gsub("ß", "ss", ., ignore.case = FALSE)
  zip_url <- paste0(page_url, zip_name)
  print(zip_url)

    tmp_zip <- tempfile(fileext = ".zip")
    on.exit(unlink(tmp_zip), add = TRUE)

    curl::curl_download(zip_url, tmp_zip)

    for (file in files_to_load) {
      df <- vroom::vroom(
        unz(tmp_zip, file),
        delim = ";",
        show_col_types = FALSE,
        locale = vroom::locale(decimal_mark = ",")
      )

      # replace "LUECKE" with NA in value columns
      value_cols <- grep("^value", names(df), value = TRUE)
      for (colname in value_cols) {
        if (any(grepl("LUECKE", df[[colname]]))) {
          df[[colname]] <- dplyr::na_if(df[[colname]], "LUECKE")
          df[[colname]] <- as.numeric(df[[colname]])
        }
      }

      all_data[[paste(zip_name, file, sep = "_")]] <- df
    }
  }

  if (length(all_data) == 0) return(NULL)
  dplyr::bind_rows(all_data)
}
