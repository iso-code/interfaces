#' Get the raw data from the LANUV datasource
#'
#' @export
#' @description Returns a dataframe containing the most recent data.
#' @param hub The file location you are querying data from. Either one of the defaults or a URL.
#'  See \href{https://github.com/iso-code/interfaces}{README}.
#' @param query the type of data either precipitation or water level data.
#' @return A dataframe with three columns: station_no, time and value.
#' @examples
#' \dontrun{
#' get_rawdata_nrw(hub = 'rawlanuv',query = 'pegel',descr = 'pegel_stand')
#' }
#'

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





