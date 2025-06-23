
library(shiny)
library(plotly)
library(vroom)
library(dplyr)

page_url <- "https://www.opengeodata.nrw.de/produkte/umwelt_klima/wasser/oberflaechengewaesser/hydro/q/"
zip_names <- list_hydrodata_files_nrw(page_url)
ezg_liste <- unique(sub("-.*", "", zip_names))

# Hilfsfunktion: Datumsbereich aus ZIP-Namen extrahieren
extract_years <- function(zip_names) {
  years <- regmatches(zip_names, gregexpr("\\d{4}-\\d{4}", zip_names))
  years <- lapply(years, function(x) if(length(x)) unlist(strsplit(x, "-")) else c(NA, NA))
  years_df <- do.call(rbind, years)
  colnames(years_df) <- c("start", "end")
  years_df <- as.data.frame(years_df, stringsAsFactors = FALSE)
  years_df$start <- as.numeric(years_df$start)
  years_df$end <- as.numeric(years_df$end)
  years_df
}
years_df <- extract_years(zip_names)
min_date <- as.Date(paste0(min(years_df$start, na.rm = TRUE), "-01-01"))
max_date <- as.Date(paste0(max(years_df$end, na.rm = TRUE), "-12-31"))

ui <- fluidPage(
  titlePanel("Hydrologische Zeitreihen NRW"),
  sidebarLayout(
    sidebarPanel(
      dateRangeInput("plot_range", "Datumsbereich wählen:",
                     start = min_date, end = max_date,
                     min = min_date, max = max_date),
      selectInput("ezg", "Einzugsgebiet wählen:", choices = ezg_liste),
      uiOutput("station_selector")
    ),
    mainPanel(
      plotlyOutput("ts_plot")
    )
  )
)

server <- function(input, output, session) {
  # Filtere ZIPs nach Einzugsgebiet und Datumsbereich
  filtered_zips <- reactive({
    req(input$plot_range, input$ezg)
    jahr_von <- as.numeric(format(input$plot_range[1], "%Y"))
    jahr_bis <- as.numeric(format(input$plot_range[2], "%Y"))
    zip_names[sapply(zip_names, function(z) {
      bereich <- regmatches(z, regexpr("\\d{4}-\\d{4}", z))
      if (length(bereich) == 1) {
        jahr_start <- as.numeric(sub("-.*", "", bereich))
        jahr_ende <- as.numeric(sub(".*-", "", bereich))
        return(grepl(input$ezg, z) && jahr_start <= jahr_bis && jahr_ende >= jahr_von)
      }
      FALSE
    })]
  })

  # Lade alle Stationsnamen aus den gefilterten ZIPs
  station_choices <- reactive({
    req(filtered_zips())
    descrs <- descr_names(page_url, filtered_zips())
    unique(unlist(descrs))
  })

  output$station_selector <- renderUI({
    selectInput("station", "Station/Datei wählen:", choices = station_choices())
  })

  # Lade und verbinde Zeitreihen für die gewählte Station
  output$ts_plot <- renderPlotly({
    req(input$station, filtered_zips())
    descrs <- descr_names(page_url, filtered_zips())
    # Finde alle ZIPs, die die gewählte Station enthalten
    relevant_zips <- names(descrs)[sapply(descrs, function(files) input$station %in% files)]
    if (length(relevant_zips) == 0) return(NULL)
    all_data <- list()
    for (zip_url in relevant_zips) {
      tmp_zip <- tempfile(fileext = ".zip")
      curl::curl_download(paste0(page_url, zip_url), tmp_zip)
      data <- vroom::vroom(
        unz(tmp_zip, input$station),
        delim = ";",
        show_col_types = FALSE,
        locale = vroom::locale(decimal_mark = ",")
      )
      unlink(tmp_zip)
      all_data[[zip_url]] <- data
    }
    df <- bind_rows(all_data)
    # Filter auf Datumsbereich
    df$date_only <- as.Date(df$dateTime)
    df <- df[df$date_only >= input$plot_range[1] & df$date_only <= input$plot_range[2], ]
    plot_ly(
      df,
      x = ~as.POSIXct(dateTime),
      y = ~`value[m³/s]`,
      type = "scatter",
      mode = "lines"
    ) %>%
      layout(
        title = paste(input$station, paste(input$plot_range, collapse = " bis ")),
        xaxis = list(title = "Zeit"),
        yaxis = list(title = "Abfluss [m³/s]")
      )
  })
}

shinyApp(ui, server)
