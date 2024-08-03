library(shiny)
library(rgbif)
library(mapview)
library(CoordinateCleaner)
library(janitor)
library(lubridate)
library(leaflet)
library(sf)
library(tidyr)
library(dplyr)
library(bslib)

# Function to retrieve and clean GBIF data
get_clean_gbif_data <- function(species_name, limit = 100000, filters) {
  key <- name_suggest(q = species_name, rank = 'species')$data$key[1]
  
  dat <- occ_search(taxonKey = key, limit = limit)
  
  cleaned_data <- dat$data |> 
    clean_names() %>%
    drop_na(decimal_latitude) |> 
    drop_na(decimal_longitude) |> 
    drop_na(species) |> 
    drop_na(country_code) |> 
    drop_na(event_date) |> 
    mutate(
      year = year(event_date),
      month = month(event_date)
    ) |> 
    filter(
      year >= 1900,
      year <= as.numeric(format(Sys.Date(), "%Y"))
    )
  
  if ("remove_sea" %in% filters) {
    cleaned_data <- cleaned_data %>% cc_sea(lon = "decimal_longitude", lat = "decimal_latitude")
  }
  if ("remove_zero" %in% filters) {
    cleaned_data <- cleaned_data %>% cc_zero(lon = "decimal_longitude", lat = "decimal_latitude")
  }
  if ("remove_invalid" %in% filters) {
    cleaned_data <- cleaned_data %>% cc_val(lon = "decimal_longitude", lat = "decimal_latitude")
  }
  if ("remove_capitals" %in% filters) {
    cleaned_data <- cleaned_data %>% cc_cap(lon = "decimal_longitude", lat = "decimal_latitude")
  }
  if ("remove_duplicates" %in% filters) {
    cleaned_data <- cleaned_data %>% cc_dupl(lon = "decimal_longitude", lat = "decimal_latitude")
  }
  
  cleaned_data <- cleaned_data %>%
    distinct(species, decimal_longitude, decimal_latitude, event_date, .keep_all = TRUE)
  
  return(cleaned_data)
}

# UI
ui <- page_fluid(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  
  h1("GBIF Species Data Viewer", class = "mt-4 mb-4"),
  
  accordion(
    accordion_panel(
      "About This App",
      p("This app allows you to visualize and clean species occurrence data from the Global Biodiversity Information Facility (GBIF)."),
      p("To use the app:"),
      tags$ol(
        tags$li("Enter a species name in the input box."),
        tags$li("Adjust the number of records to retrieve if needed. Large numbers may take time to process."),
        tags$li("Select the data quality filters you want to apply."),
        tags$li("Click the 'Get Data and Map' button to fetch and display the data.")
      )
    )
  ),
  layout_columns(
    col_widths = c(2, 10),
    card(
      card_header("Input Parameters"),
      card_body(
        textInput("species_name", "Enter species name:", "Salamandra atra"),
        numericInput("limit", "Number of records to retrieve:", 300, min = 1, max = 100000),
        checkboxGroupInput("filters", "Select data quality filters:",
                           choices = c("Remove sea points" = "remove_sea",
                                       "Remove zero coordinates" = "remove_zero",
                                       "Remove invalid coordinates" = "remove_invalid",
                                       "Remove country capitals" = "remove_capitals",
                                       "Remove duplicates" = "remove_duplicates"),
                           selected = c("remove_sea", "remove_zero", "remove_invalid")),
        actionButton("go", "Get Data", class = "btn-primary")
      )
    ),
    card(
      card_header("Species Occurrence Map"),
      card_body(
        leafletOutput("map", height = "500px")
      ),
      card_footer(
        textOutput("data_summary")
      )
    )
  )
  
)

# Server (remains largely the same)
server <- function(input, output, session) {
  
  species_data <- eventReactive(input$go, {
    withProgress(message = 'Fetching data...', value = 0, {
      data <- get_clean_gbif_data(input$species_name, input$limit, input$filters)
      incProgress(1)
      return(data)
    })
  })
  
  output$map <- renderLeaflet({
    req(species_data())
    data_sf <- species_data() %>%
      st_as_sf(coords = c("decimal_longitude", "decimal_latitude"), crs = st_crs(4326))
    
    mapview(data_sf, legend = TRUE)@map
  })
  
  output$data_summary <- renderText({
    req(species_data())
    paste("Number of records after cleaning:", nrow(species_data()))
  })
}

# Run the app
shinyApp(ui = ui, server = server)