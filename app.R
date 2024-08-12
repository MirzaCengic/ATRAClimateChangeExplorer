library(shiny)
library(rgbif)
library(terra)
library(leaflet)
library(dplyr)
library(here)
library(ggplot2)
library(memoise)
library(cachem)
library(shinyBS)
library(DT)
library(testthat)
library(forcats)
library(dplyr)
library(ggiraph)
library(tidyr)
library(shinyWidgets)  # For enhanced input widgets
library(stringr)
library(sf)
library(rnaturalearth)
library(raster)
library(cowplot)
library(patchwork)
# Create a cache
cache <- cachem::cache_mem()

# 
ne_sf <- ne_countries(returnclass = "sf", country = "Bosnia and Herzegovina", scale = 10)


source("carousel_panel.R")

source(here("R", "functions.R"))

# Panel div for visualization
# override the currently broken definition in shinyLP version 1.1.0
panel_div <- function(class_type, content) {
  div(class = sprintf("panel panel-%s", class_type),
      div(class = "panel-body", content)
  )
}




# Assuming you have a CSV file with the list of species
# species_list <- read.csv("amphibian_reptile_species.csv", stringsAsFactors = FALSE)$species_name

# species_list <- c("Vipera ammodytes", "Salamandra atra", "Salamandra salamandra", "Natrix natrix")

species_data <- data.frame(
  name = c("Vipera ursinii", "Vipera berus", "Elaphe quatuorlineata", "Algyroides nigropunctatus",
    "Pseudopus apodus", "Dinarolacerta mosorensis", "Dalmatolacerta oxycephala",
    "Zootoca vivipara", "Podarcis melisellensis", "Podarcis muralis",
    "Lacerta agilis", "Anguis fragilis", "Lacerta viridis", "Vipera ammodytes",
    "Dermochelys coriacea", "Zamenis longissimus", "Coronella austriaca",
    "Natrix natrix", "Natrix tessellata", "Platyceps najadum",
    "Hierophis gemonensis", "Zamenis situla", "Telescopus fallax",
    "Hemidactylus turcicus", "Testudo hermanni", "Emys orbicularis",
    "Caretta caretta", "Lacerta trilineata", "Ablepharus kitaibelii",
    "Malpolon insignitus", "Dolichopis caspius",
    "Pelodiscus sinensis", "Trachemys scripta",
    "Proteus anguinus", "Lissotriton vulgaris", "Salamandra atra", "Bombina bombina", 
    "Bombina variegata", "Pelobates fuscus", "Bufo bufo", "Bufotes viridis", "Hyla arborea",
    "Ichthyosaura alpestris", "Rana dalmatina", "Rana graeca", "Pelophylax lessonae", 
    "Triturus carnifex", "Triturus macedonicus", "Lissotriton graecus", 
    "Pelophylax kl. esculentus", "Pelophylax ridibundus", "Rana temporaria",
    "Salamandra salamandra", "Triturus dobrogicus"),
  class = c("Reptilia", "Reptilia", "Reptilia", "Reptilia", "Reptilia", 
            "Reptilia", "Reptilia", "Reptilia", "Reptilia", "Reptilia", "Reptilia", 
            "Reptilia", "Reptilia", "Reptilia", "Reptilia", "Reptilia", "Reptilia", 
            "Reptilia", "Reptilia", "Reptilia", "Reptilia", "Reptilia", "Reptilia", 
            "Reptilia", "Reptilia", "Reptilia", "Reptilia", "Reptilia", "Reptilia", 
            "Reptilia", "Reptilia", "Reptilia", "Reptilia", 
            "Amphibia", "Amphibia", "Amphibia", "Amphibia", "Amphibia", 
            "Amphibia", "Amphibia", "Amphibia", "Amphibia", "Amphibia", "Amphibia", 
            "Amphibia", "Amphibia", "Amphibia", "Amphibia", "Amphibia", "Amphibia", 
            "Amphibia", "Amphibia", "Amphibia", "Amphibia")
)


# Create a grouped list for selectize input
species_list <- split(species_data$name, species_data$class)

# Memoise the climate data extraction function
extract_climate_data_cached <- memoise(extract_climate_data, cache = cache)


# UI
ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "flatly"),
  titlePanel("GBIF Species and Climate Data Explorer"),
  sidebarLayout(
    sidebarPanel(
      
      # In the UI section:
      selectizeInput("species_name", "Select species:",
                     choices = species_list,
                     options = list(
                       placeholder = 'Start typing a species name',
                       maxOptions = 100,
                       create = FALSE,
                       onInitialize = I('function() { this.setValue(""); }')
                     )
      ),
      bsTooltip("species_name", "Enter the scientific name of the species you want to analyze", placement = "right", trigger = "hover"),
      
      numericInput("n_records", "Number of records to retrieve:", 100, min = 1, max = 10000),
      bsTooltip("n_records", "Maximum number of occurrence records to retrieve from GBIF", placement = "right", trigger = "hover"),
      
      selectInput("chelsa_var", "Select CHELSA bioclimatic variable:",
                  choices = setNames(paste0("bio", 1), paste("BIO", 1, "- Mean Annual Air Temperature (°C)"))),
      bsTooltip("chelsa_var", "Choose the bioclimatic variable for analysis", placement = "right", trigger = "hover"),
      
      actionButton("go", "Retrieve Data and Analyze", class = "btn-primary")
      
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Map", leafletOutput("map")),
        tabPanel("Climate Comparison", girafeOutput("climate_plot", width = "75%")),
        # tabPanel("Climate Comparison", plotOutput("climate_plot")),
        
        tabPanel("Summary", verbatimTextOutput("summary")),
        tabPanel("Data Table", DTOutput("data_table"))
        # tabPanel("Advanced Analysis", 
        #          plotOutput("advanced_plot"),
        #          verbatimTextOutput("advanced_summary"))
      )
    )
  )
)


# Server
server <- function(input, output, session) {
  
  # Update selectize input with all species
  # updateSelectizeInput(session, "species_name", choices = species_list, server = TRUE)
  observe({
    updateSelectizeInput(session, "species_name",
                         choices = species_list,
                         selected = NULL,
                         server = FALSE  # Changed to FALSE as we're not using server-side rendering for this grouped list
    )
  })
  
  # Reactive value to store all data
  all_data <- reactiveVal(NULL)
  
  # Reactive event to retrieve and process data
  observeEvent(input$go, {
    withProgress(message = 'Retrieving and processing data...', value = 0, {
      tryCatch({
        # Get GBIF data
        # species_data <- get_gbif_data_cached(input$species_name, input$n_records)
        # incProgress(0.3)
        # 
        # 
        # # Extract current climate data
        # current_path <- here("data", "chelsa", "current", paste0(input$chelsa_var, ".tif"))
        # current_values <- extract_climate_data_cached(species_data[, c("decimalLongitude", "decimalLatitude")], current_path)
        # incProgress(0.3)
        # 
        # # Extract future climate data (2070 SSP1)
        # future_path <- here("data", "chelsa", "future", paste0(input$chelsa_var, ".tif"))
        # future_values <- extract_climate_data_cached(species_data[, c("decimalLongitude", "decimalLatitude")], future_path)
        
        result <- prepare_climate_trajectory_data(
          species_name = input$species_name,
          bio_var = readr::parse_number(input$chelsa_var),
          ssp = "ssp370",
          gcm = "ipsl-cm6a-lr",
          limit = input$n_records
        )
        incProgress(0.7)
        
        # Combine all data
        # combined_data <- species_data %>%
        #   mutate(current_climate = current_values[,2],
        #          future_climate = future_values[,2]) |> 
        #   mutate(
        #     label = stringr::str_glue("Species: {species}<br>
        #                                Record: {basisOfRecord}<br>
        #                                Year of record: {year}<br>
        #                                Reference: {references}")
        #   )
        # 
        all_data(result)
        incProgress(0.3)
      }, error = function(e) {
        showNotification(paste("Error:", e$message), type = "error")
        all_data(NULL)
      })
    })
  })
  
  # Render map
  output$map <- renderLeaflet({
    req(all_data())
    leaflet(all_data()) %>%
      addTiles() %>%
      addCircleMarkers(~decimalLongitude, ~decimalLatitude, popup = ~species)
  })
  
  # Render climate comparison boxplot
  output$climate_plot <- #renderPlot({
    renderGirafe({
    req(all_data())
    
    raster_data <- load_basic_climate(readr::parse_number(input$chelsa_var))
    
    
    p1 <- plot_climate_trajectory_left(data = all_data(), raster = raster_data, country = ne_sf)
    
    p2 <- plot_climate_trajectory_right(data = all_data(), raster = raster_data, country = ne_sf)
    
    # data_long <- all_data() %>%
    #   select(species, current_climate, future_climate) %>%
    #   tidyr::pivot_longer(cols = c(current_climate, future_climate),
    #                       names_to = "period", values_to = "value")
    
    # p <- ggplot(data_long, aes(x = period, y = value)) +
    #   geom_boxplot(aes(fill = period), outlier.shape = NA) +
    #   geom_point_interactive(aes(fill = period, data_id = value, tooltip = value), position = position_jitter(width = 0.2), shape = 21) +
    #   labs(x = "Period", y = "Climate Value",
    #        title = paste("Comparison of", input$chelsa_var, "values"),
    #        subtitle = input$species_name) +
    #   theme_minimal() +
    #   theme(
    #     legend.position = "none"
    #   ) +
    #   scale_fill_brewer(palette = "Set2")
    
    # x <- girafe(ggobj = p1)
    # p1
    x <- girafe(ggobj = (p1 + p2))
    
    x
  })
  
  # Render summary
  output$summary <- renderPrint({
    req(all_data())
    
    cat("Species:", input$species_name, "\n")
    cat("Number of records:", nrow(all_data()), "\n")
    # cat("Current climate (mean):", mean(all_data()$current_climate, na.rm = TRUE), "\n")
    # cat("Future climate (mean):", mean(all_data()$future_climate, na.rm = TRUE), "\n")
    # cat("Climate change (mean difference):", 
    #     mean(all_data()$future_climate - all_data()$current_climate, na.rm = TRUE), "\n")
  })
  
  # Render data table
  output$data_table <- renderDT({
    req(all_data())
    datatable(all_data(), options = list(pageLength = 10))
  })
  # ggiraph::renderGirafe()
  # Advanced analysis
  # output$advanced_plot <- renderPlot({
  #   req(all_data())
  #   ggplot(all_data(), aes(x = current_climate, y = future_climate)) +
  #     geom_point(alpha = 0.5) +
  #     geom_smooth(method = "lm", color = "red") +
  #     geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  #     labs(x = "Current Climate", y = "Future Climate",
  #          title = paste("Relationship between Current and Future", input$chelsa_var),
  #          subtitle = input$species_name) +
  #     theme_minimal()
  #   
  #   
  # })
  
  # output$advanced_summary <- renderPrint({
  #   req(all_data())
  #   model <- lm(future_climate ~ current_climate, data = all_data())
  #   summary(model)
  # })
  
}

# Run the app
shinyApp(ui = ui, server = server)