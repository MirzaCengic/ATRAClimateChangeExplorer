# app.R


# Load required libraries
library(shiny)
library(rgbif)
library(terra)
library(leaflet)
library(here)
library(ggplot2)
library(shinyBS)
library(DT)
library(scales)
library(CoordinateCleaner)
library(forcats)
library(dplyr)
library(ggiraph)
library(tidyr)
library(shinyWidgets)  
library(stringr)
library(sf)
library(rnaturalearth)
library(raster)
library(patchwork)
library(bslib)
library(ggthemes)
library(shinydashboard)  


# Load country shapefile
ne_sf <- st_read(here::here("data", "vector", "country_shapefile.gpkg"))

# Source functions from the functions.R file
source(here("R", "functions.R"))

# Assuming you have a CSV file with the list of species
# species_list <- read.csv("amphibian_reptile_species.csv", stringsAsFactors = FALSE)$species_name

# Prepare species data alphabetically for the dropdown menu
species_data <- species_data |> 
  arrange(class, name)

# Create a grouped list for selectize input
species_list <- split(species_data$name, species_data$class)


# UI code -----------------------------------------------------------------

ui <- navbarPage(
  title = "ATRA Climate Change Explorer",
  theme = bslib::bs_theme(bootswatch = "flatly", primary  = "#2e303c"),
  # Home panel
  nav_panel(
    title = "Home",
    layout_sidebar(
      sidebar = sidebar(
        width = 400,
        # title = "Sidebar title. Uncomment if you want to add a title.",
        # Species selection dropdown
        tags$div(
          selectizeInput("species_name", "Select species:",
                         choices = species_list,
                         options = list(
                           placeholder = 'Start typing a species name',
                           maxOptions = 100,
                           create = FALSE,
                           onInitialize = I('function() { this.setValue(""); }')
                         )
          )
        ),
        # CSS for custom button class - used to create space between buttons in Data Table tab
        tags$head(
          tags$style(HTML("
    .custom-button-class {
      margin-right: 10px !important;
    }
  "))),
        bsTooltip("tooltip_species", "", placement = "right", trigger = "hover"),
        # Number of records input
        tags$div(
          # This is the tooltip that's shown when the user hovers their mouse
          title = "Maximum number of of occurence records to retrieve from GBIF. Maximum limit is 10 000.",
          numericInput("n_records", "Number of records to retrieve:", 100, min = 1, max = 10000),
          id = "tooltip_records"
        ),
        bsTooltip("tooltip_records", "", placement = "right", trigger = "hover"),
        # Data quality filters
        tags$div(
          # This is the tooltip that's shown when the user hovers their mouse
          title = "Add data quality filters to GBIF query with {CoordinateCleaner} R package. See 'About' page for more info.",
          selectizeInput("cc_filters", "Select data quality filters:",
                         choices = c(
                           "Remove zero coordinates" = "remove_zero",
                           "Remove equal records" = "remove_equal",
                           "Remove centroid records" = "remove_centroids",          
                           "Remove invalid records" = "remove_invalid",
                           "Remove records in capital cities" = "remove_capitals",
                           "Remove duplicates" = "remove_duplicates",
                           "Remove records in urban areas" = "remove_urban",
                           "Remove records in sea" = "remove_sea"),
                         multiple = TRUE,
                         options = list(placeholder = 'Select filters')
          ),
          id = "tooltip_filters"
        ),
        bsTooltip("tooltip_filters", "", placement = "right", trigger = "hover"),
        # Bioclimatic variable selection
        tags$div(
          title = "Choose the bioclimatic variable for analysis. See 'About' page for more info.",
          
          selectInput("chelsa_var", "Select CHELSA bioclimatic variable:",
                      choices = bioclim_choices),
          id = "tooltip_bioclim"
        ),
        bsTooltip("tooltip_bioclim", "", placement = "right", trigger = "hover"),
        
        # Climate scenario selection
        tags$div(
          title = "Choose the SSP (Shared Socioeconomic Pathway) climate scenario. See 'About' page for more info.",
          
          selectInput("ssp_var", "Select climate scenario:",
                      choices = setNames(
                        # ,
                        c("ssp126", "ssp370", "ssp585"),
                        c("SSP126", "SSP370", "SSP585")
                      )
          ),
          id = "tooltip_scenario"
        ),
        bsTooltip("tooltip_scenario", "", placement = "right", trigger = "hover"),
        
        # Button to trigger data retrieval and analysis
        actionButton("go", "Retrieve Data and Analyze", class = "btn-primary")
      ),
      # Main panel with tabs
      tabsetPanel(
        # Map tab
        tabPanel("Map", 
                 leafletOutput("map"),
                 tags$br(),  # Add some space between the map and summary
                 fluidRow(
                   column(12, 
                          uiOutput("summary_title"),
                          uiOutput("query_summary"),
                          tags$br(),
                          fluidRow(
                            column(4, valueBoxOutput("records_box", width = NULL)),
                            column(4, valueBoxOutput("quality_box", width = NULL)),
                            column(4, valueBoxOutput("climate_box", width = NULL))
                          )))
        ),
        # Climate Comparison tab
        tabPanel("Climate Comparison", 
                 girafeOutput("climate_plot"),
                 tags$br(),
                 uiOutput("climate_summary")
        ),
        # Data Table tab
        tabPanel("Data Table", 
                 tags$br(),
                 DTOutput("data_table")
        )
      )
    )
  ),
  # About panel
  nav_panel(
    title = "About",
    fluidRow(
      column(
        width = 12,
        h2("About This App"),
        p("This Shiny app allows users to explore GBIF species occurrence data and associated climate information."),
        p("Use the sidebar on the main page to select a species, specify the number of records to retrieve, and choose a bioclimatic variable for analysis."),
        tags$ol(
          tags$li("Select a species from the dropdown menu."),
          tags$li("Specify the number of occurrence records you want to retrieve."),
          tags$li("Choose a CHELSA bioclimatic variable for analysis."),
          tags$li("Click 'Retrieve Data and Analyze' to process the data."),
          tags$li("Explore the results in the various tabs: Map, Climate Comparison, Summary, and Data Table.")
          
          
        ),
        h3("About SSP climate scenarios"),
        p("This Shiny app uses three SSP scenarios (Shared Socioeconomic Pathways):"),
        p(tags$strong("SSP126:"), "This scenario with 2.6 W/m² by the year 2100 is a remake of the optimistic scenario RCP2.6 and was designed with the aim of simulating a development that is compatible with the 2°C target. This scenario assumes climate protection measures being taken."),
        p(tags$strong("SSP370:"), "With 7 W/m² by the year 2100, this scenario is in the upper-middle part of the full range of scenarios. It was newly introduced after the RCP scenarios, closing the gap between RCP6.0 and RCP8.5."),
        p(tags$strong("SSP585:"), "With an additional radiative forcing of 8.5 W/m² by the year 2100, this scenario represents the upper boundary of the range of scenarios described in the literature. It can be understood as an update of the CMIP5 scenario RCP8.5, now combined with socioeconomic reasons.")
      )
    )
  ),
  # How to build your own app panel
  nav_panel(
    title = "How to build your own?",
    fluidRow(
      column(
        width = 12,
        h2("Tutorial to reproduce"),
        p("To be filled")
      )
    )
  )
)


# Server code -------------------------------------------------------------

server <- function(input, output, session) {
  
  # Update selectize input with all species
  observe({
    updateSelectizeInput(session, "species_name",
                         choices = species_list,
                         selected = NULL,
                         server = FALSE  # Changed to FALSE as we're not using server-side rendering for this grouped list
    )
  })
  
  # Reactive value to store all data
  all_data <- reactiveVal(NULL)
  
  
  # Observer for the "Retrieve Data and Analyze" button
  observeEvent(input$go, {
    # Progress bar. There are a few steps happening, so progress bar goes from 0% to 100% quickly.
    withProgress(message = 'Retrieving and processing data...', value = 0, {
      tryCatch({
        # Prepare climate trajectory data
        result <- prepare_climate_trajectory_data(
          species_name = input$species_name,
          bio_var = readr::parse_number(input$chelsa_var),
          ssp = input$ssp_var,
          gcm = "gfdl-esm4",
          limit = input$n_records,
          filters = input$cc_filters
        )
        
        # Add the selected bioclimatic variable to the result
        result$bioclim_layer <- input$chelsa_var
        
        # Store the result in the reactive value
        all_data(result)
        incProgress(1)
      }, error = function(e) {
        showNotification(paste("Error:", e$message), type = "error")
        all_data(NULL)
      })
    })
  })
  
  # Render interactive map
  
  output$map <- renderLeaflet({
    req(all_data())
    leaflet(all_data()) |> 
      addTiles() |> 
      addCircleMarkers(
        ~decimalLongitude, 
        ~decimalLatitude,
        color = "green",
        fillOpacity = 0.8,
        radius = 6,
        popup = ~paste("<strong>Species:</strong>", species, "<br><strong>Record type:</strong>", basisOfRecord, "<br><strong>Record year:</strong>", year, "<br><strong>Reference:</strong>", references)
      )
  })
  # Render climate comparison plot
  output$climate_plot <- renderGirafe({
    req(all_data())
    
    raster_data <- load_basic_climate(readr::parse_number(input$chelsa_var))
    
    p1 <- plot_climate_trajectory_left(data = all_data(), raster = raster_data, country = ne_sf, bio_var = input$chelsa_var)
    p2 <- plot_climate_trajectory_right(data = all_data(), raster = raster_data, country = ne_sf, bio_var = input$chelsa_var)
    
    bio_info <- get_bioclim_info(input$chelsa_var)
    
    x <- girafe(ggobj = (p1 + p2) + plot_annotation(title = str_glue("{bio_info$label} ({toupper(input$ssp_var)})"),
                                                    theme = theme(
                                                      plot.title = element_text(size = 12),
                                                      plot.margin = margin(-120, 1, 1, 1)
                                                    )),
                
                options = list(
                  opts_toolbar(position = "bottom")
                  
                )
    )
    
    
    x
  })
  
  # Render summary
  output$summary <- renderPrint({
    req(all_data())
    
    cat("Species:", input$species_name, "\n")
    cat("Number of records:", nrow(all_data()), "\n")
    cat("Current climate (mean):", mean(all_data()$current, na.rm = TRUE), "\n")
    cat("Future climate (mean):", mean(all_data()$`2011-2040`, na.rm = TRUE), "\n")
    cat("Climate change (mean difference):",
        mean(all_data()$`2011-2040` - all_data()$current, na.rm = TRUE), "\n")
  })
  
  # Render summary title reactively, so that the heading is hidden until the query is finished
  output$summary_title <- renderUI({
    req(all_data())
    h4("Query Summary")
  })
  
  # Calculate climate change summary
  climate_change_summary <- reactive({
    req(all_data())
    data <- all_data()
    bio_info <- get_bioclim_info(input$chelsa_var)
    
    current_mean <- mean(data$current, na.rm = TRUE)
    future_2040_mean <- mean(data$`2011-2040`, na.rm = TRUE)
    future_2070_mean <- mean(data$`2041-2070`, na.rm = TRUE)
    future_2100_mean <- mean(data$`2071-2100`, na.rm = TRUE)
    
    change_2040 <- future_2040_mean - current_mean
    change_2070 <- future_2070_mean - current_mean
    change_2100 <- future_2100_mean - current_mean
    
    list(
      current_mean = current_mean,
      change_2040 = change_2040,
      change_2070 = change_2070,
      change_2100 = change_2100,
      bio_info = bio_info
    )
  })
  # Render climate summary
    output$climate_summary <- renderUI({
    summary <- climate_change_summary()
    bio_info <- summary$bio_info
    
    format_change <- function(change) {
      formatted <- if(abs(change) < 0.01) {
        sprintf("%.3f", change)
      } else {
        sprintf("%.2f", change)
      }
      paste0(ifelse(change >= 0, "+", ""), formatted, " ", bio_info$unit)
    }
    
    HTML(paste0(
      "<h4>Climate Change Summary</h4>",
      "<p><strong>Current mean ", bio_info$subtitle, ":</strong> ", 
      sprintf("%.2f", summary$current_mean), " ", bio_info$unit, "</p>",
      "<p><strong>Average change by 2011-2040:</strong> ", 
      format_change(summary$change_2040), "</p>",
      "<p><strong>Average change by 2041-2070:</strong> ", 
      format_change(summary$change_2070), "</p>",
      "<p><strong>Average change by 2071-2100:</strong> ", 
      format_change(summary$change_2100), "</p>"
    ))
  })
  output$query_summary <- renderUI({
    req(all_data())
    data <- all_data()
    
    filter_text <- if (length(input$cc_filters) > 0) {
      paste(input$cc_filters, collapse = ", ")
    } else {
      "None"
    }
    
    HTML(paste0(
      "<p><strong>Species:</strong> ", input$species_name, "</p>",
      "<p><strong>Climate scenario:</strong> ", toupper(input$ssp_var), "</p>",
      "<p><strong>Original records retrieved:</strong> ", data$original_count[1], "</p>",
      "<p><strong>Records after applying filters:</strong> ", data$final_count[1], "</p>",
      "<p><strong>Data quality filters applied:</strong> ", filter_text, "</p>"
    ))
  })
  # Render value boxes for the query summary
  output$records_box <- renderValueBox({
    req(all_data())
    valueBox(
      value = tags$p("Total records for analysis", style = "font-size: 65%;"),
      nrow(all_data()),
      icon = icon("table")
    )
  })
  
  output$quality_box <- renderValueBox({
    req(all_data())
    valueBox(
      value = tags$p("Records with valid climate data", style = "font-size: 65%;"),
      paste0(round(mean(!is.na(all_data()$current)) * 100, 1), "%"),
      icon = icon("check-circle")
    )
  })
  
  output$climate_box <- renderValueBox({
    req(all_data())
    data <- all_data()
    bio_info <- get_bioclim_info(input$chelsa_var)
    
    # Calculate mean change 
    current_mean <- mean(data$current, na.rm = TRUE)
    future_mean <- mean(data$`2071-2100`, na.rm = TRUE)
    mean_change <- future_mean - current_mean
    
    # Format the change value
    formatted_change <- if(abs(mean_change) < 0.01) {
      sprintf("%.3f", mean_change)
    } else {
      sprintf("%.2f", mean_change)
    }
    
    valueBox(
      value = tags$p(paste0("Mean change in ", bio_info$subtitle, " by the end of the century"), style = "font-size: 65%;"),
      paste0(formatted_change, " ", bio_info$unit),
      icon = icon("temperature-high")
    )
  })
  # Generate and render a table for Data Table tab
  output$data_table <- renderDT({
    req(all_data())
    datatable(
      all_data(),
      extensions = c('Buttons', 'ColReorder'),
      options = list(
        dom = 'Bfrtip',
        buttons = list(
          list(
            extend = 'colvis',
            className = 'custom-button-class'
          ),
          list(
            extend = 'collection',
            buttons = list(
              list(extend = 'csv', filename = paste0(input$species_name, "_data_", Sys.Date())),
              list(extend = 'excel', filename = paste0(input$species_name, "_data_", Sys.Date()))
            ),
            text = 'Download',
            className = 'custom-button-class'
          )
        ),
        colReorder = TRUE,  # Allows column reordering
        pageLength = 10,
        scrollX = TRUE,  # Enables horizontal scrolling
        autoWidth = TRUE
      ),
      class = 'cell-border stripe',
      rownames = FALSE
    ) |> 
      formatStyle(
        columns = names(all_data()),
        backgroundColor = 'white',
        color = 'grey10'
      )
  })
}

# Run the app
shinyApp(ui = ui, server = server)