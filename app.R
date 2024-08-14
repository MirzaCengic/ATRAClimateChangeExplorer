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

# Assuming you have a CSV file with the list of species in same format as species_data from functions.R 
# species_data <- read.csv("species_data.csv", stringsAsFactors = FALSE)

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
        p(HTML("The ATRA Climate Explorer is an interactive tool designed to assess the potential impacts of climate change on amphibian and reptile species (herpetofauna) in Bosnia and Herzegovina. This app is developed and hosted by the Herpetological Association of Bosnia and Herzegovina ", 
               "<a href='https://www.bhhuatra.com/' target='_blank'>(BHHU - ATRA)</a>", ", the leading national association for herpetological research and conservation.")),
        
        h3("Purpose and Significance"),
        p("Many species lack comprehensive climate change impact assessments in their IUCN evaluations, leading to incomplete information for conservation prioritization. This app serves as an exploratory tool to:"),
        tags$ol(
          tags$li("Query GBIF data and explore climate change impacts"),
          tags$li("Provide information to underpin research, conservation efforts, monitoring activities, policy, and decisions"),
          tags$li("Provide a reproducible framework for similar analyses in other regions")
        ),
        
        h3("How to Use the App"),
        tags$ol(
          tags$li("Species Selection: Choose a species from the dropdown menu. The list includes amphibians and reptiles observed in Bosnia and Herzegovina."),
          tags$li("Number of Records: Specify the number of occurrence records to retrieve from GBIF (maximum 10,000)."),
          tags$li(HTML("Data Quality Filters: Select filters to apply to the GBIF data for improved accuracy. These filters use the", 
                       "<a href='https://docs.ropensci.org/CoordinateCleaner/' target='_blank'>{CoordinateCleaner}</a>", " R package:")),
          tags$ul(
            tags$li("Remove zero coordinates: Excludes records with coordinates (0,0)"),
            tags$li("Remove equal records: Removes duplicate records"),
            tags$li("Remove centroid records: Excludes records that are country or province centroids"),
            tags$li("Remove invalid records: Removes records with invalid coordinates"),
            tags$li("Remove records in capital cities: Excludes records from capital city centers"),
            tags$li("Remove duplicates: Removes duplicate records based on coordinates"),
            tags$li("Remove records in urban areas: Excludes records from urban areas"),
            tags$li("Remove records in sea: Excludes marine records for terrestrial species")
          ),
          tags$li("Bioclimatic Variable: Choose a CHELSA bioclimatic variable for analysis."),
          tags$li("Climate Scenario: Select an SSP (Shared Socioeconomic Pathway) scenario."),
          tags$li("Run Analysis: Click 'Retrieve Data and Analyze' to process the data. Remember to run the query after each change in settings!"),
          tags$li("Explore Results: Use the tabs to view the map, climate comparisons, and data table.")
        ),
        
        h3("Data Sources"),
        h4("1. GBIF (Global Biodiversity Information Facility)"),
        tags$ul(
          tags$li(HTML("This app uses the ", 
                       "<a href='https://techdocs.gbif.org/en/openapi/' target='_blank'>GBIF API</a>", " via the ", 
                       "<a href='https://docs.ropensci.org/rgbif' target='_blank'>{rgbif}</a>", " R package to retrieve occurrence data.")),
          tags$li("In this app, we use the occ_search() function for real-time queries. However, it is important to note that creators of rgbif package recommend to use occ_download() function for any serious research. Therefore, we highlight that this app has an exploratory aim and should not be used for serious research."),
          tags$li(HTML("For further use of GBIF data, we recommend users to consult ", 
                       "<a href='https://www.gbif.org/citation-guidelines' target='_blank'>GBIF citation guidelines</a>", " for more information."))
        ),
        
        h4("2. CHELSA (Climatologies at High Resolution for the Earth's Land Surface Areas)"),
        p(HTML("We use ", 
               "<a href='https://chelsa-climate.org/bioclim/' target='_blank'>CHELSA bioclimate data</a>", 
               " for current and future climate projections.")),
        h5("Bioclimatic variables:"),
        tableOutput("bioclim_table"),
        
        h5("SSP Scenarios:"),
        tags$ul(
          tags$li("SSP126: Low emissions scenario. This scenario with 2.6 W/m² by the year 2100 is a remake of the optimistic scenario RCP2.6 and was designed with the aim of simulating a development that is compatible with the 2°C target. This scenario assumes climate protection measures being taken."),
          tags$li("SSP370: Intermediate emissions scenario. With 7 W/m² by the year 2100, this scenario is in the upper-middle part of the full range of scenarios. It was newly introduced after the RCP scenarios, closing the gap between RCP6.0 and RCP8.5."),
          tags$li("SSP585: High emissions scenario. With an additional radiative forcing of 8.5 W/m² by the year 2100, this scenario represents the upper boundary of the range of scenarios described in the literature. It can be understood as an update of the CMIP5 scenario RCP8.5, now combined with socioeconomic reasons."),
        ),
        
        h5("GCM (General Circulation Model):"),
        tags$ul(
          tags$li("Currently, we use the GFDL-ESM4 model."),
          tags$li("Future updates aim to allow selection from multiple GCMs, including: 'gfdl-esm4', 'ipsl-cm6a-lr', 'mpi-esm1-2-hr', 'mri-esm2-0', 'ukesm1-0-ll'."),
          tags$li(HTML("See", "<a href='https://chelsa-climate.org/wp-admin/download-page/CHELSA_tech_specification.pdf' target='_blank'>CHELSA techical specification</a>", " for more info"))
        ),
        
        h3("Reproducibility and Transferability"),
        p("This app is designed to be easily adaptable for different regions and sets of species. Users can clone the GitHub repository, modify the configuration and data files, and deploy their own version of the app. For more information on reproducing the app, please refer to the 'How to build your own?' panel."),
        
        h3("About BHHU - ATRA"),
        p("The Herpetological Association of BiH (BHHU - ATRA) curates a species distribution database with over 11,500 records. These records are being prepared for publication through GBIF, adhering to GBIF's data standards and protocols. This valuable dataset is expected to be available via GBIF within the next year, significantly enhancing the available data for herpetofauna in Bosnia and Herzegovina."),
        
        h3("Global Relevance"),
        p("While focused on Bosnia and Herzegovina, this tool has global significance. By providing open-source code and a reproducible framework, it enables researchers worldwide to create similar applications for their regions of interest. This approach supports GBIF's mission of making biodiversity data freely available for scientific research, conservation, and sustainable development.")
      )
    )
  ),
  # How to build your own app panel
  nav_panel(
    title = "How to Build Your Own?",
    fluidRow(
      column(
        width = 12,
        h2("How to Build Your Own Climate Change Explorer?"),
        p("This app is designed to be easily reproducible and adaptable. Whether you want to change the species list, geographic focus, or even the types of data and analyses, you can customize the ATRA Climate Explorer to suit your needs. We have enabled users to change the country and the list of species with minimal changes required. Here's how to get started:"),
        
        h3("1. Clone the GitHub Repository"),
        p(HTML("First, clone the ATRA Climate Explorer repository from GitHub: ",
               "<a href='https://github.com/MirzaCengic/ATRAClimateChangeExplorer' target='_blank'>https://github.com/MirzaCengic/ATRAClimateChangeExplorer</a>")),
        p(HTML("If you're new to Git, check out ", 
               "<a href='https://docs.github.com/en/repositories/creating-and-managing-repositories/cloning-a-repository' target='_blank'>GitHub's guide</a>", " on cloning repositories. Make sure your system has necessary", 
               "<a href='https://git-scm.com/book/en/v2/Getting-Started-Installing-Git' target='_blank'> git software installed.</a>")),
        
        h3("2. Set Up R and RStudio"),
        p("To run and modify the app, you'll need R and RStudio:"),
        tags$ul(
          tags$li(HTML("Download and install R from ", 
                       "<a href='https://cran.r-project.org/' target='_blank'>https://cran.r-project.org/</a>")),
          tags$li(HTML("Download and install RStudio from ", 
                       "<a href='https://www.rstudio.com/products/rstudio/download/' target='_blank'>https://www.rstudio.com/products/rstudio/download/</a>")),
          tags$li("Open the .Rproj file directly or via RStudio")
        ),
        
        h3("3. Restore Packages with renv"),
        p("We use the renv package to manage dependencies. Once you've opened the project in RStudio:"),
        tags$ul(
          tags$li("Install renv if you haven't already: ", code("install.packages('renv')")),
          tags$li("Restore the project's packages by running: ", code("renv::restore()"))
        ),
        p(HTML("This ensures you have all the necessary packages at the correct versions. If you run into issues with package rnaturalearthhires, make sure to ", 
               "<a href='https://github.com/ropensci/rnaturalearthhires' target='_blank'>install it manually</a>.")),
        
        h3("4. Customize the App"),
        p("Modify the app to fit your needs:"),
        
        h5("a) Change the species list:"),
        p("Edit the species_data variable in the functions.R file."),
        tags$ul(
          tags$li("The current app format uses 'name' for species names and 'class' for taxonomic class."),
          tags$li("You can modify this structure as needed for your use case, but ensure that the variable species_list is updated accordingly in the app context.")
        ),
        
        h5("b) Change the geographic focus:"),
        p("In the download_data.R script, you can modify the geographic focus in two ways:"),
        tags$ul(
          tags$li("Change the 'country' variable to your area of interest. Make sure the country name is correct and matches the naming convention in the rnaturalearth package. You can use ", code("getData('ISO3')"), " to see available country names."),
          tags$li("Alternatively, replace the 'ne_sf' variable with your own simple feature defining the geographic extent.")
        ),
        p("Changing the geographic focus will automatically crop the climate data for your region when you run the download_data.R script. Note that this download requires disk space and time, but see the script for more info."),
        
        h3("5. Run and Deploy the App"),
        h5("a) Run locally:"),
        p("Open app.R in RStudio and click 'Run App' to test it locally."),
        
        h5("b) Deploy online:"),
        tags$ul(
          tags$li(HTML("Create a free account on ", 
                       "<a href='https://www.shinyapps.io/' target='_blank'>shinyapps.io</a>")),
          tags$li(HTML("Use RStudio's built-in ", 
                       "<a href='https://shiny.posit.co/r/articles/share/shinyapps/' target='_blank'>publishing capabilities</a>", " to deploy your app online"))
        ),
        p("Note: The free tier on shinyapps.io includes 1GB of data use, which includes data hosted in the /data folder."),
        
        h3("Why is this free and open?"),
        p("By making this tool open source, we aim to foster collaboration and innovation in biodiversity research. Your modifications and improvements can contribute to better understanding and conservation of species worldwide. Whether you're adapting the app for a different region, adding new species, or incorporating novel analyses, your work builds upon a foundation of open science and shared knowledge. We encourage you to share your modifications, suggest improvements, and collaborate with others in the community."),
        p(HTML("This project is released under a permissive ", 
               "<a href='https://opensource.org/license/mit' target='_blank'>MIT license</a>."))
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
  
  # Render table for About panel
  output$bioclim_table <- renderTable({
    data.frame(
      Shortname = c("bio1", "bio2", "bio3", "bio4", "bio5", "bio6", "bio7", "bio8", "bio9", "bio10", "bio11", "bio12", "bio13", "bio14", "bio15", "bio16", "bio17", "bio18", "bio19"),
      Longname = c("Mean annual air temperature", "Mean diurnal air temperature range", "Isothermality", "Temperature seasonality", "Mean daily maximum air temperature of the warmest month", "Mean daily minimum air temperature of the coldest month", "Annual range of air temperature", "Mean daily mean air temperatures of the wettest quarter", "Mean daily mean air temperatures of the driest quarter", "Mean daily mean air temperatures of the warmest quarter", "Mean daily mean air temperatures of the coldest quarter", "Annual precipitation amount", "Precipitation amount of the wettest month", "Precipitation amount of the driest month", "Precipitation seasonality", "Mean monthly precipitation amount of the wettest quarter", "Mean monthly precipitation amount of the driest quarter", "Mean monthly precipitation amount of the warmest quarter", "Mean monthly precipitation amount of the coldest quarter"),
      Unit = c(rep("°C", 11), rep("kg m-2", 8)),
      Explanation = c("Mean annual daily mean air temperatures averaged over 1 year", "Mean diurnal range of temperatures averaged over 1 year", "Ratio of diurnal variation to annual variation in temperatures", "Standard deviation of the monthly mean temperatures", "The highest temperature of any monthly daily mean maximum temperature", "The lowest temperature of any monthly daily mean maximum temperature", "The difference between the Maximum Temperature of Warmest month and the Minimum Temperature of Coldest month", "The wettest quarter of the year is determined (to the nearest month)", "The driest quarter of the year is determined (to the nearest month)", "The warmest quarter of the year is determined (to the nearest month)", "The coldest quarter of the year is determined (to the nearest month)", "Accumulated precipitation amount over 1 year", "The precipitation of the wettest month", "The precipitation of the driest month", "The Coefficient of Variation is the standard deviation of the monthly precipitation estimates expressed as a percentage of the mean of those estimates (i.e. the annual mean)", "The wettest quarter of the year is determined (to the nearest month)", "The driest quarter of the year is determined (to the nearest month)", "The warmest quarter of the year is determined (to the nearest month)", "The coldest quarter of the year is determined (to the nearest month)")
    )
  }, striped = TRUE, bordered = TRUE, hover = TRUE)
  
  
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