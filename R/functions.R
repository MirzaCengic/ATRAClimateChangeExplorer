# functions.R
# This script holds all required functions for the ATRA climate impact explorer app

# This function returns information about a specific CHELSA bioclimatic variable, including conversion factors
# See https://chelsa-climate.org/bioclim/ for more info
get_bioclim_info <- function(bio_var) {
  bioclim_info <- list(
    bio1 = list(label = "Mean Annual Air Temperature", subtitle = "BIO 1", unit = "°C", scale = 0.1, offset = -273.15, axis_scale = 0.1),
    bio2 = list(label = "Mean Diurnal Air Temperature Range", subtitle = "BIO 2", unit = "°C", scale = 0.1, offset = 0, axis_scale = 0.01),
    bio3 = list(label = "Isothermality", subtitle = "BIO 3", unit = "°C", scale = 0.1, offset = 0, axis_scale = 0.01),
    bio4 = list(label = "Temperature Seasonality", subtitle = "BIO 4", unit = "°C", scale = 0.001, offset = 0, axis_scale = 0.1),
    bio5 = list(label = "Max Temperature of Warmest Month", subtitle = "BIO 5", unit = "°C", scale = 0.1, offset = -273.15, axis_scale = 0.1),
    bio6 = list(label = "Min Temperature of Coldest Month", subtitle = "BIO 6", unit = "°C", scale = 0.1, offset = -273.15, axis_scale = 0.1),
    bio7 = list(label = "Annual Range of Air Temperature", subtitle = "BIO 7", unit = "°C", scale = 0.1, offset = 0, axis_scale = 0.1),
    bio8 = list(label = "Mean Temperature of Wettest Quarter", subtitle = "BIO 8", unit = "°C", scale = 0.1, offset = -273.15, axis_scale = 0.1),
    bio9 = list(label = "Mean Temperature of Driest Quarter", subtitle = "BIO 9", unit = "°C", scale = 0.1, offset = -273.15, axis_scale = 0.1),
    bio10 = list(label = "Mean Temperature of Warmest Quarter", subtitle = "BIO 10", unit = "°C", scale = 0.1, offset = -273.15, axis_scale = 0.1),
    bio11 = list(label = "Mean Temperature of Coldest Quarter", subtitle = "BIO 11", unit = "°C", scale = 0.1, offset = -273.15, axis_scale = 0.1),
    bio12 = list(label = "Annual Precipitation Amount", subtitle = "BIO 12", unit = "mm", scale = 0.1, offset = 0, axis_scale = 1),
    bio13 = list(label = "Precipitation of Wettest Month", subtitle = "BIO 13", unit = "mm", scale = 0.1, offset = 0, axis_scale = 1),
    bio14 = list(label = "Precipitation of Driest Month", subtitle = "BIO 14", unit = "mm", scale = 0.1, offset = 0, axis_scale = 1),
    bio15 = list(label = "Precipitation Seasonality", subtitle = "BIO 15", unit = "CV", scale = 0.1, offset = 0, axis_scale = 0.1),
    bio16 = list(label = "Precipitation of Wettest Quarter", subtitle = "BIO 16", unit = "mm", scale = 0.1, offset = 0, axis_scale = 1),
    bio17 = list(label = "Precipitation of Driest Quarter", subtitle = "BIO 17", unit = "mm", scale = 0.1, offset = 0, axis_scale = 1),
    bio18 = list(label = "Precipitation of Warmest Quarter", subtitle = "BIO 18", unit = "mm", scale = 0.1, offset = 0, axis_scale = 1),
    bio19 = list(label = "Precipitation of Coldest Quarter", subtitle = "BIO 19", unit = "mm", scale = 0.1, offset = 0, axis_scale = 1)
  )
  
  return(bioclim_info[[bio_var]])
}


# Define choices for bioclimatic variables to be used in the UI input
bioclim_choices <- c(
  "BIO 1 - Mean Annual Air Temperature (°C)" = "bio1",
  "BIO 2 - Mean Diurnal Air Temperature Range (°C)" = "bio2",
  "BIO 3 - Isothermality (°C)" = "bio3",
  "BIO 4 - Temperature Seasonality (°C)" = "bio4",
  "BIO 5 - Max Temperature of Warmest Month (°C)" = "bio5",
  "BIO 6 - Min Temperature of Coldest Month (°C)" = "bio6",
  "BIO 7 - Annual Range of Air Temperature (°C)" = "bio7",
  "BIO 8 - Mean Temperature of Wettest Quarter (°C)" = "bio8",
  "BIO 9 - Mean Temperature of Driest Quarter (°C)" = "bio9",
  "BIO 10 - Mean Temperature of Warmest Quarter (°C)" = "bio10",
  "BIO 11 - Mean Temperature of Coldest Quarter (°C)" = "bio11",
  "BIO 12 - Annual Precipitation Amount (mm)" = "bio12",
  "BIO 13 - Precipitation of Wettest Month (mm)" = "bio13",
  "BIO 14 - Precipitation of Driest Month (mm)" = "bio14",
  "BIO 15 - Precipitation Seasonality (CV)" = "bio15",
  "BIO 16 - Precipitation of Wettest Quarter (mm)" = "bio16",
  "BIO 17 - Precipitation of Driest Quarter (mm)" = "bio17",
  "BIO 18 - Precipitation of Warmest Quarter (mm)" = "bio18",
  "BIO 19 - Precipitation of Coldest Quarter (mm)" = "bio19"
)

# Define a data frame of species names and their corresponding class (Reptilia or Amphibia)
# This list contains only species that were observed in Bosnia & Herzegovina 
# Replace variable species data with your own list of species. 
# Binomial names in column name will be used to query GBIF. 

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


# Function to retrieve GBIF data for a given species
get_gbif_data <- function(species_name, limit) {
  tryCatch({
    # Get the GBIF taxon key for the species
    key <- name_suggest(q = species_name, rank = 'species')$data$key[1]
    if (is.null(key)) {
      stop("Species not found in GBIF database.")
    }
    
    # Retrieve occurrence data from GBIF
    data <- occ_search(taxonKey = key, limit = limit, country = "BA", hasCoordinate = TRUE, hasGeospatialIssue = FALSE)$data
    if (nrow(data) == 0) {
      stop("No occurrence data found for this species.")
    }
    data
  }, error = function(e) {
    stop(paste("Error retrieving GBIF data:", e$message))
  })
}


# Function to clean GBIF data using filters from R package CoordinateCleaner
clean_gbif_data <- function(data, filters) {
  # Get number of records without filters
  original_count <- nrow(data)
  
  # Remove rows with missing latitude, longitude, species, or country code
  cleaned_data <- data |> 
    filter(!is.na(decimalLatitude) & !is.na(decimalLongitude)) |> 
    filter(
      !is.na(species),
      !is.na(countryCode)
    )
  
  filter_counts <- list()
  
  # Apply selected filters
  for (filter in filters) {
    if (filter == "remove_zeros") {
      cleaned_data <- cleaned_data |> cc_zero(lon = "decimalLongitude", lat = "decimalLatitude")
      cleaned_data$flag_zero_passed <- cc_zero(cleaned_data, value = "flagged")
    } else if (filter == "remove_equal") {
      cleaned_data <- cleaned_data |> cc_equ(lon = "decimalLongitude", lat = "decimalLatitude")
      cleaned_data$flag_equal_passed <- cc_equ(cleaned_data, value = "flagged")
    } else if (filter == "remove_centroids") {
      cleaned_data <- cleaned_data |> cc_cen(lon = "decimalLongitude", lat = "decimalLatitude")
      cleaned_data$flag_centroids_passed <- cc_cen(cleaned_data, value = "flagged")
    } else if (filter == "remove_invalid") {
      cleaned_data <- cleaned_data |> cc_val(lon = "decimalLongitude", lat = "decimalLatitude")
      cleaned_data$flag_invalid_passed <- cc_val(cleaned_data, value = "flagged")
    } else if (filter == "remove_capitals") {
      cleaned_data <- cleaned_data |> cc_cap(lon = "decimalLongitude", lat = "decimalLatitude")
      cleaned_data$flag_capitals_passed <- cc_cap(cleaned_data, value = "flagged")
    } else if (filter == "remove_duplicates") {
      cleaned_data <- cleaned_data |> cc_dupl(lon = "decimalLongitude", lat = "decimalLatitude")
      cleaned_data$flag_duplicates_passed <- cc_dupl(cleaned_data, value = "flagged")
    } else if (filter == "remove_urban") {
      cleaned_data <- cleaned_data |> cc_urb(lon = "decimalLongitude", lat = "decimalLatitude")
      cleaned_data$flag_urban_passed <- cc_urb(cleaned_data, value = "flagged")
    } else if (filter == "remove_sea") {
      cleaned_data <- cleaned_data |> cc_sea(lon = "decimalLongitude", lat = "decimalLatitude")
      cleaned_data$flag_sea_passed <- cc_sea(cleaned_data, value = "flagged")
    }
  }
  
  return(list(
    data = cleaned_data,
    original_count = original_count,
    final_count = nrow(cleaned_data)
  ))
}




# Function to extract climate data from occurence points
extract_climate_data <- function(points, raster_path) {
  tryCatch({
    if (!file.exists(raster_path)) {
      stop("Climate data file not found.")
    }
    r <- rast(raster_path)
    terra::extract(r, points)
  }, error = function(e) {
    stop(paste("Error extracting climate data:", e$message))
  })
}

# Convert rasters to a ggplot-friendly format
raster_to_gg <- function(x)
{
  stopifnot(inherits(x, "Raster"))
  
  x <- as(x, "SpatialPixelsDataFrame")
  x <- as.data.frame(x) |> 
    rename(value = 1)
  return(x)
}

# Function to load climate data for a specific bioclimatic variable
load_basic_climate <- function(bio_var = 1)
{
  # Load current climate data
  current_data <- here("data", "chelsa", "current") |> 
    list.files(full.names = TRUE, recursive = TRUE, pattern = paste0("bio", bio_var, "_")) |> 
    rast()
  
  raster_to_gg(raster::raster(current_data))
}

# Prepare climate data ----------------------------------------------------

# Function to prepare climate trajectory data for a species
# This function does the majority of data retrieval work
prepare_climate_trajectory_data <- function(species_name, bio_var = 1, ssp = "ssp585",
                                            gcm = "ipsl-cm6a-lr", limit = 100, filters) {
  
  # Load current climate data
  current_data <- here("data", "chelsa", "current") |> 
    list.files(full.names = TRUE, recursive = TRUE, pattern = paste0("bio", bio_var, "_")) |> 
    rast()
  
  # Load future climate data
  future_data <- here("data", "chelsa", "future") |> 
    list.files(full.names = TRUE, recursive = TRUE, pattern = paste0("bio", bio_var, "_")) |> 
    str_subset(ssp) |> 
    str_subset(gcm) |> 
    rast()
  
  # Combine current and future data
  climate_data <- c(current_data, future_data)
  names(climate_data) <- c("curr", "fut1", "fut2", "fut3")
  
  # Get species occurrence data
  species_data <- get_gbif_data(species_name = species_name, limit = limit)
  
  # Clean data and get counts
  cleaned_data_info <- clean_gbif_data(species_data, filters = filters)
  species_data <- cleaned_data_info$data
  # Extract climate data for species occurrences
  species_climate_data <- terra::extract(climate_data, species_data[, c("decimalLongitude", "decimalLatitude")], xy = TRUE)
  
  bio_info <- get_bioclim_info(bio_var)
  
  # Combine all data and apply unit conversions
  combined_data_sf <-
    species_data |> 
    mutate(
      "current" = (species_climate_data$curr * bio_info$scale + bio_info$offset) |> round(4),
      "2011-2040" = (species_climate_data$fut1 * bio_info$scale + bio_info$offset) |> round(4),
      "2041-2070" = (species_climate_data$fut2 * bio_info$scale + bio_info$offset) |> round(4),
      "2071-2100" = (species_climate_data$fut3 * bio_info$scale + bio_info$offset) |> round(4)
    ) 
  # Add count information to the result
  combined_data_sf$original_count <- cleaned_data_info$original_count
  combined_data_sf$final_count <- cleaned_data_info$final_count
  
  return(combined_data_sf)
}

# Function to plot the left part of the climate comparison plot (map)
plot_climate_trajectory_left <- function(data, raster, country, bio_var) {
  
  # Get bioclim metadata
  bio_info <- get_bioclim_info(bio_var)
  
  # Apply scaling and offset to raster data
  raster <- raster |> 
    mutate(
      value = value * bio_info$scale + bio_info$offset
    )
  
  # Create the plot
  p1 <- data |>
    st_as_sf(coords = c(x = "decimalLongitude", y = "decimalLatitude"), crs = 4326) |>
    ggplot() +
    geom_raster(data = raster, aes(x = x, y = y, fill = value)) +
    geom_sf(data = country, fill = NA, color = "grey5") +
    geom_sf_interactive(aes(data_id = key), size = 1) +
    scale_fill_viridis_c(
      name = NULL,
      option = "C",
      labels = scales::number_format(
        accuracy = bio_info$axis_scale,
        suffix = bio_info$unit,
        scale = 1,
        offset = bio_info$offset
      )
    ) +
    coord_sf()+
    theme_map() +
    theme(
      legend.position = "left",
      legend.title = element_blank(),
      # Some of these parameters require manual tuning to get right
      legend.text = element_text(size = ifelse(bio_info$unit == "mm", 2.5, 3.5), hjust = 0.5),
      legend.margin = margin(t = 0, r = -25, b = 8, l = 0, unit = "pt"),
      plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")
    ) +
    labs(
      title = NULL,
      subtitle = NULL
    ) +
    guides(fill = guide_colorbar(
      barwidth = 0.25,
      barheight = 11.5,
      ticks = TRUE
    ))
  
  p1
}


# Function to plot the right part of the climate comparison plot (boxplot)
plot_climate_trajectory_right <- function(data, raster, country, bio_var) {
  
  # Bioclim metadata
  bio_info <- get_bioclim_info(bio_var)
  
  # Create the plot
  p2 <- data |>
    dplyr::select(key, all_of(c("current", "2011-2040", "2041-2070", "2071-2100"))) |>
    pivot_longer(-key) |>
    mutate(
      period = name |>
        fct_relevel(c("current", "2011-2040", "2041-2070", "2071-2100"))
    ) |>
    ggplot() +
    aes(x = period, y = value) +
    geom_boxplot(width = 0.5, outlier.shape = NA) +
    geom_point_interactive(aes(fill = period, data_id = key, 
                               tooltip = scales::number(
                                 value,
                                 accuracy = bio_info$axis_scale,
                                 suffix = bio_info$unit
                               )
    ),
    position = position_jitter(width = 0.2, seed = 123), shape = 21) +
    scale_y_continuous(
      labels = scales::number_format(
        accuracy = bio_info$axis_scale,
        suffix = bio_info$unit
      )
    ) +
    scale_fill_manual(values = c("#4E79A7", "#E15759", "#76B7B2", "#59A14F")) +
    theme_minimal() +
    labs(x = NULL,
         y = paste(bio_info$label, "(", bio_info$unit, ")"),
    ) +
    theme(axis.text.x = element_text(size = 6, color = "grey20"),
          axis.text.y = element_text(size = 5, color = "grey20"),
          plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
          axis.title.y = element_text(size = 5, color = "grey10"),
          legend.position = "none")
  
  p2
}

# Wrapper function to run the complete climate analysis
run_climate_analyis <- function(species_name, limit, bio_var, ssp = "", gcm = "")
{
  x <- prepare_climate_trajectory_data(
    species_name = species_name,
    bio_var = bio_var,
    ssp = ssp,
    gcm = gcm,
    limit = limit
  )
  
  plot_climate_trajectory(x)
  
}

