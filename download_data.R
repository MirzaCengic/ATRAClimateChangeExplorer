# Download climate data for your custom implementation of the app

if(interactive())
{
  #### ! IMPORTANT !: 
  # To create app instance with custom geographic extent, you will need to replace current data.
  # Variable DELETE_DATA if set to TRUE will remove existing data and begin downloading data.
  # CHELSA climate data comes as a global extent, and each variable is downloaded first, then cropped to the polygon extent.
  # Note that this download requires 100+ GB of disk space (you can delete old files in /raw folder) and takes time to finish.
  # You can reduce the size of download by modifying variable 'gcms'. Use version only containing GCM 'gfdl-esm4'.
  # Only single circulation model for SSP scenarios is implemented currently in the app (gfdl-esm4), 
  # however running code as-is will download all available CHELSA bioclim GCMs as long term aim is to enable this feature in the app.
  # Global data is stored in here("data", "chelsa", "raw")
 
  
  # Increase if downloads time-out
  options(timeout=1000)
  
  DELETE_DATA = FALSE
  
  # Define country name to download GADM data with rnaturalearth
  # Use function raster::getData() to see country names
  # Replace variable ne_sf with your own simple feature with geographic extent if needed
  country = "Bosnia and Herzegovina"
  
  # raster::getData("ISO3")
  
  
  
  if(DELETE_DATA)
  {
    library(here)
    library(dplyr)
    library(purrr)
    library(stringr)
    library(rnaturalearth)
    library(rnaturalearthhires) # Requires manual installation via install.packages("rnaturalearthhires", repos = "https://ropensci.r-universe.dev", type = "source")
    library(sf)
    library(raster)
    
    # For additional safety, you will need to uncomment code below to delete all data
    # here("data") |> 
      # list.files(full.names = TRUE, recursive = TRUE) |> 
      # file.remove()
    
    
    # Create folders for data if needed
    here("data") |> 
      dir.create()
    
    here("data", "vector") |> 
      dir.create()
    
    here("data", "chelsa") |> 
      dir.create()
    
    here("data", "chelsa", "raw") |> 
      dir.create()
    
    here("data", "chelsa", "current") |> 
      dir.create()
    
    here("data", "chelsa", "future") |> 
      dir.create()
    
  ne_sf <- ne_countries(returnclass = "sf", country = country, scale = 10)
  
  # Save for later use
  ne_sf |>
    write_sf(here::here("data", "vector", "country_shapefile.gpkg"))
  
  # URLs for current climate
  bioclim_paths_current <- c("https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/1981-2010/bio/CHELSA_bio10_1981-2010_V.2.1.tif", 
                             "https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/1981-2010/bio/CHELSA_bio11_1981-2010_V.2.1.tif",
                             "https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/1981-2010/bio/CHELSA_bio12_1981-2010_V.2.1.tif",
                             "https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/1981-2010/bio/CHELSA_bio13_1981-2010_V.2.1.tif",
                             "https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/1981-2010/bio/CHELSA_bio9_1981-2010_V.2.1.tif", 
                             "https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/1981-2010/bio/CHELSA_bio8_1981-2010_V.2.1.tif", 
                             "https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/1981-2010/bio/CHELSA_bio7_1981-2010_V.2.1.tif",
                             "https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/1981-2010/bio/CHELSA_bio6_1981-2010_V.2.1.tif",
                             "https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/1981-2010/bio/CHELSA_bio5_1981-2010_V.2.1.tif",
                             "https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/1981-2010/bio/CHELSA_bio4_1981-2010_V.2.1.tif",
                             "https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/1981-2010/bio/CHELSA_bio3_1981-2010_V.2.1.tif",
                             "https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/1981-2010/bio/CHELSA_bio2_1981-2010_V.2.1.tif",
                             "https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/1981-2010/bio/CHELSA_bio1_1981-2010_V.2.1.tif", 
                             "https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/1981-2010/bio/CHELSA_bio19_1981-2010_V.2.1.tif",
                             "https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/1981-2010/bio/CHELSA_bio17_1981-2010_V.2.1.tif",
                             "https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/1981-2010/bio/CHELSA_bio18_1981-2010_V.2.1.tif",
                             "https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/1981-2010/bio/CHELSA_bio16_1981-2010_V.2.1.tif", 
                             "https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/1981-2010/bio/CHELSA_bio15_1981-2010_V.2.1.tif", 
                             "https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/1981-2010/bio/CHELSA_bio14_1981-2010_V.2.1.tif")
  
  #### Create a vector of filenames using all possible combinations of bioclim
  #### variables, circulation models, ssp scenarios, and time periods
  
  gcms = c("gfdl-esm4", "ipsl-cm6a-lr",
           "mpi-esm1-2-hr", "mri-esm2-0", "ukesm1-0-ll")
  # Uncomment below and comment-out above to reduce download size
  # gcms = "gfdl-esm4"
  scenarios = c("ssp126", "ssp370", "ssp585")
  
  years = c("2011-2040", "2041-2070", "2071-2100")
  
  layers = 1:19
  
  
  # Create all combinations
  combinations <- expand.grid(layer = layers, year = years, gcm = gcms, scenario = scenarios)
  
  # Create the vector of file names
  file_names <- with(combinations, paste0("CHELSA_bio", layer, "_", year, "_", gcm, "_", scenario, "_V.2.1.tif"))
  
  #### Create a dataframe with input and output names.
  
  # Current climate
  bioclim_paths_current_df <- bioclim_paths_current |>
    as_tibble() |>
    transmute(
      origin = value, dest =  here::here("data", "chelsa", "raw", basename(value)) |>
        str_remove("_1981-2010_V.2.1") |>
        str_replace("CHELSA_", "current_") |>
        str_replace("_bio", "_bio_")
      
    )
  
  # Future climate
  file_list <- file_names |>
    as_tibble() |> 
    mutate(
      basepath = "https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/YEAR/GCM/SCENARIO/bio/",
      period = word(value, sep = "_", 3, 3),
      gcm = toupper(word(value, sep = "_", 4, 4)),
      scenario = word(value, sep = "_", 5, 5),
      basepath = str_replace_all(basepath, "YEAR", period),
      basepath = str_replace_all(basepath, "GCM", gcm),
      basepath = str_replace_all(basepath, "SCENARIO", scenario),
      full_path = paste0(basepath, value),
      dest = paste0(here::here("data", "chelsa", "raw"), "/", value)
    )

  
  
  #### Run code

  # Loop over each file for current climate. Code will skip if cropped file already exists
  for (i in 1:nrow(bioclim_paths_current_df))
  {
    
    print(i)
    origin <- bioclim_paths_current_df[i, "origin"][[1]]
    destination <- bioclim_paths_current_df[i, "dest"][[1]]
    
    dest_cropped_file <- str_replace_all(destination, "raw", "current")
    
    if (!file.exists(dest_cropped_file))
    {
      if (!file.exists(destination))
      {
        # Use appropriate download method depending on your system
        download.file(origin, destination, method = "curl")
      }
      rr <- raster(destination)
      rr_crop <- crop(rr, ne_sf)
      writeRaster(rr_crop, dest_cropped_file)
    }
  }
  # End loop
  
  
  # Loop over each file for future climate. Code will skip if cropped file already exists
  for (i in 1:nrow(file_list))
  {
    
    print(i)
    origin <- file_list[i, "full_path"][[1]]
    destination <- file_list[i, "dest"][[1]]
    
    dest_cropped_file <- str_replace_all(destination, "raw", "future")
    
    if (!file.exists(dest_cropped_file))
    {
      if (!file.exists(destination))
      {
        # Use appropriate download method depending on your system
        download.file(origin, destination, method = "curl")
      }
      rr <- raster(destination)
      rr_crop <- crop(rr, ne_sf)
      writeRaster(rr_crop, dest_cropped_file)
    }
  }
  # End loop
  
  
  
  }
}

