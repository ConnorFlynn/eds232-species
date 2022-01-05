library(readr)
library(sf)
library(geojsonio)
library(rJava)
# Neomonachus schauinslandi

# load packages, installing if missing
if (!require(librarian)){
  install.packages("librarian")
  library(librarian)
}
librarian::shelf(
  dismo, dplyr, DT, ggplot2, here, htmltools, leaflet, mapview, purrr, raster, readr, rgbif, rgdal, rJava, sdmpredictors, sf, spocc, tidyr)
select <- dplyr::select # overwrite raster::select
options(readr.show_col_types = FALSE)

# set random seed for reproducibility
set.seed(42)

# directory to store data
dir_data <- here("data/sdm")
dir.create(dir_data, showWarnings = F)


obs_csv <- file.path(dir_data, "obs.csv")
obs_geo <- file.path(dir_data, "obs.geojson")
redo    <- FALSE

if (!file.exists(obs_geo) | redo){
  # get species occurrence data from GBIF with coordinates
  (res <- spocc::occ(
    query = 'Neomonachus schauinslandi', 
    from = 'gbif', has_coords = T))
  
  # extract data frame from result
  df <- res$gbif$data[[1]] 
  readr::write_csv(df, obs_csv)
  
  # convert to points of observation from lon/lat columns in data frame
  obs <- df %>% 
    sf::st_as_sf(
      coords = c("longitude", "latitude"),
      crs = st_crs(4326)) %>% 
    select(prov, key) # save space (joinable from obs_csv)
  sf::write_sf(obs, obs_geo, delete_dsn=T)
}
obs <- sf::read_sf(obs_geo)
nrow(obs) # number of rows

mapview::mapview(obs, map.types = "Stamen.Terrain")
