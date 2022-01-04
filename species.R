library(readr)
library(sf)
library(geojsonio)
# load packages, installing if missing
if (!require(librarian)){
  install.packages("librarian")
  library(librarian)
}
librarian::shelf(
  dismo, dplyr, DT, ggplot2, here, htmltools, leaflet, mapview, purrr, raster, readr, rgbif, rgdal, rJava, sdmpredictors, sf, spocc, tidyr)
select <- dplyr::select # overwrite raster::select

# set random seed for reproducibility
set.seed(42)

# directory to store data
dir_data <- here("data/sdm")
dir.create(dir_data, showWarnings = F)


obs_csv <- file.path(dir_data, "obs.csv")
obs_geo <- file.path(dir_data, "obs.geojson")

# get species occurrence data from GBIF with coordinates
(res <- spocc::occ(
  query = 'Neomonachus schauinslandi', 
  from = 'gbif', has_coords = T))

# extract data frame from result
df <- res$gbif$data[[1]] 
nrow(df) # number of rows

# convert to points of observation from lon/lat columns in data frame
obs <- df %>% 
  sf::st_as_sf(
    coords = c("longitude", "latitude"),
    crs = st_crs(4326))

readr::write_csv(df, obs_csv)
geojsonio::geojson_write(obs, file = "data/sdm/obs.geojson")

# show points on map
mapview::mapview(obs, map.types = "Stamen.Terrain")
