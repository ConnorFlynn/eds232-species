librarian::shelf(
  DT, dplyr, dismo, GGally, here, readr, tidyr)
select <- dplyr::select # overwrite raster::select
options(readr.show_col_types = F)

dir_data    <- here("data/sdm")
pts_env_csv <- file.path(dir_data, "pts_env.csv")

pts_env <- read_csv(pts_env_csv)
nrow(pts_env)

datatable(pts_env, rownames = F)


GGally::ggpairs(
  select(pts_env, -ID),
  aes(color = factor(present)))


# setup model data
d <- pts_env %>% 
  select(-ID) %>%  # remove terms we don't want to model
  tidyr::drop_na() # drop the rows with NA values
nrow(d)


mdl <- lm(present ~ ., data = d)
summary(mdl)


y_predict <- predict(mdl, d, type="response")
y_true    <- d$present

range(y_predict)


range(y_true)


mdl <- glm(present ~ ., family = binomial(link="logit"), data = d)
summary(mdl)


y_predict <- predict(mdl, d, type="response")

range(y_predict)


termplot(mdl, partial.resid = TRUE, se = TRUE, main = F, ylim="free")


librarian::shelf(mgcv)

# fit a generalized additive model with smooth predictors
mdl <- mgcv::gam(
  formula = present ~ s(BO_chlorange) + s(BO_damean) + 
    s(BO_dissox) + s(BO_nitrate) + s(BO_parmean) + 
    s(BO_ph) + s(BO_ph) + s(BO_phosphate) + s(BO_salinity) + s(BO_silicate) +
    s(BO_sstmean) + s(lon) + s(lat) , 
  family = binomial, data = d)
summary(mdl)

plot(mdl, scale=0)


# load extra packages
librarian::shelf(
  maptools, sf)

mdl_maxent_rds <- file.path(dir_data, "mdl_maxent.rds")

# show version of maxent
if (!interactive())
  maxent()


# get environmental rasters
# NOTE: the first part of Lab 1. SDM - Explore got updated to write this clipped environmental raster stack
env_stack_grd <- file.path(dir_data, "env_stack.grd")
env_stack <- stack(env_stack_grd)
plot(env_stack, nc=2)


# get presence-only observation points (maxent extracts raster values for you)
obs_geo <- file.path(dir_data, "obs.geojson")
obs_sp <- read_sf(obs_geo) %>% 
  sf::as_Spatial() # maxent prefers sp::SpatialPoints over newer sf::sf class

# fit a maximum entropy model
if (!file.exists(mdl_maxent_rds)){
  mdl <- maxent(env_stack, obs_sp)
  readr::write_rds(mdl, mdl_maxent_rds)
}
mdl <- read_rds(mdl_maxent_rds)

# plot variable contributions per predictor
plot(mdl)

response(mdl)

y_predict <- predict(env_stack, mdl) #, ext=ext, progress='')

plot(y_predict, main='Maxent, raw prediction')
data(wrld_simpl, package="maptools")
plot(wrld_simpl, add=TRUE, border='dark grey')




