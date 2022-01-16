# global knitr chunk options
knitr::opts_chunk$set(
  warning = FALSE, 
  message = FALSE)

# load packages
librarian::shelf(
  caret,       # m: modeling framework
  dplyr, ggplot2 ,here, readr, 
  pdp,         # X: partial dependence plots
  ranger,      # m: random forest modeling
  rpart,       # m: recursive partition modeling
  rpart.plot,  # m: recursive partition plotting
  rsample,     # d: split train/test data
  skimr,       # d: skim summarize data table
  vip)         # X: variable importance

# options
options(
  scipen = 999,
  readr.show_col_types = F)
set.seed(42)

# graphical theme
ggplot2::theme_set(ggplot2::theme_light())

# paths
dir_data    <- here("data/sdm")
pts_env_csv <- file.path(dir_data, "pts_env.csv")

# read data
pts_env <- read_csv(pts_env_csv)
d <- pts_env %>% 
  select(-ID) %>%                   # not used as a predictor x
  mutate(
    present = factor(present)) %>%  # categorical response
  na.omit()                         # drop rows with NA
skim(d)


# create training set with 80% of full data
d_split  <- rsample::initial_split(d, prop = 0.8, strata = "present")
d_train  <- rsample::training(d_split)

# show number of rows present is 0 vs 1
table(d$present)

table(d_train$present)


# run decision stump model
mdl <- rpart(
  present ~ ., data = d_train, 
  control = list(
    cp = 0, minbucket = 5, maxdepth = 1))
mdl


# plot tree 
par(mar = c(1, 1, 1, 1))
rpart.plot(mdl)


# decision tree with defaults
mdl <- rpart(present ~ ., data = d_train)
mdl


rpart.plot(mdl)

# plot complexity parameter
plotcp(mdl)

# rpart cross validation results
mdl$cptable


# caret cross validation results
mdl_caret <- train(
  present ~ .,
  data       = d_train,
  method     = "rpart",
  trControl  = trainControl(method = "cv", number = 10),
  tuneLength = 20)

ggplot(mdl_caret)


vip(mdl_caret, num_features = 40, bar = FALSE)


# Construct partial dependence plots
p1 <- partial(mdl_caret, pred.var = "lat") %>% autoplot()
p2 <- partial(mdl_caret, pred.var = "BO_damean") %>% autoplot()
p3 <- partial(mdl_caret, pred.var = c("lat", "BO_damean")) %>% 
  plotPartial(levelplot = FALSE, zlab = "yhat", drape = TRUE, 
              colorkey = TRUE, screen = list(z = -20, x = -60))

# Display plots side by side
gridExtra::grid.arrange(p1, p2, p3, ncol = 3)


# number of features
n_features <- length(setdiff(names(d_train), "present"))

# fit a default random forest model
mdl_rf <- ranger(present ~ ., data = d_train)

# get out of the box RMSE
(default_rmse <- sqrt(mdl_rf$prediction.error))


# re-run model with impurity-based variable importance
mdl_impurity <- ranger(
  present ~ ., data = d_train,
  importance = "impurity")

# re-run model with permutation-based variable importance
mdl_permutation <- ranger(
  present ~ ., data = d_train,
  importance = "permutation")
p1 <- vip::vip(mdl_impurity, bar = FALSE)
p2 <- vip::vip(mdl_permutation, bar = FALSE)

gridExtra::grid.arrange(p1, p2, nrow = 1)
