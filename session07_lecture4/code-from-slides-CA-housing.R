library(tidyverse)
library(tidymodels)
library(GGally)
library(sf)
library(leaflet)
library(janitor)
library(rpart.plot)
library(gt)
library(DataExplorer)

# for figures
train_color <- "#1a162d"
test_color <- "#84cae1"
data_color <- "#CA225E"
assess_color <- data_color
splits_pal <- c(data_color, train_color, test_color)

# California Housing 
# https://www.kaggle.com/datasets/camnugent/california-housing-prices


california_housing <- read_csv("data/housing.csv")

glimpse(california_housing)

california_housing <- california_housing %>% 
  mutate(rooms_per_household = total_rooms/households,
        bedrooms_per_room = total_bedrooms/total_rooms,
        population_per_household = population/households)


## Remove median house value
# make categorical variable `above`


housing_df <- california_housing %>% 
  mutate(price_category = case_when( 
    median_house_value < 150000 ~ "below",
    median_house_value >= 150000 ~ "above")) %>% 
  mutate(price_category = factor(price_category),
         ocean_proximity = factor(ocean_proximity)) %>% 
  select(-median_house_value)

# lets make the table look nicer
library(gt)
housing_df %>% 
  count(price_category, 
        name ="districts_total") %>%
  mutate(percent = districts_total/sum(districts_total)*100,
         percent = round(percent, 2)) %>%
 gt() %>%
  tab_header(
    title = "California median house prices",
    subtitle = "Districts above and below 150K $"
  ) %>%
  cols_label(
    price_category = "Price",
    districts_total = "Districts",
    percent = "Percent"
  ) %>% 
  fmt_number(
    columns = c(districts_total),
    suffixing = TRUE
  ) 


## Any missing values?
DataExplorer::plot_missing(housing_df)



## Scatterplot- Correlation Matrix
housing_df %>% 
  select(
    housing_median_age, 
    median_income, 
    bedrooms_per_room, 
    rooms_per_household, 
    population_per_household, 
    ocean_proximity,
    price_category) %>% 
  ggpairs()

## A couple of maps

# Read a shapefile with CA counties
california_sf <- read_sf(here::here("data/CA_Counties", "CA_Counties_TIGER2016.shp"))

# check CRS of shapefile
st_geometry(california_sf)

# turn long-lan into a geoetry/CRS using CRS=4326, the WGS system
housing_map <- st_as_sf(california_housing,
                        coords = c("longitude", "latitude"),
                        crs = 4326)
st_geometry(housing_map)

ggplot()+
  # draw the shapefile of CA and its counties
  geom_sf(data = california_sf, 
          fill = "#fafafa",
          size = 0.125,
          colour = "grey10") +
  theme_void()+
  # add our shapefile with data on prices
  geom_sf(
    data = housing_map, 
    aes(size = population,
        colour = median_house_value),
    alpha = 0.4  )+
  scale_colour_gradientn(labels = comma, 
                         colours=rev(rainbow(5)))+
  labs(size = "Population",
       colour = "Median House Value")

## Interactive map


# Create a continuous palette function
binpal <- colorBin("Set3", 
                   california_housing$median_house_value, 5, pretty = TRUE)


leaflet(data = california_housing) %>% 
  addProviderTiles(providers$CartoDB.Positron) %>% 
  addCircleMarkers(lng = ~longitude, 
                   lat = ~latitude, 
                   radius = 1, 
                   color = ~binpal(median_house_value), 
                   fillOpacity = 0.6,
                   label = ~median_house_value,
                   popup = ~median_house_value
                   )


## Feature Exploration of numeric variables

housing_df %>% 
  select(price_category, where(is.numeric), -longitude, - latitude) %>% 
  pivot_longer(cols = 2:10,
               names_to = "feature",
               values_to = "value") %>% 
  ggplot()+
  aes(x=price_category, y = value, fill = price_category)+
  coord_flip()+
  geom_boxplot()+
  facet_wrap(~feature, scales = "free")+
  theme_bw()+
  theme(legend.position = "none")+
  labs(x=NULL,y = NULL)

## Feature Exploration without outliers


housing_df %>% 
  select(price_category, where(is.numeric), -longitude, - latitude) %>% 
  filter(rooms_per_household < 50, 
         population_per_household < 20,
         population < 20000) %>% 
  
  pivot_longer(cols = 2:10,
               names_to = "feature",
               values_to = "value") %>% 
  ggplot()+
  aes(x=price_category, y = value, fill = price_category)+
  coord_flip()+
  geom_boxplot()+
  facet_wrap(~feature, scales = "free")+
  theme_bw()+
  theme(legend.position = "none")+
  labs(x=NULL,y = NULL)



## Scatterplot - Correlation Numerical Variables

housing_df %>% 
  select(price_category, 
         median_income, 
         bedrooms_per_room, 
         rooms_per_household, 
         population_per_household) %>% 
  ggscatmat(color="price_category", 
            corMethod = "spearman",
            alpha=0.2)+
  theme_bw()



## Categorical Variables

library(gt)

housing_df %>% 
  count(price_category, ocean_proximity) %>% 
  group_by(price_category) %>% 
  mutate(percent = n / sum(n) *100,
         percent = round(percent, 2)) %>% 
  gt() %>% 
    tab_header(
    title = "California median house prices",
    subtitle = "Districts above and below 150.000$"
  ) %>% 
  cols_label(
    ocean_proximity = "Ocean Proximity",
    n = "Districts",
    percent = "Percent"
  ) %>% 
  fmt_number(
    columns = vars(n),
    suffixing = TRUE
  ) 


housing_df %>%
  ggplot(aes(x = price_category, 
             y = ocean_proximity)) +
  geom_bin2d() +
  scale_fill_continuous(type = "viridis")+
  theme_light()+
  labs(x=NULL, y= NULL)


## Data Preparation

##  Feature Selection and Data Split

housing_df_new <- housing_df %>% 
  select( # select features
    longitude, latitude, # We keep long-lat but we will not use them in our model.
    price_category, median_income, 
    ocean_proximity, bedrooms_per_room, 
    rooms_per_household, population_per_household)

glimpse(housing_df_new)


# **Split the data**

set.seed(123)

data_split <- initial_split(housing_df_new, # updated data
                           prop = 0.8, 
                           strata = price_category)

train_data <- training(data_split) 
test_data <- testing(data_split)

## `recipe()`

housing_rec <-
  recipe(price_category ~ .,
         data = train_data) 

## A few more steps in our recipe

# - `step_novel()` converts all nominal variables to factors and takes care of other issues related to categorical variables.
# 
# - `step_log()` will log transform data (since some of our numerical variables are right-skewed). Note that this step can not be performed on negative numbers.
# 
# - `step_normalize()` normalizes (center and scales) the numeric variables to have a standard deviation of one and a mean of zero. (i.e., z-standardization).
# 
# - `step_dummy()` converts our factor column ocean_proximity into numeric binary (0 and 1) variables.
# 
# `step_zv()` removes any numeric variables that have zero variance.
# 
# `step_corr()` will remove predictor variables with high correlations with other predictor variables.


## Recipe we will use

housing_rec <-
  recipe(price_category ~ .,
         data = train_data) %>%
  update_role(longitude, latitude, 
              new_role = "ID") %>% 
  step_log(
    median_income,
    bedrooms_per_room, rooms_per_household, 
    population_per_household
    ) %>% 
  step_naomit(everything(), skip = TRUE) %>% 
  step_novel(all_nominal(), -all_outcomes()) %>%
  step_normalize(all_numeric(), -all_outcomes(), 
                 -longitude, -latitude) %>% 
  step_dummy(all_nominal(), -all_outcomes()) %>%
  step_zv(all_numeric(), -all_outcomes()) %>%
  step_corr(all_predictors(), threshold = 0.7, method = "spearman") 

summary(housing_rec)


## What does our processed data look like? (1/2)

prepped_data <- 
  housing_rec %>% # use the recipe object
  prep() %>% # perform the recipe on training data
  juice() # extract only the preprocessed dataframe 

glimpse(prepped_data)

prepped_data %>% 
  select(median_income, rooms_per_household, population_per_household, price_category) %>% 
  ggpairs()


## Validation Set

# Use k-fold cross validation to build a set of 10 validation folds

set.seed(123)

cv_folds <-
 vfold_cv(train_data, 
          v = 10, 
          strata = price_category) 


## Model Building (1/2)

# 1. Pick a `model type`
# 2. set the `engine`
# 3. Set the `mode`: regression or classification

# Logistic regression
log_spec <-  logistic_reg() %>%  # model type
  set_engine(engine = "glm") %>%  # model engine
  set_mode("classification") # model mode

# Show your model specification
log_spec

# Decision Tree
tree_spec <- decision_tree() %>%
  set_engine(engine = "C5.0") %>%
  set_mode("classification")

tree_spec

# Random Forest
library(ranger)

rf_spec <- 
  rand_forest() %>% 
  set_engine("ranger", importance = "impurity") %>% 
  set_mode("classification")


# Boosted tree (XGBoost)
library(xgboost)

xgb_spec <- 
  boost_tree() %>% 
  set_engine("xgboost") %>% 
  set_mode("classification") 

# K-nearest neighbour (k-NN)
knn_spec <- 
  nearest_neighbor(neighbors = 4) %>% # we can adjust the number of neighbors 
  set_engine("kknn") %>% 
  set_mode("classification") 


## Bundle recipe and model with `workflows`


log_wflow <- # new workflow object
 workflow() %>% # use workflow function
 add_recipe(housing_rec) %>%   # use the new recipe
 add_model(log_spec)   # add your model spec

# show object
log_wflow



## A few more workflows

tree_wflow <-
 workflow() %>%
 add_recipe(housing_rec) %>% 
 add_model(tree_spec) 

rf_wflow <-
 workflow() %>%
 add_recipe(housing_rec) %>% 
 add_model(rf_spec) 

xgb_wflow <-
 workflow() %>%
 add_recipe(housing_rec) %>% 
 add_model(xgb_spec)

knn_wflow <-
 workflow() %>%
 add_recipe(housing_rec) %>% 
 add_model(knn_spec)


## Evaluate Models

## Logistic regression results{.smaller}

log_res <- log_wflow %>% 
  fit_resamples(
    resamples = cv_folds, 
    metrics = metric_set(
      recall, precision, f_meas, accuracy,
      kap, roc_auc, sens, spec),
    control = control_resamples(save_pred = TRUE)) 

# Show average performance over all folds (note that we use log_res):
log_res %>%  collect_metrics(summarize = TRUE)

# Show performance for every single fold:
log_res %>%  collect_metrics(summarize = FALSE)



## `collect_predictions()` and get confusion matrix{.smaller}

log_pred <- log_res %>% collect_predictions()

log_pred %>%  conf_mat(price_category, .pred_class) 

log_pred %>% 
  conf_mat(price_category, .pred_class) %>% 
  autoplot(type = "mosaic") +
  geom_label(aes(
      x = (xmax + xmin) / 2, 
      y = (ymax + ymin) / 2, 
      label = c("TP", "FN", "FP", "TN")))


log_pred %>% 
  conf_mat(price_category, .pred_class) %>% 
  autoplot(type = "heatmap")


## ROC Curve

log_pred %>% 
  group_by(id) %>% # id contains our folds
  roc_curve(price_category, .pred_above) %>% 
  autoplot()


## Decision Tree results

tree_res <-
  tree_wflow %>% 
  fit_resamples(
    resamples = cv_folds, 
    metrics = metric_set(
      recall, precision, f_meas, 
      accuracy, kap,
      roc_auc, sens, spec),
    control = control_resamples(save_pred = TRUE)
    ) 

tree_res %>%  collect_metrics(summarize = TRUE)


## Random Forest

rf_res <-
  rf_wflow %>% 
  fit_resamples(
    resamples = cv_folds, 
    metrics = metric_set(
      recall, precision, f_meas, 
      accuracy, kap,
      roc_auc, sens, spec),
    control = control_resamples(save_pred = TRUE)
    ) 

rf_res %>%  collect_metrics(summarize = TRUE)

## Boosted tree - XGBoost

xgb_res <- 
  xgb_wflow %>% 
  fit_resamples(
    resamples = cv_folds, 
    metrics = metric_set(
      recall, precision, f_meas, 
      accuracy, kap,
      roc_auc, sens, spec),
    control = control_resamples(save_pred = TRUE)
    ) 

xgb_res %>% collect_metrics(summarize = TRUE)

## K-nearest neighbour

knn_res <- 
  knn_wflow %>% 
  fit_resamples(
    resamples = cv_folds, 
    metrics = metric_set(
      recall, precision, f_meas, 
      accuracy, kap,
      roc_auc, sens, spec),
    control = control_resamples(save_pred = TRUE)
    ) 

knn_res %>% collect_metrics(summarize = TRUE)


## Model Comparison

log_metrics <- 
  log_res %>% 
  collect_metrics(summarise = TRUE) %>%
  # add the name of the model to every row
  mutate(model = "Logistic Regression") 

tree_metrics <- 
  tree_res %>% 
  collect_metrics(summarise = TRUE) %>%
  mutate(model = "Decision Tree")

rf_metrics <- 
  rf_res %>% 
  collect_metrics(summarise = TRUE) %>%
  mutate(model = "Random Forest")

xgb_metrics <- 
  xgb_res %>% 
  collect_metrics(summarise = TRUE) %>%
  mutate(model = "XGBoost")

knn_metrics <- 
  knn_res %>% 
  collect_metrics(summarise = TRUE) %>%
  mutate(model = "Knn")

# create dataframe with all models
model_compare <- bind_rows(log_metrics,
                           tree_metrics,
                           rf_metrics,
                           xgb_metrics,
                           knn_metrics) 

#Pivot wider to create barplot
  model_comp <- model_compare %>% 
  select(model, .metric, mean, std_err) %>% 
  pivot_wider(names_from = .metric, values_from = c(mean, std_err)) 

# show mean are under the curve (ROC-AUC) for every model
model_comp %>% 
  arrange(mean_roc_auc) %>% 
  mutate(model = fct_reorder(model, mean_roc_auc)) %>% # order results
  ggplot(aes(model, mean_roc_auc, fill=model)) +
  geom_col() +
  coord_flip() +
  scale_fill_brewer(palette = "Blues") +
   geom_text(
     size = 3,
     aes(label = round(mean_roc_auc, 2), 
         y = mean_roc_auc + 0.08),
     vjust = 1
  )+
  theme_light()+
  theme(legend.position = "none")+
  labs(y = NULL)

## `last_fit()` on test set

# - `last_fit()`  fits a model to the whole training data and evaluates it on the test set. 
# - provide the workflow object of the best model as well as the data split object (not the training data). 
 
last_fit_xgb <- last_fit(xgb_wflow, 
                        split = data_split,
                        metrics = metric_set(
                          accuracy, f_meas, kap, precision,
                          recall, roc_auc, sens, spec))

last_fit_xgb %>% collect_metrics(summarize = TRUE)

#Compare to training
xgb_res %>% collect_metrics(summarize = TRUE)


## Variable importance using `{vip}` package

library(vip)

last_fit_xgb %>% 
  pluck(".workflow", 1) %>%   
  pull_workflow_fit() %>% 
  vip(num_features = 10) +
  theme_light()


## Final Confusion Matrix

last_fit_xgb %>%
  collect_predictions() %>% 
  conf_mat(price_category, .pred_class) %>% 
  autoplot(type = "heatmap")


## Final ROC curve
last_fit_xgb %>% 
  collect_predictions() %>% 
  roc_curve(price_category, .pred_above) %>% 
  autoplot()
