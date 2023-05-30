library(tidyverse)
library(tidymodels)
library(GGally)
library(sf)
library(leaflet)
library(janitor)
library(rpart.plot)

# for figures
train_color <- "#1a162d"
test_color <- "#84cae1"
data_color <- "#CA225E"
assess_color <- data_color
splits_pal <- c(data_color, train_color, test_color)


ncbirths <- readr::read_csv(
  here::here("data", "ncbirths.csv")) %>% 
  mutate(low = factor(low),
         smoke = factor(smoke))

glimpse(ncbirths)


## `initial_split()`
# "Splits" data randomly into a single testing and a single training set.


set.seed(123)

data_parts <- ncbirths %>%
  initial_split(prop = 0.80, 
                strata = low) # divides the data

data_parts

train <- data_parts %>%
  training()  # recovers training data

test <- data_parts %>%
  testing()  # recovers testing data

nrow(train)
nrow(test)

# How frequently is a baby <2500grams?


ncbirths %>%
  janitor::tabyl(low) %>%
  janitor::adorn_pct_formatting()

# or using dplyr
ncbirths %>% 
  count(low, sort=TRUE) %>% 
  mutate(percent = n/sum(n))


ggplot(data = ncbirths, aes(
  x = weeks,
  y = birth_weight_gm,
  color = factor(low)
)) +
  geom_point(alpha = .3, size = 3) +
  geom_hline(yintercept = 2500, colour = "orange")+
  scale_color_manual(values = c("#001e62", "#CA225E")) +
  labs(
    x = "Weeks of gestation",
    y = "Birth Weight",
    colour = "< 2500g?"
  ) +
  theme_bw()+
  theme(legend.position = "right")


## EDA for ML {transition="slide-in"}

ncbirths %>% 
  drop_na(smoke) %>% 
  select(sex, smoke, mom_age, weeks, birth_weight_gm ) %>% 
  ggpairs(aes(fill=smoke, alpha = 0.33)) +
  theme_light(base_size = 7)

# Parsnip
# 1. Pick a **model**
# 2. Set the **engine**
# 3. Set the **mode** (if needed)

linear_reg() %>%
  set_engine("lm") %>%
  set_mode("regression")

logistic_reg() %>%
  set_engine("glm") %>%
  set_mode("classification")

decision_tree() %>%
  set_engine("C5.0") %>%
  set_mode("classification")

nearest_neighbor() %>%
  set_engine("kknn") %>%
  set_mode("classification")


lm_spec <- linear_reg() %>%
  set_mode("regression") %>%
  set_engine("lm")

lm_fit <- lm_spec %>%
    fit(birth_weight_gm ~ weeks, data = train)

lm_fit

linear_preds <- 
  lm_fit %>% 
  augment(new_data = train)

ggplot(data = NULL, aes(weeks, birth_weight_gm)) +
  geom_segment(data = linear_preds,
               aes(x = weeks, xend = weeks, 
                   y = birth_weight_gm, yend = .pred), 
               colour = train_color, alpha = 0.8) +
  geom_smooth(data = train, method = "lm", 
              se = FALSE, fullrange = TRUE,
              alpha = 0.8, size = 2, color = data_color) +
  geom_point(data = linear_preds, color = test_color, size = 3) +
  theme_bw(base_size = 18)


## Decision trees

age_rng <- range(ncbirths$weeks, na.rm=TRUE)
age_grid <- tibble(age = seq(age_rng[1], age_rng[2], length.out = 24))

tree_fit <-
  decision_tree(engine = "rpart",
                cost_complexity = 0.005, 
                mode = "regression") %>%
  fit(birth_weight_gm ~ weeks, data = train)

tree_preds <- 
  tree_fit %>%
  augment(new_data = train)


library(rpart.plot)
tree_fit %>%
  extract_fit_engine() %>%
  rpart.plot(roundint = FALSE)
```

:::
::::

## Decision trees

::: columns
::: {.column width="50%"}
```{r}
#| echo: false
#| fig-align: center
library(rpart.plot)
tree_fit %>%
  extract_fit_engine() %>%
  rpart.plot(roundint = FALSE)
```
:::

::: {.column width="50%"}
-   Series of splits or if/then statements based on predictors

-   First the tree *grows* until some condition is met (maximum depth, no more data)

-   Then the tree is *pruned* to reduce its complexity
:::
:::

## Decision trees

::: columns
::: {.column width="50%"}
```{r}
#| echo: false
#| fig-align: center
library(rpart.plot)
tree_fit %>%
  extract_fit_engine() %>%
  rpart.plot(roundint = FALSE)


ggplot(data = tree_preds, aes(x=weeks, y=birth_weight_gm)) +
  geom_segment(aes(x = weeks, xend = weeks, 
                   y = birth_weight_gm, yend = .pred), 
               colour = train_color, alpha = 0.8) +
  geom_line(aes(x = weeks, y = .pred), size = 2, alpha = 0.8, color = data_color) +
  geom_point(data = tree_preds, color = test_color, size = 3) +
  theme_bw(base_size = 18)


# A model workflow

tree_spec <-
  decision_tree() %>% 
  set_mode("regression")

tree_wflow <- workflow(
  birth_weight_gm ~ smoke+ weeks + sex+ marital + mom_age, tree_spec) %>% 
  fit(data = train) 

## Use `augment()` to get predictions

augment(tree_wflow, new_data = test) %>%  # using the 20% testing set
  select(sex, weeks, smoke, birth_weight_gm, .pred) # display fewer variables

tree_wflow %>%
  extract_fit_engine() %>%
  rpart.plot(roundint = FALSE) #  used to quiet a warning
 

# Metrics for model performance 


augment(tree_wflow, new_data = test) %>%
  metrics(birth_weight_gm, .pred)

reg_metrics <- metric_set(rmse, rsq, mae)

augment(lm_fit, new_data = test) %>% 
  reg_metrics(truth = birth_weight_gm,  # actual, observed value
              estimate = .pred)         # model predictions


augment(tree_wflow, new_data = test) %>% 
  reg_metrics(truth = birth_weight_gm,  # actual, observed value
              estimate = .pred)         # model predictions

## Cross-validation
set.seed(123) # Set seed when creating resamples for reproducibility

folds <- vfold_cv(train, v = 10, strata = birth_weight_gm)

folds
folds$splits[1:2]

## `fit_resamples()` fit model to resamples


tree_res <- fit_resamples(tree_wflow, folds)
tree_res


## Evaluating model performance


tree_res %>%
  collect_metrics()


## Comparing metrics  
# **Average over 10 CV folds**

tree_res %>%
  collect_metrics() %>% 
  select(.metric, mean, n)

#  **RMSE for training-  testing set**
training_rmse <- tree_fit %>%
  augment(train) %>%
  rmse(birth_weight_gm, .pred) %>%
  pull(.estimate) %>%
  round(digits = 2)

testing_rmse <- tree_fit %>% 
  augment(test) %>%
  rmse(birth_weight_gm, .pred) %>%
  pull(.estimate) %>%
  round(digits = 2)

## `control_resamples()` and `fit_resamples()` allow us to save predictions{.smaller}


# Save the assessment set results
ctrl <- control_resamples(save_pred = TRUE)
tree_res <- fit_resamples(tree_wflow, folds, control = ctrl)

tree_res # Where are the fitted models? 

tree_preds <- collect_predictions(tree_res)
tree_preds



# Classification 


ncbirths_classify <- ncbirths %>% 
  select(-birth_weight_gm)


set.seed(42)

data_parts <- ncbirths_classify %>%
  initial_split(prop = 0.80, 
                strata = low) # divides the data

data_parts

train_classify <- data_parts %>%
  training()  # recovers training data

test_classify <- data_parts %>%
  testing()  # recovers testing data


## The null model


train_classify %>% 
  count(low) %>% 
  mutate(percent = n/sum(n))



## Two class data{.smaller}

glm_model <- logistic_reg() %>%
  set_engine(engine = "glm") %>%
  set_mode("classification")

glm_model

tree_model <- decision_tree() %>%
  set_engine(engine = "C5.0") %>%
  set_mode("classification")

tree_model

## `fit()` to **train** models
glm_model %>% # parsnip model
  parsnip::fit(low ~ . , # a formula
    data = train_classify # dataframe
  ) 

tree_model %>% # parsnip model
  parsnip::fit(low ~ . , # a formula
    data = train_classify # dataframe
  )


## A fitted logistic regression model


glm_model %>% # parsnip model
  parsnip::fit(low ~ . , # a formula
    data = train_classify # dataframe
  ) %>% 
  broom::tidy()


## *All models are wrong, but some are useful*

glm_preds <- glm_model %>% # create tibble with predictions
  parsnip::fit(low ~ . , # a formula
      data = train_classify) %>%
  augment(new_data = train_classify) %>%
  mutate(.pred_match = if_else(low == .pred_class, 1, 0))

glm_preds %>% 
  select(weeks, smoke, low, .pred_class, .pred_match, .pred_0, .pred_1) %>% 
  head(12) 



## Confusion matrix

confusion_matrix <- glm_preds %>% 
  yardstick::conf_mat(truth = low, estimate = .pred_class)

confusion_matrix

autoplot(confusion_matrix) +
  geom_label(
    aes(
      x = (xmax + xmin) / 2, 
      y = (ymax + ymin) / 2, 
      label = c("TN", "FP", "FN", "TP")
    )
  )

yardstick::accuracy(glm_preds, low, .pred_class)


#refit `low` on `weeks` only
glm_preds0 <- glm_model %>% # create tibble with predictions
  parsnip::fit(low ~ weeks , # a formula
      data = train) %>%
  augment(new_data = train) %>%
  mutate(.pred_match = if_else(low == .pred_class, 1, 0))


ggplot(data = glm_preds0, aes(
  x = weeks,
  y = birth_weight_gm,
  colour = factor(low),
  shape = as.factor(.pred_match),
  alpha = as.factor(.pred_match)
)) +
  geom_point(alpha = .3, size = 3) +
  geom_hline(yintercept = 2500, colour = "orange")+
  scale_alpha_manual(values = c(1, .2), guide = "none") +
  scale_shape_manual(values = c(4, 19), guide = "none") +
  scale_color_manual(values = c("#001e62", "#CA225E")) +
  labs(
    title = "Logistic regression to predict low birth weight",
    x = "Weeks of gestation",
    y = "Birth Weight",
    colour = "< 2500g?"
  ) +
  theme_bw()+
  theme(legend.position = "right")


## ROC (receiver operating characteristic) curve

## Classification decision tree

# What happens if we fit a model to predict `low` birth weight and leave `birth_weight_gm` as a feature?

tree_mod <- 
    rpart::rpart(
        low ~ .,
        data = train, # uses data that contains both birth weight and `low`
        control = rpart::rpart.control(maxdepth = 5, cp = 0, minsplit = 10)
    ) %>% 
    partykit::as.party()
plot(tree_mod)



## Relationship between outcome and feature 

tree_mod <- 
    rpart::rpart(
        low ~ .,
        data = train_classify,
        control = rpart::rpart.control(maxdepth = 5, cp = 0, minsplit = 10)
    ) %>% 
    partykit::as.party()
plot(tree_mod)


# California Housing 

Median house prices for California districts derived from the 1990 census.

::: footer
https://www.kaggle.com/datasets/camnugent/california-housing-prices
:::



```{r, echo=FALSE, warning=FALSE, message=FALSE}
library(tidyverse)
library(sf)
library(janitor)
library(tidymodels)
library(skimr)
library(GGally)
library(leaflet)

california_housing <- read_csv("data/housing.csv")
```

Can we predict whether the median house value in a district is above/below 150K?

## Data Manipulation

```{r}
glimpse(california_housing)
```

- `total_rooms` in  district is not very useful. Use number of rooms per household.
- `total_bedrooms` equally not useful
- `population` in district is ok, but waht about `population_per_household`
 

```{r}
california_housing <- california_housing %>% 
  mutate(rooms_per_household = total_rooms/households,
        bedrooms_per_room = total_bedrooms/total_rooms,
        population_per_household = population/households)
```

## Remove median house value

```{r}
housing_df <- california_housing %>% 
  mutate(price_category = case_when( 
    median_house_value < 150000 ~ "below",
    median_house_value >= 150000 ~ "above")) %>% 
  mutate(price_category = factor(price_category),
         ocean_proximity = factor(ocean_proximity)) %>% 
  select(-median_house_value)
```

```{r, echo=FALSE}
# housing_df %>% 
#   count(price_category) %>% 
#   mutate(percent = n/sum(n))

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
```


## Any missing values?
```{r}
DataExplorer::plot_missing(housing_df)
```


## Scatterplot- Correlation Matrix

::: columns
::: {.column width="31%"}

```{r, ggpairs, warning= FALSE, eval=FALSE}

library(GGally)

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
```
:::

:::{.column width="69%"}

```{r, ggpairs2, warning= FALSE, echo=FALSE}
#| fig-width: 11
#| fig-height: 6 
#| fig-align: 'center'

library(GGally)

housing_df %>% 
  select(
    housing_median_age, 
    median_income, bedrooms_per_room, rooms_per_household, 
    population_per_household, ocean_proximity,
    price_category) %>% 
  ggpairs()
```
:::
::::

## A couple of maps{.smaller}

```{r, echo= FALSE, message=FALSE, warning=FALSE}
#| fig-width: 11
#| fig-height: 6 
#| fig-align: 'center'

# Read a shapefile with CA counties
california_sf <- read_sf(here::here("data/CA_Counties", "CA_Counties_TIGER2016.shp"))

# check CRS of shapefile
# st_geometry(california_sf)

# turn long-lan into a gemotry/CRS using CRS=4326, the WGS system
housing_map <- st_as_sf(california_housing,
                        coords = c("longitude", "latitude"),
                        crs = 4326)
# st_geometry(housing_map)

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

```

- Prices seem to be related to location and to population density
- `ocean_proximity` may be a useful predictor, but in northern CA  prices in coastal districts are not too high

## Interactive map{.smaller}

```{r}
#| fig-width: 11
#| fig-height: 4 
#| fig-align: 'center'
#| echo: false

library(leaflet)

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

```

## Feature Exploration of numeric variables

```{r}
#| fig-width: 11
#| fig-height: 6 
#| fig-align: 'center'
#| echo: false
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

```

## Feature Exploration without outliers

```{r}
#| fig-width: 11
#| fig-height: 6 
#| fig-align: 'center'
#| echo: false
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

```


## Scatterplot - Correlation Numerical Variables


::: columns
::: {.column width="33%"}

```{r, eval=FALSE}
library(GGally)

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

```
:::

::: {.column width="67%"}
```{r, echo=FALSE}
#| fig-width: 11
#| fig-height: 6 
#| fig-align: 'center'
library(GGally)

housing_df %>% 
  select(price_category, median_income, 
         bedrooms_per_room, rooms_per_household, 
         population_per_household) %>% 
  ggscatmat(color="price_category", 
            corMethod = "spearman",
            alpha=0.2)+
  theme_bw()
```

:::
::::

## Categorical Variables

```{r, echo=FALSE}

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

```


## Categorical Variables


:::: columns
::: {.column width="37%"}

```{r, eval=FALSE}
#| fig-align: 'center'
housing_df %>%
  ggplot(aes(x = price_category, 
             y = ocean_proximity)) +
  geom_bin2d() +
  scale_fill_continuous(type = "viridis")+
  theme_light()+
  labs(x=NULL, y= NULL)
```
:::

::: {.column width="63%"}

```{r, echo=FALSE}
#| fig-height: 4.5 
#| fig-align: 'center'
#| echo: false
housing_df %>%
  ggplot(aes(price_category, ocean_proximity)) +
  geom_bin2d() +
  scale_fill_continuous(type = "viridis")+
  theme_light()+
  labs(x=NULL, y= NULL)
```
:::

::::

## Data Preparation


1. Data Cleaning: Handle missing values; fix or remove outliers
1. Feature selection
1. Feature engineering
1. Feature scaling
1. Create a validation set


##  Feature Selection and Data Split

```{r}
housing_df_new <- housing_df %>% 
  select( # select features
    longitude, latitude, # We keep long-lat but we will not use them in our model.
    price_category, median_income, 
    ocean_proximity, bedrooms_per_room, 
    rooms_per_household, population_per_household)

glimpse(housing_df_new)
```

<br>
**Split the data**

```{r}
set.seed(123)

data_split <- initial_split(housing_df_new, # updated data
                           prop = 0.8, 
                           strata = price_category)

train_data <- training(data_split) 
test_data <- testing(data_split)
```

## `recipe()`

::: footer
https://www.tmwr.org/pre-proc-table.html
:::

`recipe()` has two arguments:

1. a formula, e.g., `price_category ~ .`
    - any variable on the left-hand side of the tilde (`~`) is considered the model outcome (here, `price_category`). 
    - On the right-hand side of the tilde are the predictors. Variables may be listed by name (`x1 + x2 + ...`), or you can use the dot (.) to indicate all other variables as predictors.
2. the data, which is always the training dataset. `data = train_set`


```{r, eval=FALSE}
housing_rec <-
  recipe(price_category ~ .,
         data = train_data) 
```

## Data preprocessing `recipe()` {.smaller}

::: footer
https://www.tmwr.org/pre-proc-table.html
:::

::::{.columns}

::: {.column width="37%"}
- `update_role()` function to let recipes know that `longitude`, `latitude` have a custom role that we called "ID" (a role can have any character value). 
This tells the recipe to keep these two variables but not use them as either outcomes or predictors.

- `step_naomit()` removes rows of data with NAs We use `skip = TRUE` because we don’t want to perform this part to new data so that the number of samples in the assessment set is the same as the number of predicted values (even if they are NA).
    - Imputing data: Instead of deleting NAs, we can impute missing values by [any of the following methods](https://recipes.tidymodels.org/reference/index.html#step-functions-imputation)
    
:::
    
::: {.column width="62%"}    
  
![](img/step_impute.png){fig-align="center"}



:::
::::



## A few more steps in our recipe

- `step_novel()` converts all nominal variables to factors and takes care of other issues related to categorical variables.

- `step_log()` will log transform data (since some of our numerical variables are right-skewed). Note that this step can not be performed on negative numbers.

- `step_normalize()` normalizes (center and scales) the numeric variables to have a standard deviation of one and a mean of zero. (i.e., z-standardization).

- `step_dummy()` converts our factor column ocean_proximity into numeric binary (0 and 1) variables.

`step_zv()` removes any numeric variables that have zero variance.

`step_corr()` will remove predictor variables with high correlations with other predictor variables.


## Recipe we will use

```{r}
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
```

## What does our processed data look like? (1/2)

```{r, warning=FALSE, message=FALSE}
prepped_data <- 
  housing_rec %>% # use the recipe object
  prep() %>% # perform the recipe on training data
  juice() # extract only the preprocessed dataframe 

glimpse(prepped_data)
```

## What does our processed data look like? (2/2)

```{r, warning=FALSE, message=FALSE}
prepped_data %>% 
  select(median_income, rooms_per_household, population_per_household, price_category) %>% 
  ggpairs()
```


## Validation Set

Use k-fold cross validation to build a set of 10 validation folds


```{r}
set.seed(123)

cv_folds <-
 vfold_cv(train_data, 
          v = 10, 
          strata = price_category) 
```

## Model Building (1/2)

1. Pick a `model type`
2. set the `engine`
3. Set the `mode`: regression or classification

```{r}
# Logistic regression
log_spec <-  logistic_reg() %>%  # model type
  set_engine(engine = "glm") %>%  # model engine
  set_mode("classification") # model mode

# Show your model specification
log_spec
```

```{r}
# Decision Tree
tree_spec <- decision_tree() %>%
  set_engine(engine = "C5.0") %>%
  set_mode("classification")

tree_spec
```

## Model Building (2/2)

```{r}
# Random Forest
library(ranger)

rf_spec <- 
  rand_forest() %>% 
  set_engine("ranger", importance = "impurity") %>% 
  set_mode("classification")

```

```{r}
# Boosted tree (XGBoost)
library(xgboost)

xgb_spec <- 
  boost_tree() %>% 
  set_engine("xgboost") %>% 
  set_mode("classification") 

```

```{r}
# K-nearest neighbour (k-NN)
knn_spec <- 
  nearest_neighbor(neighbors = 4) %>% # we can adjust the number of neighbors 
  set_engine("kknn") %>% 
  set_mode("classification") 
```


## Bundle recipe and model with `workflows`

```{r}
log_wflow <- # new workflow object
 workflow() %>% # use workflow function
 add_recipe(housing_rec) %>%   # use the new recipe
 add_model(log_spec)   # add your model spec

# show object
log_wflow
```

## A few more workflows

```{r}
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
```

## Evaluate Models

Use validation set `cv_folds` to estimate performance of our models using `fit_resamples()`

- `fit_resamples()` fits our model to each resample and evaluate on the heldout set from each resample. 
- Computes performance metrics across some set of resamples to evaluate our models (like accuracy, AUC) and the models are not  stored. 
- We save the predictions to visualize  model fit and residuals with `control_resamples(save_pred = TRUE)`
- Collect  performance metrics with `collect_metrics()` and pick the model that does best on the validation set.

## Logistic regression results{.smaller}

```{r}
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

```

## `collect_predictions()` and get confusion matrix{.smaller}

```{r}
log_pred <- log_res %>% collect_predictions()

log_pred %>%  conf_mat(price_category, .pred_class) 
```

::::{.columns}

::: {.column width="50%"}

```{r}
#| fig-height: 4 
#| fig-align: 'center'
log_pred %>% 
  conf_mat(price_category, .pred_class) %>% 
  autoplot(type = "mosaic") +
  geom_label(aes(
      x = (xmax + xmin) / 2, 
      y = (ymax + ymin) / 2, 
      label = c("TP", "FN", "FP", "TN")))

```

:::

:::{.column width="50%"}

```{r}
#| fig-height: 4 
#| fig-align: 'center'
log_pred %>% 
  conf_mat(price_category, .pred_class) %>% 
  autoplot(type = "heatmap")
```

:::

::::



## ROC Curve

```{r}
#| fig-height: 5 
#| fig-align: 'center'
log_pred %>% 
  group_by(id) %>% # id contains our folds
  roc_curve(price_category, .pred_above) %>% 
  autoplot()
```

## Decision Tree results

```{r}
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

```

## Random Forest

```{r}
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
```


## Boosted tree - XGBoost

```{r}
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




```

## K-nearest neighbour

```{r}
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
```

## Model Comparison

 
```{r, echo=FALSE}
#| fig-width: 11
#| fig-height: 6 
#| fig-align: 'center'
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
```
 
## `last_fit()` on test set{.smaller}

- `last_fit()`  fits a model to the whole training data and evaluates it on the test set. 
- provide the workflow object of the best model as well as the data split object (not the training data). 
 
```{r}
last_fit_xgb <- last_fit(xgb_wflow, 
                        split = data_split,
                        metrics = metric_set(
                          accuracy, f_meas, kap, precision,
                          recall, roc_auc, sens, spec))

last_fit_xgb %>% collect_metrics(summarize = TRUE)

#Compare to training
xgb_res %>% collect_metrics(summarize = TRUE)
```
 
If model fit to the training dataset also fits the test dataset well, minimal overfitting has taken place
 
## Variable importance using `{vip}` package{.smaller}


- `{vip}` visualizes variable importance scores for the top features. Note that we can’t create this type of plot for every model engine.

- Access the variable importance scores via the `.workflow` column; pluck out the first element in the workflow column, then pull out the fit from the workflow object. 

:::: columns
::: {.column width="25%"}

```{r, eval=FALSE}
library(vip)

last_fit_xgb %>% 
  pluck(".workflow", 1) %>%   
  pull_workflow_fit() %>% 
  vip(num_features = 10) +
  theme_light()
```
:::


::: {.column width="75%"}

```{r, echo=FALSE}
#| fig-height: 3.5 
#| fig-align: 'center'
library(vip)

last_fit_xgb %>% 
  pluck(".workflow", 1) %>%   
  pull_workflow_fit() %>% 
  vip(num_features = 10) +
  theme_light()

```

:::

::::


## Final Confusion Matrix

```{r}

last_fit_xgb %>%
  collect_predictions() %>% 
  conf_mat(price_category, .pred_class) %>% 
  autoplot(type = "heatmap")


```

## Final ROC curve

```{r}
#| fig-height: 3 
#| fig-align: 'center'
last_fit_xgb %>% 
  collect_predictions() %>% 
  roc_curve(price_category, .pred_above) %>% 
  autoplot()

```

