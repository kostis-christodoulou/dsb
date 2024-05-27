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



## Decision trees

library(rpart.plot)
tree_fit %>%
  extract_fit_engine() %>%
  rpart.plot(roundint = FALSE)
 
# -   Series of splits or if/then statements based on predictors
# 
# -   First the tree *grows* until some condition is met (maximum depth, no more data)
# 
# -   Then the tree is *pruned* to reduce its complexity

## Decision trees
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