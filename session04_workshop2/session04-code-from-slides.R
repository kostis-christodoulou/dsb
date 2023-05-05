library(tidyverse)
library(nycflights13)

## `group_by()` and why we need to `ungroup()`

flights %>% 
  group_by(origin, month) %>% 
  summarise(n = n()) 

flights %>% 
  group_by(origin, month) %>% 
  summarise(n = n()) %>%
  slice(1)

flights %>% 
  group_by(origin, month) %>% 
  summarise(n = n()) %>%
  ungroup() %>%
  slice(1)


## `across`: Apply a function, or set of functions, to columns
flights %>% 
  filter(month == 12, day >= 15) %>% 
  group_by(day) %>% 
  summarise(across(c(dep_delay, arr_delay), 
                   ~median(., na.rm = TRUE))) 

flights %>% 
  filter(month == 12, day >= 15) %>% 
  group_by(day) %>% 
  summarise(across(.cols  = c(dep_delay, arr_delay), 
                   .fns   = ~median(., na.rm = TRUE),
                   .names = "median_{.col}"))

flights %>% 
  filter(month == 12, day >= 15) %>% 
  group_by(day) %>% 
  summarise(across(.cols  = c(dep_delay, arr_delay), 
                   .fns   = list(mean = ~mean(., na.rm = TRUE),
                                 sd   = ~sd(., na.rm = TRUE)),
                   .names = "{.fn}_{.col}"))


## `distinct()` and `sample()`
flights %>%
  select(origin, dest) %>%
  distinct() %>%
  arrange(origin, dest)

flights %>% 
  select(year, origin, dest, month, day) %>% 
  sample_n(10)

flights %>% 
  select(year, origin, dest, month, day) %>% 
  sample_frac(0.02)

## `{tidyr}` package


## Organising your data

# We have four variables: `country`, `year`, `cases`, `population`
# The following tables organise the values in a different way

table1
table2
table3
table4a
table4b

  
# Ways data tend not to be tidy{.smaller}
# Column headers are values, not variable names.
# Multiple variables are stored in one column.
# Variables are stored in both rows and columns.
# Multiple types of observational units are stored in the same table.
# A single observational unit is stored in multiple tables.


wide_data <- 
  tribble(
    ~student_id, ~final_exam,~midterm,~individual_project,~group_project,
    "2457625",   79, 68,71,83,
    "1758293",   92, 73, 67,56,
    "1622247",   71, 87,74,77
  )

wide_data

## Wide and long data
# Variable `assignment` is spread across multiple columns:
  

wide_data %>% 
  pivot_longer(
    cols = 2:5,
    names_to = "assignment",
    values_to = "score"
  )

    
    
## `pivot_wider()`
    
# `{ukbabynames}`  contains a listing of UK baby names occurring more than three times per year between 1974 and 2020
    
library(ukbabynames)
glimpse(ukbabynames)
    
ukbabynames %>%
      group_by(year, sex) %>%
      summarise(count = sum(n))

      
## Plot % of males born every year
# Which dataframe makes it easier to calculate % of males born every year?
      

ukbabynames %>%
  group_by(year, sex) %>%
  summarise(count = sum(n)) %>% 
  ungroup()

ukbabynames %>%
  group_by(year, sex) %>%
  summarise(count = sum(n)) %>% 
  ungroup() %>% 
  pivot_wider(
    names_from = "sex",
    values_from = "count") 

ukbabynames %>%
  group_by(year, sex) %>%
  summarise(count = sum(n)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = "sex",
              values_from = "count") %>% 
  mutate(percent_male = M/(F+M))

    
ukbabynames %>%
  group_by(year, sex) %>%
  summarise(count = sum(n)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = "sex",
              values_from = "count") %>% 
  mutate(percent_male = M/(F+M)) %>% 
  ggplot(aes(x=year, y= percent_male))+
  geom_line()+
  theme_light()


## World Health Organisation TB Data
# - A subset of data from the WHO Global Tuberculosis (TB) Report, and accompanying global populations.
# - Data is broken down by year, country, age, gender, and diagnosis method
# - Part of `{tidyr}` package
    
glimpse(who)

# `Type of TB case`
# 
# - `rel`   relapse
# - `ep`     extra-pulmonary
# - `sn`    pulmonary, smear negative
# - `sp`    pulmonary, smear positive
# 
# `Gender`
# 
# -`m` male
# -`f` female
# :::
#   
#   `Age Group`
# 
# - `014` - 0 to 14 years old
# - `1524` - 15 to 24 years old
# - `2534` - 25 to 34 years old
# - `3544` - 35 to 44 years old
# - `4554` - 45 to 54 years old
# - `5564` - 55 to 64 years old
# - `65` - 65 and older
    
# Pivot longer... 
# the 5th through 60th columns of `who` 
# into a pair of key:value columns named `tb_codes` and `n.`
# Then `select` just the `country`, `year`, `tb_codes` and `n` variables.

who %>%
  pivot_longer(
    cols = 5:60, 
    names_to = "tb_codes",
    values_to = "n") %>%
  select(-iso2, -iso3)


## `tidyr::separate()`
## Splits a column by dividing values at a specific character.
    
who %>%
  pivot_longer(
    cols = 5:60, 
    names_to = "tb_codes",
    values_to = "n") %>%
  select(-iso2, -iso3) %>% 
  separate(tb_codes, 
           into = c("new", "type", "sexage"), 
           sep = "_")

## `tidyr::separate()`
# If `sep` is numeric, it is interpreted as character positions to split at. 
    
who %>%
  pivot_longer(
    cols = 5:60, 
    names_to = "tb_codes",
    values_to = "n") %>%
  select(-iso2, -iso3) %>% 
  separate(tb_codes, 
           into = c("new", "type", "sexage"), 
           sep = "_") %>% 
  select(-new) %>%
  separate(sexage, into = c("sex", "age"), sep = 1)


## `tidyr::drop_na()`
# Drops rows that contain NA’s in the specified columns.
    
who %>%
  pivot_longer(
    cols = 5:60, 
    names_to = "tb_codes",
    values_to = "n") %>%
  select(-iso2, -iso3) %>% 
  separate(tb_codes, 
           into = c("new", "type", "sexage"), 
           sep = "_") %>% 
  select(-new) %>%
  separate(sexage, into = c("sex", "age"), sep = 1) %>% 
  drop_na(n)


# -------------- Functions, functional programming




## `NA`s in vectors

vector <- c(1, 2, 3, 4, NA)
sum(vector)
mean(vector)
min(vector)
max(vector)

#use `na.rm=TRUE` to make sure NAs don't screw up your work
sum(vector, na.rm=TRUE)
mean(vector, na.rm=TRUE)
min(vector, na.rm=TRUE)
max(vector, na.rm=TRUE)


## Exercise on vectors
# - Create a character vector of colours using `c()`. 
# - Use colours `"grey90"` and `"#001e62"`. 
# - Assign the vector to a name, `my_colours`
# - Use the vector you just created to change the colours in the plot using `scale_colour_manual()`. 
# - Pass the vector of colours using the `values` argument.

library(tidyverse)
library(gapminder)

my_colours <- c("grey90", "#001e62")

gapminder %>%
  mutate(rwanda = ifelse(country == "Rwanda", TRUE, FALSE)) %>%
  ggplot(aes(year, lifeExp, color = rwanda, group = country)) +
  geom_line() +
  scale_colour_manual(values = my_colours) +
  theme_minimal()

my_colours <- c("grey90", "#001e62", "tomato")

# filter with a function on the fly
gapminder %>%
  mutate(rwanda = ifelse(country == "Rwanda", TRUE, FALSE)) %>%
  ggplot(aes(year, lifeExp, colour = rwanda, group = country)) +
  geom_line(
    data = function(x) filter(x, !rwanda),
    colour = my_colours[1]
  ) +
  geom_line(
    data = function(x) filter(x, rwanda),
    colour = my_colours[3],
    size = 4
  ) +
  theme_minimal()

  
## Writing functions

add_one <- function(x) {
  x <- x + 1
  x
}

add_one(1)

## Write a function!
  
# - Create a function called `simulated_data` that doesn't take any arguments.
# - In the function body, we'll return a tibble, a dataframe with three variables: `x`, `sex`, and `age`
# - For `x`, use `rnorm()` to generate 50 random numbers from a Normal distribution with a mean=0 and SD=1.
# - For `sex`, use `rep()` to create 50 values of "male" and "female". 
# 
# - Hint: You'll have to give `rep()` a character vector for the first argument. The `times` argument is how many times `rep()` should repeat the first argument, so make sure you account for that. If in doubt, type `?rep` in the console

# - For `age()` use the `sample()` function to sample 50 numbers from 25 to 50 with replacement.

set.seed = 18
sim_data <- function() {
  tibble(
    x = rnorm(50),
    sex = rep(c("male", "female"), times = 25),
    age = sample(25:50, size = 50, replace = TRUE)
  )
}
sim_data()



## `Passing the dots ...`

select_gapminder <- function(...) {
  gapminder %>%
    select(...)
}
select_gapminder(pop, year)

## Exercise passing the dots `...`
# 
# - Use `...` to pass the arguments of your function, `filter_summarise()`, to filter().
# - In `summarise`, get the n (count) and mean life expectancy of the filtered data set
# - Check `filter_summarize()` with `year == 2002`.
# - Check `filter_summarize()` with `year == 2002`, but also filter for European countries only.


filter_summarise <- function(...) {
  gapminder %>%
    filter(...) %>%
    summarise(n = n(), mean_lifeExp = mean(lifeExp))
}


filter_summarise(year == 2002)
  
filter_summarise(year == 2002, continent == "Africa")


## Programming with dplyr, ggplot2, et al

# What do you expect this function to do?

plot_hist <- function(x) {
  ggplot(gapminder, aes(x = x)) + 
    geom_histogram()
}

plot_hist(lifeExp)
plot_hist("lifeExp")

## `Curly curly {{}}`

plot_hist <- function(x) {
  ggplot(gapminder, aes(x = {{x}})) + 
    geom_histogram()
}

plot_hist(lifeExp)
plot_hist(pop)
plot_hist(gdpPercap)

## `top_scatterplot(variable, year, n)`

# 1. Filter `gapminder` by `year` using the value of `.year` (notice the period before hand!). You do NOT need curly-curly for this, as you are passing one value for `year`. 
# 2. To plot the top values for whatever `variable` we pass, we have to use `fct_reorder()`. Redefine `country` so it becomes a factor ordered by `{{variable}}`.
# 3. Use `slice_max()`, ordering by `{{variable}}` and take the top `n` values
# 4. Make a scatter plot. Use `{{variable}}` for `x`. For `y`, we'll use `country` 

top_scatterplot <- function(variable, .year, .n) {
  gapminder %>%
    filter(year == .year) %>%
    mutate(country  =  fct_reorder(country, {{variable}})) %>%
    slice_max(order_by = {{variable}}, n = .n) %>%
    ggplot(aes(x = {{variable}}, y = country)) +
    geom_point() +
    theme_minimal()
}



top_scatterplot(lifeExp, 1967, 30)
top_scatterplot(pop, 1982, 10)
top_scatterplot(gdpPercap, 1997, 20)


# Functional Programming
# A programming paradigm that treats computation as the evaluation of mathematical functions and avoids changing the state of variables


## Package `{purrr}`
# - All `map()` functions either accept:
#   
#   - function, 
# - formulas (used for succinctly generating anonymous functions), 
# - a character vector (used to extract components by name), or 
# - a numeric vector (used to extract by position).

#   | Map variant              | Description                            |
#   |--------------------------|----------------------------------------|
#   | `map()`                  | returns a list                         |
#   | `map_lgl()`              | returns a logical vector               |
#   | `map_int()`              | returns a integer vector               |
#   | `map_dbl()`              | returns a double vector                |
#   | `map_chr()`              | returns a character vector             |
#   | `map_df()` / `map_dfr()` | returns a data frame by row binding    |
#   | `map_dfc()`              | returns a data frame by column binding |
  
## Lists may contain anything

list(a = "hello world",
     b = 1,
     c = mean)


## Main problems with lists 

# - Viewing contents
# - Extracting contents
# - Using with functions


## Subsetting lists

number_list <- list(
  a = rnorm(8, mean = 2, sd = 1.4),
  b = rnorm(7, mean = 1, sd = 1),
  c = rnorm(6, mean = 10, sd = 3)
)
```


:::: {.columns}
::: {.column width="35%"}
`[]` outputs a list
```{r}
single_bracket <- number_list["a"]
single_bracket
typeof(single_bracket)
```
:::
  
  
  ::: {.column width="30%"}
`$` outputs a vector
```{r}
dollar <- number_list$a
dollar
typeof(dollar)
```
:::
  
  ::: {.column width="35%"}
`[[]]` outputs a vector
```{r}
double_bracket <- number_list[["a"]]
double_bracket
typeof(double_bracket)
```
:::
  ::::
  
  ## Subsetting lists
  
  <br>
  ![](img/session06/pepper-list.png){fig-align="center" fig-alt="error" fig-height="90%"}

## Vectorised operations don't work on lists 

:::: {.columns}
::: {.column width="40%"}

```{r}
mean(rnorm(100))
```
:::
  
  
  ::: {.column width="58%"}

```
mean(list(x = rnorm(100), y = rnorm(100), z = rnorm(100)))

[1] NA
Warning message:
  In mean.default(list(x = rnorm(100), y = rnorm(100), z = rnorm(100))) :
  argument is not numeric or logical: returning NA
```
:::
  ::::
  
  ## `purrr:map*()` functions{.smaller}
  
  `map(.x, .f)`

`.x` -a vector, list, or dataframe

`.y` -a function

:::: {.columns}
::: {.column width="40%"}
![](img/session06/purrr-map2.png){fig-align="center" fig-alt="error" fig-height="60%"}
:::
  
  ::: {.column width="60%"}
| `map` variant            | Description                            |
  |--------------------------|----------------------------------------|
  | `map()`                  | returns a list                         |
  | `map_lgl()`              | returns a logical vector               |
  | `map_int()`              | returns a integer vector               |
  | `map_dbl()`              | returns a double vector                |
  | `map_chr()`              | returns a character vector             |
  | `map_df()` / `map_dfr()` | returns a data frame by row binding    |
  | `map_dfc()`              | returns a data frame by column binding |
  :::
  ::::
  
## Lists and maps
 
#  What do you think will happen? Can you use `map()` to create the same output?

set.seed(18)  
x_list <- list(x = rnorm(100), 
               y = rnorm(100), 
               z = rnorm(100))

list(
  sum_x = sum(x_list$x),
  sum_y = sum(x_list$y),
  sum_z = sum(x_list$z)
)

# what if we use map?
map(x_list, sum)


## Using `map()` with dataframes

# calculate Standard deviation of all numeric variables
gapminder %>%
  dplyr::select_if(is.numeric) %>%
  map(sd)


## functions and `map()`

# - Write a function named `mean_sd` that returns the mean and standard deviation of a numeric vector.
# - Find the mean and SD of x
# - Map your function to x_list

set.seed(18)
x_list <- list(x = rnorm(100), 
               y = rnorm(100), 
               z = rnorm(100))
## Using `map()` with dataframes{.smaller}

```{r}
mean_sd <- function(x) {
  x_mean <- mean(x)
  x_sd <- sd(x)
  tibble(mean = x_mean, 
         sd = x_sd)
}


map(x_list, mean_sd)


## Passing functions to `map()`

# Pass directly to `map()`
# Use anonymous function
# Use `~`

## Returning types
map(gapminder, 
    ~length(unique(.x)))

map_int(gapminder, 
        ~length(unique(.x)))

map_dbl(gapminder, 
        ~length(unique(.x)))

map_chr(gapminder, 
        ~length(unique(.x)))

## `map()` challenge{.smaller}
  
# - Check the WHO TB data (`tidyr::who`) for any missing data.
# - Using the `~.f(.x)` shorthand, check each column for any missing values using `is.na()` and `any()`
# - Return a logical vector. Do we have any columns missing data? 
# - Try counting the number of NAs, returning an integer vector

map_lgl(who, 
        ~any(is.na(.x)))

map_int(who, 
        ~sum(is.na(.x)))


## `map()` and functions
  
set.seed(18)
exams <- list(
  student1 = round(runif(10, 50, 100)),
  student2 = round(runif(10, 50, 100)),
  student3 = round(runif(10, 50, 100)),
  student4 = round(runif(10, 50, 100)),
  student5 = round(runif(10, 50, 100))
)
exams

exams %>% 
  map(mean)

exams %>% 
  map_dbl(mean)

## `set_names()` and `map_dfr()`

names <- c("hw1","hw2","hw3","hw4","hw5","hw6","hw7","hw8","hw9","hw10") 
set_names(exams$student1, nm = names)

exams %>% 
  map(set_names, nm = names)

exams %>%
  map(set_names, nm = names) %>%
  map_dfr(bind_rows)


## `~` and `.` to map generic expressions

# - Begin the expression with `~`
# - Use a `.` to indicate where inputs should go

exams %>% 
  map_lgl(~mean(.) > 75)

## Exercise

# Recall the original mean of the scores

exams %>% 
  map_dbl(mean)


# Drop lowest score and recompute mean

(sum(`.input`) - min(`.input`)) / 9

exams %>% 
  map_dbl(~(sum(.) - min(.)) / 9) %>% 
  round(2)
