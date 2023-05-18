library(tidyverse)

expand_dates <- function(df){
  df %>% # takes as argument a dataframe df...
    mutate(# will generate new variables... 
      across(where(is.Date), # if in the df we have a variable of type date (is.Date())
              list( # then create three new variables year, month, day, as follows
                year = lubridate::year,
                month = lubridate::month,
                day = lubridate::mday
                )
      )
    )
}

# Let's see if it works on holywood age gaps
age_gaps <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-14/age_gaps.csv')

age_gaps %>% 
  glimpse()

age_gaps <- age_gaps %>% 
  expand_dates() 

age_gaps %>% 
  glimpse()


