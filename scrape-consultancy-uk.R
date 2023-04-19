library(tidyverse)
library(rvest)
library(polite)
library(janitor)
library(LSAfun)



#dataTable > tbody > tr:nth-child(43) > td:nth-child(1) > a

get_listings <- function(page) {
  
  base_url <- "https://www.consultancy.uk/jobs/page/"
  url <- str_c(base_url, page)
  listings_html <- read_html(url)
  
  job <- listings_html %>%
    html_nodes(css = "span.title") %>%
    html_text2() 
  
  firm <- listings_html %>%
    html_nodes(css = ".hide-phone .row-link") %>%
    html_text2() 

  link <- listings_html %>%
    html_nodes(css = ".hide-phone .row-link") %>%
    html_attr('href') %>%
    str_c("https://www.consultancy.uk", .)
  
    
  functional_area <- listings_html %>%
    html_elements(css = ".initial") %>%
    html_text2() 
  
  type <- listings_html %>%
    html_nodes(css = ".hide-tablet-landscape .row-link") %>%
    html_text2() 

  
  # ad_text <- link %>% 
  #   read_html() %>%
  #   html_elements("p, h1, h2, .columnn, #content li") %>%  
  #   
  #   # Use html_text2() to extract the plain text contents of an HTML element:
  #   html_text2() %>%
  #   
  #   # remove empty elements
  #   .[. != ""] 
  

  jobs_df <- tibble(
    job = job,
    firm     = firm,
    functional_area     = functional_area,
    type    = type,
    link = link
  ) 

  
    return(jobs_df)
}


pages <- 1:9
jobs <- map_df(pages, get_listings)

jobs |> 
  count(firm, sort=TRUE)

jobs |> 
  count(functional_area, sort=TRUE) %>%
  filter(functional_area != "Unknown") |> 
  mutate(perc = n/sum(n))

jobs |> 
  count(job, sort=TRUE) |> 
  View()


# summarise text from links- use LSAfun package
# https://search.r-project.org/CRAN/refmans/LSAfun/html/genericSummary.html

# genericSummary(D,k=1)
# whereby 'D' specifies your text document and 'k' the number of sentences to be used in the summary. (Further modifications are shown in the package documentation).


url2sum <- "https://www.consultancy.uk/jobs/26249/bain-company/consultant"
url2sum <- "https://www.consultancy.uk/jobs/35933/cil-management-consultants/senior-analyst"

read_ad <- read_html(url2sum) |> 
  html_elements("p, h1, h2, .columnn, #content li") |> 
  
  # Use html_text2() to extract the plain text contents of an HTML element:
  html_text2()%>%
  # remove empty elements
  .[. != ""] 

read_ad

genericSummary(read_ad,k=3)
