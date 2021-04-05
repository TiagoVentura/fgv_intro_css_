# Scrap Igor
library(tidyverse)
library(httr)
library(rebus)
library(xml2)
library(rvest)

# Go to the web -----------------------------------------------------------

url <- "https://www.latinnews.com/component/k2/itemlist/category/33.html?archive=true&archive_id=33&period=2020"
years <- 2003:2004
url_bind <- paste0("https://www.latinnews.com/component/k2/itemlist/category/33.html?archive=true&archive_id=33&period=", years)
url <- url_bind[1]

# Get links from the url
to_get_url <- function(url){
  
text <- url %>% 
  read_html() %>%
  html_nodes(".archive_item") %>%
  html_text()

link <- url %>% 
  read_html() %>%
  html_nodes(".archive_item") %>%
  html_attr("href") %>%
  paste0("https://www.latinnews.com/", .)

link <- link[1:5]
text <- text[1:5]


list_abstracts <- map_chr(link, to_get_links) %>%
                  map_chr(~str_replace_all(.x, "[\t\r\n]" , ""))

outputs <- list(text, link, list_abstracts) %>%
              set_names(., nm=c("text", "link", "abstracts")) 

return(outputs)
}

# extract text from the links

to_get_links<- function(link){
  print("Collected")  
  Sys.sleep(runif(1, 0,5))
  
abstract <- link %>%
    read_html() %>%
    html_nodes(".itemFullText") %>%
    html_text() %>%
    paste0(., collapse = " ")
return(abstract)
  }


res <- to_get_url(url_bind[[1]]) %>%
            as_tibble() %>% 
            mutate(years=years[[1]])


# run alll
res <- map(url_bind, to_get_url)

# Make it a nice data table
map2(res, years, ~ as_tibble(.x) %>% mutate(years=.y)) %>% bind_rows()
