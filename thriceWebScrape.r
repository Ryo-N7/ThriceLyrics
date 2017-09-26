# Get lyrics through webscraping!
library(tidytext)
library(stringr)
library(lubridate)
library(purrr)
library(tidyverse)
library(rvest)



# Define list of songs:


# scrape lyrics names:
thriceNameUrl <- "http://www.azlyrics.com/t/thrice.html"

thriceName <- thriceNameUrl %>% 
  read_html() %>% 
  html_nodes("listAlbum") %>% 
  html_text()

thriceName <- thriceNameUrl %>% 
  read_html() %>% 
  html_nodes("listAlbum") %>% 
  html_attr("href")

thriceName

??html_attr()

# try html_attr()??

#listAlbum > a:nth-child(4)
#listAlbum
#listAlbum > a:nth-child(2)
#listAlbum
# div.album:nth-child(2)
#\31 4286#\31 4286
#listAlbumdiv.album:nth-child(2)
#listAlbum
thriceName

# Clean poem names:
thriceName <- thriceName %>% 
  str_replace_all(pattern = "\r", replacement = "") %>% 
  str_replace_all(pattern = "\n", replacement = " ") %>% 
  str_replace_all(pattern = "[  ]{2}", replacement = "") %>% 
  str_replace_all(pattern = "[[:punct:]]", replacement = "") %>% 
  tolower()

head(poemName)
poemName[9] <- "he mourns for the change"
poemName[24] <- "the old men admiring themselves"

head(poemName, 24)

# scrape poems and dates:
nameVec <- unlist(str_split(poemName, pattern = " "))
nameVec


url <- "http://www.poetry-archive.com/y/at_galway_races.html"
date <-  url %>%
  read_html() %>%
  html_nodes("td font") %>%
  html_text()

date[1]
date[2]
date[3]
date[3] %>% str_extract(pattern = "[0-9]+")
date[1] %>% str_replace_all(pattern = "\r", replacement = "") %>% 
  str_replace_all(pattern = "\n", replacement = " ") %>% 
  tolower()

### Function across multiple Lyrics Pages

getThriceLyrics <- function(poemName) {
  # split string at empty space and unlist the result  
  nameVec <- unlist(str_split(poemName, pattern = " "))
  
  # use result in url 
  url <- str_c("http://www.poetry-archive.com/y/", 
               paste(nameVec, collapse = "_"),
               ".html") 
  
  # scrape for poem and clean
  poem <- url %>%
    read_html() %>%
    html_nodes("dl") %>%
    html_text() %>%
    str_replace_all(pattern = "\r", replacement = "") %>%
    str_replace_all(pattern = "\n", replacement = " ") %>%
    str_replace_all(pattern = "[  ]{2}", replacement = "")
  
  # scrape for date 
  date <- url %>%
    read_html() %>%
    html_nodes("td font") %>%
    html_text() 
  
  # clean dates
  date <- date[3] %>%
    str_extract(pattern = "[0-9]+")
  
  # pause before function return
  Sys.sleep(runif(1, 0, 1))
  
  return(list(poem = poem, date = date))
}