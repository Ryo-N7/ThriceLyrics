# Get lyrics through webscraping!
library(tidytext)
library(stringr)
library(lubridate)
library(purrr)
library(tidyverse)
library(rvest)

# scrape song names from Artist Page:

url <- "https://www.azlyrics.com/t/thrice.html"
url2 <- "https://www.azlyrics.com/s/sum41.html"

#listAlbum
#listAlbum > a:nth-child(61)
#listAlbum > a:nth-child(63)
#listAlbum > a:nth-child(65)
# div.album:nth-child(68)

url %>% 
  read_html() %>% 
  html_nodes("#listAlbum > a") %>% 
  html_attr("href") %>% 
  as_tibble() %>% 
  na.omit() %>% 
  mutate(value = str_replace(value, "..", "")) %>% 
  mutate(value = paste0("https://azlyrics.com", value))


url2 %>% 
  read_html() %>% 
  html_nodes("#listAlbum > a") %>% 
  html_attr("href") %>% 
  as_tibble() %>% 
  na.omit() %>% 
  mutate(value = str_replace(value, "..", "")) %>% 
  mutate(value = paste0("https://azlyrics.com", value))


# >>>>

url <- "https://www.azlyrics.com/t/thrice.html"

url_link <- url %>% 
  read_html() %>% 
  html_nodes("#listAlbum > a") %>% 
  html_attr("href") %>% 
  as_tibble() %>% 
  na.omit() %>% 
  mutate(value = str_replace(value, "..", "")) %>% 
  mutate(value = paste0("https://azlyrics.com", value)) %>% as.matrix()

# map_df()
thrice <- map_df(url_link, function(i) {
  
  pg <- read_html(i) 
  
  tibble(
    title = pg %>%  
      html_nodes("b") %>% 
      .[[2]] %>% 
      html_text(),
    
    lyrics = pg %>% 
      html_nodes("div.col-xs-12:nth-child(2) > div:nth-child(8)") %>% 
      html_text()
  )
  
})


# grab album release year? >>> url link needs to be from different base...
# >>> just extract from when wikipedia web scrap instead...

glimpse(thrice)

write.csv(thrice, "~/R_materials/ThriceLyrics/thrice_webscrape.csv")

thrice <- read.csv("~/R_materials/ThriceLyrics/thrice_webscrape.csv", stringsAsFactors = FALSE)



# clean the strings:

thrice %>% 
  mutate(title = str_replace_all(title, pattern = "\"", "")) %>% 
  mutate(lyrics = str_replace_all(lyrics, pattern = "\r", ""),
         lyrics = str_replace_all(lyrics, pattern = "\n", " "),
         lyrics = trimws(lyrics, "both")) %>% glimpse()


thrice %>% 
  mutate(lyrics = str_detect(lyrics, pattern = "[[:punct:]]"))

thrice %>% select(lyrics) %>% filter(title == "Whistleblower")

glimpse(thrice)
thrice %>% filter(title == "All Eyes" )



# Wikipedia meta data -----------------------------------------------------

url <- "https://en.wikipedia.org/wiki/Thrice"

# album titles
url %>% 
  read_html() %>% 
  html_nodes(".mw-parser-output > ul:nth-child(68) > li > i > a") %>% 
  html_attr("title")

# album titles (text) with year
url %>% 
  read_html() %>% 
  html_nodes(".mw-parser-output > ul:nth-child(68) > li") %>% 
  html_text("#text")

# href AND title
url %>% 
  read_html() %>% 
  html_nodes(".mw-parser-output > ul:nth-child(68) > li > i > a") %>% 
  html_attrs()

# href
url %>% 
  read_html() %>% 
  html_nodes(".mw-parser-output > ul:nth-child(68) > li > i > a") %>% 
  html_attr("href")


# track list + lengths
.tracklist > tbody:nth-child(1)


.tracklist > tbody:nth-child(1)

.tracklist > tbody:nth-child(1) > tr:nth-child(3) > td:nth-child(2)

.tracklist > tbody:nth-child(1) > tr:nth-child(3) > td:nth-child(2)

u <- "https://en.wikipedia.org/wiki/Identity_Crisis_(Thrice_album)"

u %>% 
  read_html() %>% 
  html_nodes(".tracklist") %>% 
  html_table() %>% str()

# song lengths
u %>% 
  read_html() %>% 
  html_nodes(".tracklist") %>% 
  html_table() %>% 
  as.data.frame() %>% 
  select(Length)

# song titles
u %>% 
  read_html() %>% 
  html_nodes(".tracklist") %>% 
  html_table() %>% 
  as.data.frame() %>% 
  select(Title)

# track num.
u %>% 
  read_html() %>% 
  html_nodes(".tracklist") %>% 
  html_table() %>% 
  as.data.frame() %>% 
  select(No.)



head_url <- "https://en.wikipedia.org"



