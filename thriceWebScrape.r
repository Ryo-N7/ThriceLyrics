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


url <- "https://en.wikipedia.org/wiki/Thrice"
head_url <- "https://en.wikipedia.org"

# href
album_url <- url %>% 
  read_html() %>% 
  html_nodes(".mw-parser-output > ul:nth-child(68) > li > i > a") %>% 
  html_attr("href")


album_links <- paste0(head_url, album_url)

album_links[1] %>% read_html() %>% html_nodes(".tracklist")

thrice_metadata <- map_df(album_links, function(i) {
  
  page <- read_html(i) 
  
  tibble(
    track_num <- page %>% 
      html_nodes(".tracklist") %>% 
      html_table() %>% 
      as.data.frame() %>% 
      select(No.) %>% as.matrix(),
    
    title = page %>%  
      html_nodes(".tracklist") %>% 
      html_table() %>% 
      as.data.frame() %>% 
      select(Title) %>% as.matrix(),
    
    song_length = page %>% 
      html_nodes(".tracklist") %>% 
      html_table() %>% 
      as.data.frame() %>% 
      select(Length) %>% as.matrix()
  )
  
}) 


# Error: Columns `track_num <- page %>% html_nodes(".tracklist") %>% html_table() %>% \n    
# as.data.frame() %>% select(No.) %>% as.matrix()`, `title`, `song_length` must be 1d atomic 
# vectors or lists


# ATTEMPT 2: get entire table, then split up into columns in tibble ####

url <- "https://en.wikipedia.org/wiki/Thrice"
head_url <- "https://en.wikipedia.org"

# href
album_url <- url %>% 
  read_html() %>% 
  html_nodes(".mw-parser-output > ul:nth-child(68) > li > i > a") %>% 
  html_attr("href")


album_links <- paste0(head_url, album_url)

album_links[1] %>% 
  read_html() %>% 
  html_nodes(".tracklist") %>% 
  .[[1]] %>% 
  html_table() %>% glimpse()
  mutate(song_num = as.numeric(`No.`),
         title = `Title` %>% stringr::str_replace_all('"', ""),
         song_length = lubridate::ms(Length)) %>% glimpse()


album_links[9] %>% 
  read_html() %>% 
  html_nodes(".tracklist") %>% 
  .[[1]] %>% 
  html_table() %>% glimpse()
  mutate(song_num = as.numeric(`No.`),
         title = `Title` %>% stringr::str_replace_all('"', ""),
         song_length = lubridate::ms(Length)) %>% glimpse()

# 2, 3, in strings
# 9 doesnt exist as it appears as a LIST in wikipedia.... RRRRRRRRRRRRRR

album_links[9] %>% 
    read_html() %>% 
    html_nodes(".tracklist") %>% 
    html_table() %>% glimpse()
  mutate(song_num = as.numeric(`No.`),
         title = `Title` %>% stringr::str_replace_all('"', ""),
         song_length = lubridate::ms(Length)) %>% glimpse()

album_links[1:2]
album_links

album_links_trial <- album_links[1:3]

album_links_again <- album_links[c(1,4:8)]
album_links_again


ex_1 <- album_links[1] %>% 
  read_html() %>% 
  html_nodes(".tracklist") %>% 
  .[[1]] %>% 
  html_table() #%>% as.data.frame()

str(ex_1)
glimpse(ex_1)

ex_1 %>% mutate(song_num = as.numeric(`No.`))
ex_1 %>% mutate(son_len = as.Date(Length, format = '%M:%S'))
ex_1 %>% mutate(son_len = ms(Length))


thrice_metadata <- map_df(album_links_again, function(i) {
  
  page <- read_html(i) 
  
  meta_table <- page %>% 
      html_nodes(".tracklist") %>% 
      .[[1]] %>% 
      html_table() %>% 
      as.data.frame()
  
  meta_table <- meta_table %>% 
    mutate(song_num = as.numeric(`No.`),
           title = `Title` %>% stringr::str_replace_all('"', ""),
           song_length = lubridate::ms(Length))
  
}) 

glimpse(thrice_metadata)
# for the most aprt it's ok.... Length is chr var but that can be transformed later on.
getwd()
write.csv(thrice_metadata, "~/R_materials/ThriceLyrics/thrice_metadata.csv")

thrice_metadata <- map_df(album_links_again, function(i) {
  
  page <- read_html(i) 
  
  meta_table <- page %>% 
    html_nodes(".tracklist") %>% 
    .[[1]] %>% 
    html_table() %>% 
    as.data.frame()
  
  meta_table <- meta_table %>% 
    mutate(album = i)
  
}) 



# not mutate()

thrice_metadata <- map_dfr(album_links_trial, function(i) {
  
  page <- read_html(i) 
  
  meta_table <- page %>% 
    html_nodes(".tracklist") %>% 
    .[[1]] %>% 
    html_table() %>% 
    rename(song_num = `No.`)
  
}) 










# try this from: https://community.rstudio.com/t/whats-the-most-interesting-use-of-rvest-youve-seen-in-the-wild/745/7

####Straight from my notes: scrape links from a specified column in a well-formatted HTML table. ####

library(rvest)
library(xml2)

# Get table
html_table <- url %>%
  read_html() %>%
  html_nodes("table") %>%
  .[[1]]

# Get column names in table
header <- html_table %>%
  html_nodes("thead") %>%
  html_nodes("th") %>%
  html_text()

# Find position of specific column
pos <- grep(column_pattern, header)

# Get table rows
rows <- html_table %>%
  html_nodes("tbody") %>%
  html_nodes("tr")

# Loop through rows and extract `href` attribute from `a` at position `pos`
links <- sapply(rows, function(x) {
  children <- xml2::xml_children(x)
  children[pos] %>% html_nodes("a") %>% html_attr("href")
})











## 12/18/17:




########  ALBUMS: 1-4       NOT WORK     2, 3

thrice_metadata <- map_df(al_link, function(i) {
  
  page <- read_html(i) 
  
  meta_table <- page %>% 
    html_nodes(".tracklist") %>% 
    .[[1]] %>% 
    html_table() 
  
}) 


#######   ALBUMS: 1, 4-8


thrice_metadata <- map_df(album_links_again, function(i) {
  
  page <- read_html(i) 
  
  meta_table <- page %>% 
    html_nodes(".tracklist") %>% 
    .[[1]] %>% 
    html_table() %>% 
    as.data.frame()
  
  meta_table <- meta_table %>% 
    mutate(album = i)
  
}) 



## first album only:
ex_1 <- album_links[1] %>% 
  read_html() %>% 
  html_nodes(".tracklist") %>% 
  .[[1]] %>% 
  html_table() 

str(ex_1)
glimpse(ex_1)

ex_1 %>% mutate(son_len = ms(Length))

ex_1 %>% mutate(Num = str_replace(`No.`, "\\.", "") %>% as.numeric())


album_links[2] %>% 
  read_html() %>% 
  html_nodes(".tracklist") %>% 
  .[[1]] %>% 
  html_table() %>% mutate(son_len = ms(Length))

# 12/18/17: album 2 and album 3, the `No.` appear as chr rather than dbl. with track num appearing as 1.   2.   3. rather than 1    2   3
# also NOT grabbing 2nd table for Alchemy Index >>> Vol. II and Vol. IV

album_links[5] %>% 
  read_html() %>% 
  html_nodes(".tracklist") %>% 
  html_table()  # grab BOTH .[[1]] and .[[2]] for Alchemy Index albums

album_links[6] %>% 
  read_html() %>% 
  html_nodes(".tracklist") %>% 
  html_table()  # grab BOTH .[[1]] and .[[2]] for Alchemy Index albums

alc <- album_links[6] %>% 
  read_html() %>% 
  html_nodes(".tracklist") %>% 
  html_table() %>% as.data.frame()

alc %>% cbind()


album_links[3] %>% 
  read_html() %>% 
  html_nodes(".tracklist") %>% 
  .[[1]] %>% 
  html_table()

album_links[1] %>% 
  read_html() %>% 
  html_nodes(".tracklist") %>%
  .[[1]] %>% 
  html_table()

list_al <- album_links[1] %>% 
  read_html() %>% 
  html_nodes(".tracklist") %>%
  html_table()

list_al %>% as.data.frame()

album_links[7] %>% 
  read_html() %>% 
  html_nodes(".tracklist") %>% 
  .[[1]] %>% 
  html_table()

album_links[9] %>% 
  read_html() %>% 
  html_nodes(".tracklist") %>% 
  .[[1]] %>% 
  html_table() %>% glimpse()
mutate(song_num = as.numeric(`No.`),
       title = `Title` %>% stringr::str_replace_all('"', ""),
       song_length = lubridate::ms(Length)) %>% glimpse()

# 9 doesnt exist as it appears as a LIST in wikipedia.... RRRRRRRRRRRRRR >>> 12/18/17 created by self


album_links[9] %>% 
  read_html() %>% 
  html_nodes(".tracklist") %>% 
  html_table() %>% glimpse()
mutate(song_num = as.numeric(`No.`),
       title = `Title` %>% stringr::str_replace_all('"', ""),
       song_length = lubridate::ms(Length)) %>% glimpse()

album_links[1:2]
album_links

album_links_trial <- album_links[1:3]

album_links_again <- album_links[c(1,4:8)]
album_links_again

al_link <- album_links[1:4]
al_link



