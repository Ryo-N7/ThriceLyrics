install.packages("ggrepel")
install.packages("wordcloud")
install.packages("gridExtra")
install.packages("lubridate")
install.packages("tidytext")
install.packages("")


# Packages:
library(knitr)
library(stringr)
library(lubridate)
library(ggrepel)
library(wordcloud)
library(gridExtra)
library(tidyverse)
library(tidytext)



df <- read.csv('thrice.df.csv', header=TRUE, stringsAsFactors = FALSE)
View(df)
df[df$ID == 100, "length"]
df[df$ID == 100, "lengthS"]
df[df$ID == 7, "length"]
df[df$ID == 7, "lengthS"]

df <- df %>% mutate(album = factor(album, levels = unique(album))) %>% 
       mutate(length = ms(length)) %>% 
       mutate(lengthS = seconds(length))
df[df$ID == 100, "length"]
df[df$ID == 100, "lengthS"]
df[df$ID == 7, "length"]
df[df$ID == 7, "lengthS"]

df <- rbind(df, "100" = c())
df[df$ID == 100, "length"] <- as.period("4M")



practice[practice$ID == 2 & practice$Time == "hour 1", "score 1"] <- 5

writersAll <- paste(df$writers, collapse=', ') # turn all artists into one list. each separated by commas (?)
writersAll <- str_replace_all(writersAll, ',,', ',')  # fix any double-commas (?)
writersAll
writersAll <- unlist(strsplit(writersAll, ', ')) # unlist writers. separated by the commas. (?), each appear "individually".

writersList
writersList <- sort(unique(writersAll))

writersListLabel <- str_replace_all(writersList, ' ', '_') # label of writers, replace " " with "_" for calling purposes
writersListLabel

?str_detect()


dfWriters <- df # copy df into new dfWriters dataframe
for(i in 1:length(writersList)){
  dfWriters[,writersListLabel[i]] <- str_detect(dfWriters$writers, writersList[i]) # detect per each row (song) which writer in WriterList appears T/F
}

buffer <- dfWriters[, 10:13]   # all rows with writers as T/F. 

writers <-  data.frame(writer = writersList,           # name of writer from writerlist
                       nSongs = apply(buffer, 2, sum), # number of songs written by writer from writerlist
                       row.names = NULL,
                       stringsAsFactors = F) 
writers <- writers %>%
  arrange(desc(nSongs))     # not much to see here as 99.999% of Thrice songs written by Dustin...LOL.

?kable()

length(unique(df$album))  # How many total albums have Thrice released?? (Counting each element from AlchemyIndex...)

df %>% group_by(year, album) %>% 
  summarise(SongNum = n(),
            duration = seconds_to_period(sum(lengthS)))

test.1 <- df %>% group_by(album) %>% 
  mutate(duration = seconds_to_period(sum(lengthS)))


df %>% seconds_to_period(sum(lengthS))

seconds_to_period(sum(df$lengthS))
df %>% summarise(duration = seconds_to_period(sum(lengthS)))
df %>% group_by(album) %>% summarise(duration = seconds_to_period(sum(lengthS)))

df %>% group_by(album) %>% select(length, lengthS)

df %>% filter(album == "Vheissu") %>% summarise(songnum = n(), duration = seconds_to_period(sum(lengthS)))
df %>% filter(album == "Identity Crisis") %>% summarise(songnum = n(), duration = seconds_to_period(sum(lengthS)))
df %>% filter(album == "The Illusion Of Safety") %>% summarise(duration = seconds_to_period(sum(lengthS)))
df %>% filter(album == "The Artist In The Ambulance") %>% summarise(duration = seconds_to_period(sum(lengthS)))
df %>% filter(album == "Major Minor") %>% summarise(duration = seconds_to_period(sum(lengthS)))
df %>% filter(album == "Beggars") %>% summarise(duration = seconds_to_period(sum(lengthS)))
df %>% filter(album == "To Be Everywhere And To Be Nowhere") %>% summarise(duration = seconds_to_period(sum(lengthS)))
df %>% filter(album == "The Alchemy Index Fire") %>% summarise(duration = seconds_to_period(sum(lengthS)))
df %>% filter(album == "The Alchemy Index Water") %>% summarise(duration = seconds_to_period(sum(lengthS)))
df %>% filter(album == "The Alchemy Index Air") %>% summarise(duration = seconds_to_period(sum(lengthS)))
df %>% filter(album == "The Alchemy Index Earth") %>% summarise(duration = seconds_to_period(sum(lengthS)))




#
df %>% distinct(album) %>% select(df$length, df$lengthS)
 



albums <- df %>% 
  group_by(year, album) %>% 
  summarise(nbreSongs=n(), duration=seconds_to_period(sum(lengthS)))
rm(albums)





