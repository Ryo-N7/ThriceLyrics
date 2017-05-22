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
library(scales)



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

# df <- rbind(df, "100" = c())
# df[df$ID == 100, "length"] <- as.period("4M")
# practice[practice$ID == 2 & practice$Time == "hour 1", "score 1"] <- 5

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
test.1 %>% print(nrow = n(.))

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

# INDIVIDUAL ALBUMS SHOW DURATION BUT NOT WHEN AS WHOLE WHYYYYYYYYYYYYYYYYYYY


df %>% ggplot(aes(x = as.numeric(lengthS))) + 
       geom_histogram(binwidth = 10, 
                      color = 'white',
                      fill = '#FCCB85') +
       scale_y_continuous(breaks = pretty_breaks()) +
       xlab('Seconds') +
       ylab('# of Songs') +
       labs(title = 'Distr. of Songs by Length')

as.numeric(df$lengthS)

writers %>%
  mutate(writer = as.factor(writer)) %>%
  mutate(writer = reorder(writer, nSongs)) %>%
  top_n(10)

## Lyrics analyses:

df <- df %>%
  mutate(lyrics = str_replace_all(lyrics, '\'', ' ')) %>%
  mutate(numLines = str_count(lyrics, '<br>') + 1) %>%
  mutate(numWord = str_count(lyrics, ' ') + 1)

lineToken <- df %>%
  unnest_tokens(line, lyrics, token = stringr::str_split, pattern = ' <br>') %>% 
  mutate(lineCount = row_number())
lineToken

wordToken <-  lineToken %>% 
  unnest_tokens(word, line) %>% 
  mutate(wordCount = row_number())
wordToken # uncleaned unigrams containing all 'stop words' such as 'I', 'you', 'we', 'very', etc. etc.


countWord <- count(wordToken, word, sort=TRUE)
countWord <- head(countWord, 100)
empty <- data.frame(a=character(100),b=rep('|',100),c=character(100),
                    stringsAsFactors = FALSE)

data("stop_words")     # data base of 'stop words' to clean unigrams.
stop_words %>% print(n = 1149)

wordToken2 <- wordToken %>% 
  anti_join(stop_words) %>%
  arrange(wordCount)

countWord2 <- count(wordToken2, word, sort=TRUE)
countWord2 <- head(countWord2, 100)


?paste()
nGram <- data_frame(text = paste(wordToken$word, collapse = ' '))
nGramCleaned <- data_frame(text=paste(wordToken2$word, collapse = ' '))

biGrams <-  nGram %>% 
  unnest_tokens(ngram, text, token = "ngrams", n = 2) %>%
  count(ngram, sort = TRUE)

biGramsCleaned <-  nGramCleaned %>% 
  unnest_tokens(ngram, text, token = "ngrams", n = 2) %>%
  count(ngram, sort = TRUE)

triGrams <-  nGram %>% 
  unnest_tokens(ngram, text, token = "ngrams", n = 3) %>%
  count(ngram, sort = TRUE)



medianWord <- median(df$numWord)

uniqueWords <- wordToken2 %>% 
  select(word) %>% 
  filter(!str_detect(word, '[0-9]')) %>% 
  group_by(word) %>% 
  filter(row_number(word) == 1) %>% 
  arrange(word)

numUniqueWords <- nrow(uniqueWords)
numUniqueWords

df %>% 
  ggplot(., aes(x=numWord)) +
  geom_histogram(binwidth=10,
                 color='white',
                 fill='#FCCB85') +
  geom_vline(aes(xintercept=medianWord), colour="#990000", linetype="dashed") +
  coord_cartesian(ylim=c(0, 15)) + 
  scale_y_continuous(breaks=seq(0, 15, 1)) +
  scale_x_continuous(breaks=seq(0, 400, 20)) +
  theme(panel.grid.minor = element_blank()) +
  xlab('Total # of Words') +
  ylab('# of Songs') +
  labs(title='Distribution of Songs by Number of Words', 
       subtitle='Verses repeats not included - Dashed red line is median')


tab <- cbind(countWord, empty, countWord2)

kable(tab[1:20,], format='markdown', row.names = F,
      col.names = c('All uniGrams', 'Freq', ' ', '|', ' ', 'Cleaned uniGrams', 'Freq'))


layout(matrix(c(1,2),1,2, byrow = TRUE))
wordcloud(countWord$word, countWord$n, random.order=FALSE, max.words = 100, 
          colors=brewer.pal(8, "Dark2"), use.r.layout=TRUE)
wordcloud(countWord2$word, countWord2$n, random.order=FALSE, max.words = 100,
          colors=brewer.pal(8, "Dark2"), use.r.layout=TRUE)

# per album:
l <- length(levels(wordToken2$album))
plotList <- list()
for(i in 1:l){
  part <- wordToken2[wordToken2$album == levels(wordToken2$album)[i],] %>%
    group_by(album) %>%
    count(word) %>%
    top_n(10)
  p <- ggplot(part[1:10,], aes(reorder(word,n), n)) +
    geom_bar(stat = "identity", fill='#FCCB85', width=0.65) +
    #        scale_fill_discrete(drop=F) +
    labs(y=NULL, x=NULL, title=paste('Album: ', levels(wordToken2$album)[i], sep='')) +
    coord_flip() +
    theme(plot.title = element_text(size=11))
  plotList[[i]] <- p
}
do.call(grid.arrange, c(plotList, ncol=3))




