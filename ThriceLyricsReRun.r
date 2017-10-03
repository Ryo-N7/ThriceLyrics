# Packages:
library(tidyverse)
library(tidytext)
library(lubridate)
library(stringr)
library(broom)
library(RColorBrewer)

library(gplots)
library(ggrepel)
library(scales)
library(hrbrthemes)
library(wordcloud)
library(gridExtra)

library(knitr)

# load dataset ------------------------------------------------------------

df <- read.csv('thrice.df.csv', header=TRUE, stringsAsFactors = FALSE)

df <- df %>% 
       mutate(album = factor(album, levels = unique(album)),
              length = ms(length),
              lengthS = seconds(length))

glimpse(df)
# both length and lengthS are now Period/S4 variables!
# album is a factor!


# Lyrics analysis ---------------------------------------------------------

df <- df %>%
  mutate(lyrics = str_replace_all(lyrics, '\'', ' ')) %>%   # replace apostrophes in lyrics with blank
  mutate(numLines = str_count(lyrics, '<br>') + 1) %>%      # num of lines per song
  mutate(numWord = str_count(lyrics, ' ') + 1)              # num of words per song

lineToken <- df %>%
  unnest_tokens(line, lyrics, token = stringr::str_split, pattern = ' <br>') %>% 
  mutate(lineCount = row_number())

lineToken # lyrics separated by line, "<br>" tag

wordToken <-  lineToken %>% 
  unnest_tokens(word, line) %>% 
  mutate(wordCount = row_number()) %>%    # count of words in order of line, song, album
  select(-numLines, -numWord)

wordToken # uncleaned unigrams containing all 'stop words' such as 'I', 'you', 'we', 'very', etc. etc.


countWord <- count(wordToken, word, sort=TRUE)
countWord <- head(countWord, 100)
# 'the' is most common....

# empty <- data.frame(a=character(100),b=rep('|',100),c=character(100),
                    # stringsAsFactors = FALSE)


# Include stop_words ------------------------------------------------------

data("stop_words")
stop_words         # data base of 'stop words' to clean unigrams.

stop_words %>% head(5)

wordToken2 <- wordToken %>% 
  anti_join(stop_words) %>%
  select(-wordCount, -lineCount) %>% 
  arrange(ID)  # or track_num essentially same thing

countWord2 <- count(wordToken2, word, sort=TRUE)
countWord2 <- head(countWord2, 100)

countWord2 %>% head(5)
# with no stop words, 'eyes', ''ll', ''ve', and 'love', 'light' are most common
# interesting in -light- of the fact that sentiment-score wise Thrice songs are overtly negative...


###    CHECKING DATASET WITH SIMPLE DPLYR VERBS!   ####

df %>% summarise(Num.Songs = n()) # 103 songs in total, as each row = 1 song

# songs per album (by order of # of songs desc)
df %>% group_by(album) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count)) 

# song with most # of lines
df %>% group_by(title) %>%
  select(title, album, numLines) %>% 
  arrange(desc(numLines))    

# album with most # of lines
df %>% group_by(album) %>% 
  select(album, numLines) %>% 
  arrange(desc(numLines))


# words per song?
df %>% group_by(title) %>% 
  select(title, album, numWord) %>% 
  arrange(desc(numWord))    
# The Weight has the most words (all incl. stop words)... also includes <br> though...

aggregate(df$numWord, by = list(df$album), FUN = sum) %>% 
  arrange(desc(x))   
# instrumentals still count the blank as 1, but insignificant.
# numWord by album
aggregate(df$numWord, by = list(df$album), FUN = sum, na.rm = F)

df %>% filter(album == "Vheissu") %>% select(numWord) %>% sum()


sum(str_count(df$lyrics, "<br>"))   # 2562 breaks in total

sum(str_count(df$lyrics, "light"))
wordToken$word[wordToken$word == "light"]
# number of times "light" appears in lyrics of song (desc. order)

df %>% 
  mutate(number = str_count(lyrics, pattern = "light")) %>% 
  select(title, album, number) %>% 
  arrange(desc(number)) %>% 
  head(5)

wordToken2 %>% 
  group_by(title) %>% 
  count(word) %>% 
  arrange(desc(n))

###
###
###

WordsPerSong_no_stop <- df %>%
  mutate(lyrics = str_replace_all(lyrics, '\'', ' ')) %>%
  unnest_tokens(line, lyrics, token = stringr::str_split, pattern = ' <br>') %>% 
  unnest_tokens(word, line) %>%
  anti_join(stop_words) %>%           # filter stop words
  group_by(title) %>% 
  summarize(wordcounts = n()) %>% 
  arrange(desc(wordcounts))

WordsPerSong <- df %>%
  mutate(lyrics = str_replace_all(lyrics, '\'', ' ')) %>%
  unnest_tokens(line, lyrics, token = stringr::str_split, pattern = ' <br>') %>% 
  unnest_tokens(word, line) %>%      # NOT filter stop words (they are still words in the lyrics for the total count)
  group_by(title) %>% 
  summarize(wordcounts = n()) %>%    # # of rows per group(title)  ????
  arrange(desc(wordcounts))

LinesPerSong <- df %>%
  mutate(lyrics = str_replace_all(lyrics, '\'', ' ')) %>%
  unnest_tokens(line, lyrics, token = stringr::str_split, pattern = ' <br>') %>% 
  group_by(title) %>% 
  summarise(linecounts = n()) %>% 
  arrange(desc(linecounts))

PerSong <- full_join(WordsPerSong, LinesPerSong, by = "title")




# wordcount by song
asdf <- TEST %>% group_by(title) %>% 
  summarize(wordcounts = n()) %>%    # # of rows per group(title)  ????
  arrange(desc(wordcounts))

# Histogram of Word counts (per Song)   >>>> same as in uni-bi-tri section...
WordsPerSong %>% 
  ggplot(aes(x = wordcounts)) + 
  geom_histogram(binwidth = 10) +
  geom_vline(xintercept = mean(WordsPerSong$wordcounts), color = "red") +
  scale_x_continuous(breaks = pretty_breaks(n = 10)) +
  scale_y_continuous(breaks = pretty_breaks(n = 10))

df %>% 
  filter(title == "As The Crow Flies") %>% 
  select(-lyrics)







# Uni-bi-trigrams ---------------------------------------------------------

nGram <- data_frame(text = paste(wordToken$word, collapse = ' '))
nGramCleaned <- data_frame(text=paste(wordToken2$word, collapse = ' '))

# BI

biGrams <-  nGram %>% 
  unnest_tokens(ngram, text, token = "ngrams", n = 2) 

biGramsCleaned <-  nGramCleaned %>% 
  unnest_tokens(ngram, text, token = "ngrams", n = 2) %>%
  count(ngram, sort = TRUE)

biGramsCleaned %>%
  count(ngram, sort = TRUE)

biGramsSep <- biGramsCleaned %>% 
  separate(ngram, c("word1", "word2", sep = " "))


# TRI

triGrams <-  nGram %>% 
  unnest_tokens(ngram, text, token = "ngrams", n = 3) 

triGrams %>% 
  separate(ngram, c("word1", "word2", "word3"), sep = " ") %>% 
  count(word1, word2, word3, sort = T)

triGrams %>% 
  separate(ngram, c("word1", "word2", "word3"), sep = " ") %>% 
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word,
         !word3 %in% stop_words$word) %>% 
  count(word1, word2, word3, sort = T)

?top_n

# AFINN 
AFINN <- get_sentiments("afinn")

love_words <- biGramsSep %>% 
  filter(word1 == "love") %>% 
  inner_join(AFINN, by = c(word2 = "word")) %>%
  count(word2, score, sort = TRUE) %>%
  ungroup()

love_words %>% 
  mutate(contribution = nn * score) %>% 
  arrange(desc(abs(contribution))) %>% 
  head(20) %>% 
  mutate(word2 = reorder(word2, contribution)) %>% 
  ggplot(aes(word2, nn * score, fill = nn *score > 0)) + 
  geom_col(show.legend = F) +
  coord_flip()

# love betrays, love ... die, love ... evil, love ... chance, love .../ true, love ... loyalty

negation_words <- c("not", "no", "never", "without")

negated_words <- biGramsSep %>%
  filter(word1 %in% negation_words) %>%
  inner_join(AFINN, by = c(word2 = "word")) %>%
  count(word1, word2, score, sort = TRUE) %>%
  ungroup()

medianWord <- median(df$numWord)

uniqueWords <- wordToken2 %>% 
  select(word) %>% 
  filter(!str_detect(word, '[0-9]')) %>% 
  group_by(word) %>% 
  filter(row_number(word) == 1) %>% 
  arrange(word)

numUniqueWords <- nrow(uniqueWords)
numUniqueWords



# Distribution of Songs by # of Words
df %>% 
  ggplot(., aes(x = numWord)) +
  geom_histogram(binwidth = 10,
                 color = 'white',
                 fill = '#FCCB85') +
  geom_vline(aes(xintercept = medianWord), colour="#990000", linetype="dashed") +
  coord_cartesian(ylim = c(0, 15)) + 
  scale_y_continuous(breaks = seq(0, 15, 1)) +
  scale_x_continuous(breaks = seq(0, 400, 20)) +
  theme(panel.grid.minor = element_blank()) +
  xlab('Total # of Words') +
  ylab('# of Songs') +
  labs(title = 'Distribution of Songs by Number of Words', 
       subtitle = 'Verses repeats not included - Dashed red line is median')



# TABLE:

tab <- cbind(countWord, empty, countWord2)

kable(tab[1:20,], format='markdown', row.names = F,
      col.names = c('All uniGrams', 'Freq', ' ', '|', ' ', 'Cleaned uniGrams', 'Freq'))



# WORD CLOUD
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


