# Packages:
library(tidyverse)
library(tidytext)
library(lubridate)
library(stringr)

library(gplots)
library(ggrepel)
library(scales)
library(gridExtra)

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

glimpse(lineToken)
# lyrics separated by line, "<br>" tag

wordToken <-  lineToken %>% 
  unnest_tokens(word, line) %>% 
  mutate(wordCount = row_number()) %>%    # count of words in order of line, song, album
  select(-numLines, -numWord)

glimpse(wordToken)
# uncleaned unigrams containing all 'stop words' such as 'I', 'you', 'we', 'very', etc. etc.


countWord <- wordToken %>% count(word, sort=TRUE)
countWord  %>% head(10)
# 'the' is most common....

# Include stop_words ------------------------------------------------------

data("stop_words")
stop_words         # data base of 'stop words' to clean unigrams.

stop_words %>% head(5)

wordToken2 <- wordToken %>% 
  anti_join(stop_words) %>%                 
  # use anti-join to take out words in wordToken that appear in stop_words
  select(-wordCount, -lineCount) %>% 
  arrange(ID)  # or track_num essentially same thing

countWord2 <- wordToken2 %>% count(word, sort=TRUE)

countWord2 %>% head(10)
# with no stop words, 'eyes', ''ll', ''ve', and 'love', 'light' are most common
# interesting in -light- of the fact that sentiment-score wise Thrice songs are overtly negative...

countWord2 %>% head(10) %>% 
  ggplot(aes(reorder(word, n), n)) + geom_bar(stat = "identity") +
  coord_flip()


# WORD CLOUD
library(wordcloud)
layout(matrix(c(1,2),1,2, byrow = TRUE))

wordcloud(countWord$word, countWord$n, random.order=FALSE, max.words = 100, 
          colors=brewer.pal(8, "Dark2"), use.r.layout=TRUE)

wordcloud(countWord2$word, countWord2$n, random.order=FALSE, max.words = 100,
          colors=brewer.pal(8, "Dark2"), use.r.layout=TRUE)

####    Explore dataset with dplyr verbs!   ####

df %>% summarise(Num.Songs = n()) # 103 songs in total, as each row = 1 song

# songs per album (by order of # of songs desc)
df %>% group_by(album) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count)) 

# song with most # of lines
df %>% group_by(title) %>%
  select(title, numLines) %>% 
  arrange(desc(numLines)) %>% 
  head(10)

# album with most # of lines
df %>% group_by(album) %>% 
  summarize(lines = sum(numLines)) %>% 
  arrange(desc(lines))

# words per song?
df %>% group_by(title) %>% 
  select(title, album, numWord) %>% 
  arrange(desc(numWord))    
# The Weight has the most words (all incl. stop words)... also includes <br> though...

# words per album?
aggregate(df$numWord, by = list(df$album), FUN = sum) %>% 
  arrange(desc(x))   
# instrumentals still count the blank as 1, but insignificant.
# numWord by album
# with dplyr
df %>% group_by(album) %>% 
  summarize(num = sum(numWord)) %>% 
  arrange(desc(num)) %>% 
  ggplot(aes(reorder(album, num), num)) + geom_bar(stat = "identity") + coord_flip()


# random stuff
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

df %>% 
  filter(title == "As The Crow Flies") %>% 
  select(-lyrics)

wordToken2 %>% 
  group_by(title) %>% 
  count(word) %>% 
  arrange(desc(n))


###
###
###

# WordsPerNoStop
df %>%
  mutate(lyrics = str_replace_all(lyrics, '\'', ' ')) %>%
  unnest_tokens(line, lyrics, token = stringr::str_split, pattern = ' <br>') %>% 
  unnest_tokens(word, line) %>%
  anti_join(stop_words) %>%           # filter stop words
  group_by(title) %>% 
  summarize(wordcounts = n()) %>% 
  arrange(desc(wordcounts))

# WordsPerSong
WordsPerSong <- df %>%
  mutate(lyrics = str_replace_all(lyrics, '\'', ' ')) %>%
  unnest_tokens(line, lyrics, token = stringr::str_split, pattern = ' <br>') %>% 
  unnest_tokens(word, line) %>%      # NOT filter stop words (they are still words in the lyrics for the total count)
  group_by(title) %>% 
  summarize(wordcounts = n()) %>%    # # of rows per group(title)  ????
  arrange(desc(wordcounts))

# Histogram of Word counts (per Song)   >>>> same as in uni-bi-tri section...
WordsPerSong %>% 
  ggplot(aes(x = wordcounts)) + 
  geom_histogram(binwidth = 10) +
  geom_vline(xintercept = mean(WordsPerSong$wordcounts), color = "red") +
  scale_x_continuous(breaks = pretty_breaks(n = 10)) +
  scale_y_continuous(breaks = pretty_breaks(n = 10))

# Distribution of Songs by # of Words     same as above^
medianWord <- median(df$numWord)

df %>% 
  ggplot(., aes(x = numWord)) +
  geom_histogram(binwidth = 10,
                 color = 'white',
                 fill = 'darkgreen') +
  geom_vline(aes(xintercept = medianWord), colour = "red", linetype = "dashed", size = 1.25) +
  scale_y_continuous(breaks = pretty_breaks(), expand = c(0, 0), limits = c(0, 12)) +
  scale_x_continuous(breaks = pretty_breaks()) +
  xlab('Total # of Words') +
  ylab('# of Songs') +
  labs(title = 'Distribution of Songs by Number of Words', 
       subtitle = 'Dashed red line: median') + 
  theme_bw() +
  theme(panel.grid.minor = element_blank(), 
        plot.title = element_text(hjust = 0.5))


# LinesPerSong
df %>%
  mutate(lyrics = str_replace_all(lyrics, '\'', ' ')) %>%
  unnest_tokens(line, lyrics, token = stringr::str_split, pattern = ' <br>') %>% 
  group_by(title) %>% 
  summarise(linecounts = n()) %>% 
  arrange(desc(linecounts))


# most frequent unigrams per album:
# nest on albums!

word_count_nested <- wordToken2 %>% 
  group_by(album, word) %>% 
  summarize(count = n()) %>% 
  top_n(5) %>% 
  arrange(album, desc(count)) %>% 
  nest() 

word_count_nested$data

word_count_nested$data[[1]]

library(scales)
word_count_nested <- word_count_nested %>% 
  mutate(plot = map2(data, album, ~ggplot(data = .x) +
           geom_bar(aes(reorder(word, count), count), 
                    stat = "identity", fill = "darkgreen", width = 0.65) +
           scale_y_continuous(breaks = pretty_breaks(10), limits = c(0, 24), expand = c(0, 0)) +
           ggtitle(.y) +
           labs(x = NULL, y = NULL) +
           coord_flip()  ))

word_count_nested$plot
word_count_nested$plot[[1]]
word_count_nested$plot[[2]]
word_count_nested$plot[[9]]
word_count_nested$plot[[11]]

word_count_nested %>% 
  unnest(data) %>% 
  ggplot(aes(x = word, y = count)) +
  geom_bar(stat = "identity") +
  facet_grid(.~album)

grid.arrange(word_count_nested %>% 
  unnest(data) %>% 
  ggplot(aes(x = word, y = count)) +
  geom_bar(stat = "identity"))

nested_plots <- word_count_nested$plot
glimpse(nested_plots)[[1]]
str(nested_plots, list.len = 2, max.level = 2)

do.call(grid.arrange, c(word_count_nested$plot, ncol = 3))

# how tf do i get this to work with purrr? lol.
map(c(unlist(nested_plots), grid.arrange, ncol = 3))
map(nested_plots, grid.arrange)
map(nested_plots, grid.arrange, ncol = 3, nrow = 5)

library(cowplot)
cowplot::plot_grid(plotlist = nested_plots, ncol = 3)

# works now!
map2(paste0(word_count_nested$album, ".pdf"), word_count_nested$plot, ggsave)

# but how to show in one panel a la grid.arrange() ...?

###############################################

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


# Uni-bi-trigrams ---------------------------------------------------------

nGram <- data_frame(text = paste(wordToken$word, collapse = ' '))       # stop-words
nGramCleaned <- data_frame(text=paste(wordToken2$word, collapse = ' ')) # NO stop-words

countWord2 %>% head(20) %>% 
  ggplot(aes(reorder(word, n), n)) + 
  geom_bar(stat = "identity") +
  coord_flip()

glimpse(countWord2)

# BI

biGrams <-  nGram %>% 
  unnest_tokens(ngram, text, token = "ngrams", n = 2) 

biGramsCleaned <-  nGramCleaned %>% 
  unnest_tokens(ngram, text, token = "ngrams", n = 2) %>%
  count(ngram, sort = TRUE)

glimpse(biGrams)

bi_count <- biGrams %>% count(ngram, sort = TRUE)

bi_count %>% head(20) %>% 
  ggplot(aes(reorder(ngram, n), n)) + 
  geom_bar(stat = "identity") +
  coord_flip()

biGramsSep <- biGramsCleaned %>% 
  separate(ngram, c("word1", "word2", sep = " "))


# TRI

triGrams <-  nGram %>% 
  unnest_tokens(ngram, text, token = "ngrams", n = 3) 

tri_count <- triGrams %>% count(ngram, sort = TRUE)

tri_count %>% head(20) %>% 
  ggplot(aes(reorder(ngram, n), n)) +
  geom_bar(stat = "identity") +
  coord_flip()

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



# TABLE:

tab <- cbind(countWord, empty, countWord2)

kable(tab[1:20,], format='markdown', row.names = F,
      col.names = c('All uniGrams', 'Freq', ' ', '|', ' ', 'Cleaned uniGrams', 'Freq'))










