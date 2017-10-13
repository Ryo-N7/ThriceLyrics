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

df <- read.csv('thrice.df.csv', header = TRUE, stringsAsFactors = FALSE)

df <- df %>% 
  mutate(album = factor(album, levels = unique(album)),
         length = ms(length),
         lengthS = seconds(length))

glimpse(df)
# both length and lengthS are now Period/S4 variables!
# album is a factor!


# Lyrics analysis ---------------------------------------------------------

# lyrics separated by line, "<br>" tag
# uninterested in looking at line counts so split into lines on each <br> tag 
# and then unnest each word from each "line" of lyrics

wordToken <-  df %>%
  unnest_tokens(line, lyrics, token = stringr::str_split, pattern = ' <br>') %>%   # take out <br> tags
  unnest_tokens(word, line, to_lower = TRUE) 

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
  arrange(ID)  # or track_num essentially same thing

countWord2 <- wordToken2 %>% count(word, sort=TRUE)

countWord2 %>% head(10)





# Uni-bi-trigrams ---------------------------------------------------------

nGram <- data_frame(text = paste(wordToken$word, collapse = ' '))       # stop-words
nGramCleaned <- data_frame(text=paste(wordToken2$word, collapse = ' ')) # NO stop-words

# top 20 unigrams
countWord2 %>% head(20) %>% 
  ggplot(aes(reorder(word, n), n)) + 
  geom_bar(stat = "identity") +
  coord_flip()

glimpse(countWord2)

# BI

biGrams <-  nGram %>% 
  unnest_tokens(ngram, text, token = "ngrams", n = 2) 

biGrams <- df %>% 
  unnest_tokens(line, lyrics, token = stringr::str_split, pattern = ' <br>') %>%   # take out <br> tags
  unnest_tokens(ngram, line, token = "ngrams", n = 2)


biGramsCleaned <-  nGramCleaned %>% 
  unnest_tokens(ngram, text, token = "ngrams", n = 2) %>%
  count(ngram, sort = TRUE)

glimpse(biGrams)

bi_count <- biGrams %>% count(ngram, sort = TRUE)

bi_count %>% head(20) %>% 
  ggplot(aes(reorder(ngram, n), n)) + 
  geom_bar(stat = "identity") +
  coord_flip()

biGramsSep <- biGrams %>% 
  separate(ngram, c("word1", "word2", sep = " "))

biGramsSep <- biGramsCleaned %>% 
  separate(ngram, c("word1", "word2", sep = " "))

biGramsSep <- biGrams %>% 
  select(title, album, ngram) %>% 
  separate(ngram, c("word1", "word2", sep = " "))  # doesnt properly parse ' s...


# TRI

triGrams <-  nGram %>% 
  unnest_tokens(ngram, text, token = "ngrams", n = 3) 

triGrams <- df %>% 
  unnest_tokens(line, lyrics, token = stringr::str_split, pattern = ' <br>') %>%   # take out <br> tags
  unnest_tokens(ngram, line, token = "ngrams", n = 3)

tri_count <- triGrams %>% count(ngram, sort = TRUE)

tri_count %>% head(20) %>% 
  ggplot(aes(reorder(ngram, n), n)) +
  geom_bar(stat = "identity") +
  coord_flip()

triGrams %>% 
  separate(ngram, c("word1", "word2", "word3"), sep = " ") %>% 
  count(word1, word2, word3, sort = T)

# take out stop_words in each ngram...
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
# 2091 unique words


# TABLE:

tab <- cbind(countWord, empty, countWord2)

knitr::kable(tab[1:20,], format='markdown', row.names = F,
             col.names = c('All uniGrams', 'Freq', ' ', '|', ' ', 'Cleaned uniGrams', 'Freq'))


