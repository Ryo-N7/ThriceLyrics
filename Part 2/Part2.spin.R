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
  mutate(numLines = str_count(lyrics, '<br>') + 1,       # num of lines per song
         numWord = str_count(lyrics, ' ') + 1)           # num of words per song
     
# how accurate are the numLines and numWord counts?
# comparison when unnest lyrics by line and word in later section of this article!

# lyrics separated by line, "<br>" tag
# uninterested in looking at line counts so split into lines on each <br> tag 
# and then unnest each word from each "line" of lyrics

wordToken <-  df %>%
  unnest_tokens(line, lyrics, token = stringr::str_split, pattern = ' <br>') %>%   # take out <br> tags
  unnest_tokens(word, line) %>% 
  mutate(wordCount = row_number()) %>%    # count of words in order of line, song, album
  select(-numLines, -numWord)             # take out numLines and numWord, can calculate independtly later

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
  select(-wordCount) %>%     # won't need wordCount
  arrange(ID)  # or track_num essentially same thing

countWord2 <- wordToken2 %>% count(word, sort=TRUE)

countWord2 %>% head(10)
# with no stop words, 'eyes', ''ll', ''ve', and 'love', 'light' are most common
# interesting in -light- of the fact that sentiment-score wise Thrice songs are overtly negative...
# we'll see more of that in Part 3

# graph of top words (no stop words) ####
countWord2 %>% head(10) %>% 
  ggplot(aes(reorder(word, n), n)) + 
  geom_bar(stat = "identity", fill = "darkgreen", alpha = 0.75) +
  xlab("Most common words") +
  coord_flip() +
  theme_bw() +
  theme(panel.grid.major.x = element_line(size = 1.25))

# WORD CLOUD (comparison stop vs. no-stop)   ####
library(wordcloud)
layout(matrix(c(1,2),1,2, byrow = TRUE))

wordcloud(words = countWord$word, freq = countWord$n, random.order = FALSE, max.words = 200, 
          colors = brewer.pal(8, "Dark2"), use.r.layout = TRUE)

# compare with wordcloud without stop_words!
wordcloud(countWord2$word, countWord2$n, random.order = FALSE, max.words = 200,
          colors = brewer.pal(8, "Dark2"), use.r.layout = TRUE)
# looks much better!


####    Explore dataset with dplyr verbs!   ####

# several ways to do this as shown in Part 1 using n_distinct() on `title`...
# the `n()` function specifically counts the # of obsv in the current group and can only be
# used inside summarize(), mutate(), and filter()
df %>% summarise(num_songs = n()) # 103 songs in total, as each row = 1 song

# another example of n(), usage in conjunction with group_by()
# songs per album (by order of # of songs desc) >>> did this in part 1
df %>% group_by(album) %>% 
  summarise(num_songs = n()) %>% 
  arrange(desc(num_songs)) 

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
# - The Weight has the most words (all incl. stop words)... also includes <br> though...
# - need to use wordToken instead of wordToken2 as stop_words should be included for sum
wordToken %>% 
  select(title, word) %>% 
  group_by(title) %>% 
  summarize(num_word = n()) %>% 
  arrange(desc(num_word))

# words per album?
aggregate(df$numWord, by = list(df$album), FUN = sum) %>% 
  arrange(desc(x))   
# instrumentals still count the blank as 1, but insignificant.
# numWord by album
df %>% group_by(album) %>% 
  summarize(num = sum(numWord)) %>% 
  arrange(desc(num)) %>% 
  ggplot(aes(reorder(album, num), num)) + geom_bar(stat = "identity") + coord_flip()

wordToken %>% 
  select(album, word) %>% 
  group_by(album) %>% 
  summarize(num_word = n()) %>% 
  arrange(desc(num_word))

# using count of "word" in unnested wordToken 
# and original df "numWord" estimates similar enough...


# random stuff
df %>% 
  filter(album == "Vheissu") %>% 
  select(numWord) %>% 
  sum()

sum(str_count(df$lyrics, "<br>"))   # 2562 breaks in total

sum(str_count(df$lyrics, "light"))
wordToken$word[wordToken$word == "light"]

wordToken2 %>% 
  str_count("light") %>% 
  sum()
# number of times "light" appears in lyrics of song (desc. order)

df %>% 
  mutate(number = str_count(lyrics, pattern = "light")) %>% 
  select(title, album, number) %>% 
  arrange(desc(number)) %>% 
  head(15)

df %>% 
  filter(title == "The Weight") %>% 
  select(album, title, numWord, numLines)
# 421
wordToken %>% 
  filter(title == "The Weight") %>% 
  summarize(num_word = n())    # count rows, in this case: one row = one word
# 383

# most frequent word in each song ####
wordToken2 %>% 
  group_by(title) %>% 
  count(word) %>% 
  arrange(desc(n))

# "I'll" in Black Honey >>> song about
# "image" and "invisible" in "Image of the Invisible" >>> given as shouted out during the chorus numerous
# times in the song... To consider: skewed toward phrases repeated in chorus!

# WordsPerNoStop
df %>%
  mutate(lyrics = str_replace_all(lyrics, '\'', ' ')) %>%
  unnest_tokens(line, lyrics, token = stringr::str_split, pattern = ' <br>') %>% 
  unnest_tokens(word, line) %>%
  anti_join(stop_words) %>%           # filter stop words
  group_by(title) %>% 
  summarize(wordcounts = n()) %>% 
  arrange(desc(wordcounts))

# Distribution of Songs by # of Words     same as above^ but not as accurate?
medianWord <- median(df$numWord)
one <- df %>% 
  ggplot(., aes(x = numWord)) +
  geom_histogram(binwidth = 10,
                 color = 'white',
                 fill = 'darkgreen') +
  geom_vline(aes(xintercept = medianWord), colour = "red", linetype = "dashed", size = 1.25) +
  scale_y_continuous(breaks = pretty_breaks(), expand = c(0, 0), limits = c(0, 12)) +
  scale_x_continuous(breaks = pretty_breaks(), expand = c(0, 0)) +
  xlab('Total # of Words') +
  ylab('# of Songs') +
  labs(title = 'Distribution of Songs by Number of Words', 
       subtitle = 'Dashed red line: median') + 
  theme_bw() +
  theme(panel.grid.minor = element_blank(), 
        plot.title = element_text(hjust = 0.5))

# WordsPerSong
WordsPerSong <- wordToken %>%      # NOT filter stop words (included in the lyrics for the total count)
  group_by(title) %>% 
  summarize(wordcounts = n()) %>%    #
  arrange(desc(wordcounts))

# Histogram of Word counts (per Song)   >>>> same as in uni-bi-tri section...
two <- WordsPerSong %>% 
  ggplot(aes(x = wordcounts)) + 
  geom_histogram(binwidth = 10, color = "white", fill = "darkgreen") +
  geom_vline(xintercept = median(WordsPerSong$wordcounts), color = "red", linetype = "dashed", size = 1.25) +
  scale_y_continuous(breaks = pretty_breaks(), expand = c(0, 0), limits = c(0, 12)) +
  scale_x_continuous(breaks = pretty_breaks(), expand = c(0, 0)) +
  xlab('Total # of Words') +
  ylab('# of Songs') +
  labs(title = 'Distribution of Songs by Number of Words', 
       subtitle = 'Dashed red line: median') + 
  theme_bw() +
  theme(panel.grid.minor = element_blank(), 
        plot.title = element_text(hjust = 0.5))

# compare accuracy of numWord method used...
grid.arrange(one, two)
# crucial difference: WordsPerSong filter out instrumentals all together!
# df includes instrumentals and has counted as 1

# most frequent unigrams per album: ####
# nested on albums!

word_count_nested <- wordToken2 %>% 
  group_by(album, word) %>% 
  summarize(count = n()) %>% 
  top_n(10) %>% 
  arrange(album, desc(count)) %>% 
  nest() 

word_count_nested$data

word_count_nested$data[[1]]

library(scales)
word_count_nested <- word_count_nested %>% 
  mutate(plot = map2(data, album, 
                     ~ggplot(data = .x) +
           geom_bar(aes(reorder(word, count), count), 
                    stat = "identity", fill = "darkgreen", width = 0.65) +
           scale_y_continuous(breaks = pretty_breaks(10), limits = c(0, 22), expand = c(0, 0)) +
           ggtitle(.y) +
           labs(x = NULL, y = NULL) +
           coord_flip() +  
           theme_bw()
           ))

glimpse(word_count_nested)
# can now see that column "data" is a list holds the top 10 words for each album (row)
# "plot" column is a list that holds the plot for each album (row)

# by selecting the specific element within the list, we can extract the plot for a certain album
word_count_nested$plot[[1]]
word_count_nested$plot[[2]]
word_count_nested$plot[[9]]
word_count_nested$plot[[11]]

word_count_nested %>% 
  unnest(data) %>%                   # take data out from list
  ggplot(aes(x = word, y = count)) +
  geom_bar(stat = "identity") +
  facet_grid(.~album)
# won't work with facetting

# save all plots in as one list
nested_plots <- word_count_nested$plot

glimpse(nested_plots)[[1]]
str(nested_plots, list.len = 2, max.level = 2)

# base R method with do.call() function:
do.call(grid.arrange, c(word_count_nested$plot, ncol = 3))

# how tf do i get this to work with purrr? lol.
map(c(unlist(nested_plots), grid.arrange, ncol = 3))
map(nested_plots, grid.arrange)
map(nested_plots, grid.arrange, ncol = 3, nrow = 5)

# works very easily with cowplot::plot_grid() function!
# call list of ggplots (per album) with plotlist = __ then specify # of columns/rows/etc...
library(cowplot)
plot_grid(plotlist = nested_plots, ncol = 3)

# save plot of most frequent word (for each album) individually now! ####
# apply the function ggsave() so that it iteratively saves the plot for each album!
map2(paste0(word_count_nested$album, ".pdf"), word_count_nested$plot, ggsave)
