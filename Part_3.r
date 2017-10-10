library(stringr)

library(tidyverse)
library(tidytext)
library(lubridate)

library(ggrepel)
library(scales)
library(gplots)
library(gridExtra)

library(wordcloud)

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

df <- df %>%
  mutate(numLines = str_count(lyrics, '<br>') + 1) %>%      # num of lines per song
  mutate(numWord = str_count(lyrics, ' ') + 1)              # num of words per song

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




# Sentiment analysis ------------------------------------------------------

# using WORDTOKEN2 <<<

tidy_lyrics <- wordToken2 %>% 
  filter(!grepl('[0-9]', word)) %>% 
  left_join(get_sentiments("bing"), by = "word") %>% 
  group_by(album) %>%    # will show up in ALL subsequent so take note. use ungroup()
  mutate(linenumber = row_number(),
         sentiment = ifelse(is.na(sentiment), 'neutral', sentiment)) %>% 
  ungroup()

# Net sentiment ratio by album across time.
tidy_lyrics %>% 
  group_by(album) %>% 
  count(sentiment) %>% 
  spread(key = sentiment, value = n) %>%     # turn each sentiment into unique columns!!
  mutate(sentiment_ratio = (positive - negative) / (positive + negative)) # include neutral?

# total sentiment of every word (minus stop words) from all songs
tidy_lyrics %>% 
  count(sentiment)
# Positive: 423, Negative: 912, Neutral: 5095

# Net sentiment ratio for each song 
tidy_lyrics %>% 
  group_by(title, album) %>% 
  count(sentiment) %>% 
  spread(key = sentiment, value = n) %>% 
  mutate(sentiment_ratio = (positive - negative) / (positive + negative))
# NAs in some sentiment columns >>> convert to ZEROs...

# use replace_na() from the tidyr package!
tidy_lyrics %>% 
  group_by(title, album) %>% 
  count(sentiment) %>% 
  spread(key = sentiment, value = n) %>% 
  replace_na(replace = list(negative = 0, neutral = 0, positive = 0)) %>%  # replace NAs with ZEROs!
  mutate(sentiment_ratio = (positive - negative) / (positive + negative))

# now group by album!
tidy_lyrics %>% 
  group_by(title, album) %>% 
  count(sentiment) %>% 
  spread(key = sentiment, value = n) %>% 
  replace_na(replace = list(negative = 0, neutral = 0, positive = 0)) %>%  # replace NAs with ZEROs!
  mutate(sentiment_ratio = (positive - negative) / (positive + negative)) %>% 
  group_by(album) %>% 
  summarize(mean_album = mean(sentiment_ratio))


# Lyrics sentiment ratio graph ####
# without neutral

lyrics_sentiment <-  tidy_lyrics %>% 
  group_by(album, year) %>% 
  count(sentiment) %>% 
  spread(key = sentiment, value = n) %>% 
  replace_na(replace = list(negative = 0, neutral = 0, positive = 0)) %>%   # replace NAs with ZEROs!
  mutate(sentiment_ratio = (positive - negative) / (positive + negative)) %>% # NO neutral in equation...
  select(album, year, sentiment_ratio)

library(hrbrthemes)

lyrics_sentiment %>% 
  ggplot(aes(reorder(album, desc(sentiment_ratio)), sentiment_ratio)) + 
  geom_bar(stat = 'identity', fill = "darkgreen") +  # aes(fill = sentiment_ratio > 0)   irrelevant as ALL negative...
  geom_text(aes(label = album), size = 3, angle = 90, hjust = -0.07) +
  scale_fill_manual(guide = FALSE, values = c('#565b63', '#c40909')) +
  scale_y_percent(limits = c(-0.55, 0.15), breaks = pretty_breaks(7)) +    # from hrbrthemes
  theme_bw() +                     # from hrbrthemes
  theme(axis.text.x = element_blank(), axis.title.x = element_blank()) +
  ggtitle("Lyrics Sentiment Ratio", subtitle = "(Positive-Negative) / (Positive + Negative)") +
  labs(x = "Albums", y = "Sentiment Ratio (%)")

# needs some work........    why negative? look at HOW negative is defined.
# negative talk but with positive action with negative undertones???? 


# try with neutral as well...
tidy_lyrics %>% 
  group_by(album, year) %>% 
  count(sentiment) %>% 
  spread(key = sentiment, value = n) %>% 
  replace_na(replace = list(negative = 0, neutral = 0, positive = 0)) %>%   # replace NAs with ZEROs!
  mutate(sentiment_ratio = (positive - negative) / (positive + negative + neutral)) %>% # NO neutral in equation...
  select(album, year, sentiment_ratio) %>% 
  ggplot(aes(album, sentiment_ratio)) + 
  geom_bar(stat = 'identity', fill = "darkgreen") +  # aes(fill = sentiment_ratio > 0)   irrelevant as ALL negative...
  scale_fill_manual(guide = FALSE, values = c('#565b63', '#c40909')) +
  scale_y_percent(limits = c(-0.15, 0.10), breaks = pretty_breaks(7)) +    # from hrbrthemes
  theme_bw() +                     # from hrbrthemes
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 0.95)) +
  ggtitle("Lyrics Sentiment Ratio", subtitle = "(Positive-Negative) / (Positive + Negative + Neutral)") +
  labs(x = "Albums", y = "Sentiment Ratio (%)")




# Most common pos.neg words in THrice lyrics! ####

word_count <- tidy_lyrics %>% 
  count(word, sentiment, sort = T) %>% 
  ungroup()

top_sentiments <-  word_count %>% 
  filter(sentiment != 'neutral') %>% 
  group_by(sentiment) %>% 
  top_n(6, wt = n) %>% 
  mutate(num = ifelse(sentiment == "negative", -n, n)) %>%  # count of negative words as negative #s!
  mutate(word = reorder(word, num)) %>% 
  select(word, sentiment, num)

# plot the occurences of the most common pos-neg words!
ggplot(top_sentiments, aes(reorder(word, num), num, fill = sentiment)) +
  geom_bar(stat = 'identity', alpha = 0.75) + 
  scale_fill_manual(guide = F, values = c("black", "darkgreen")) +
  scale_y_continuous(limits = c(-40, 70), breaks = pretty_breaks(12)) + # c(-40, -25, -10, 0, 10, 25, 40, 55, 70)
  labs(y = "Number of Occurrences",
       x = '',
       title = 'Lyrics Sentiment of Thrice',
       subtitle = 'Most Common Positive and Negative Words') +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = 1.1))

# separate between negative and positive words
# order by number of occurences
# another way to visualize this is using a wordcloud!

# Word cloud: Most common Pos-Neg words in Thrice lyrics ####
library(wordcloud)

tidy_lyrics %>% 
  filter(sentiment != 'neutral') %>% 
  count(word, sentiment, sort = T) %>% 
  reshape2::acast(word ~ sentiment, value.var = "n", fill = 0) %>% 
  comparison.cloud(colors = c("black", "darkgreen"), title.size = 1.5)

# LOVE appears most, free, faith, perfect, grace...
# FALL appears most, dead, burn, fear, sick...

# instead of reshape2::acast()   use a spread/gather??
# comparisond.cloud() takes only matrix as input!


# with nrc >>>>  Distribution of emotion words BOXPLOT: ####

emotions_lyrics <- wordToken2 %>% 
  filter(!grepl('[0-9]', word)) %>%                  # because numbers cant have feelings
  left_join(get_sentiments("nrc"), by = "word") %>% 
  filter(!(sentiment == "negative" | sentiment == "positive")) %>% 
  group_by(album, sentiment) %>% 
  summarize(freq = n()) %>% 
  mutate(percent = freq / sum(freq)) %>%   # round()   *100
  select(-freq) %>% 
  ungroup() 

emotion_box <- emotions_lyrics %>% 
  mutate(percent = round(percent*100)) %>% 
  spread(sentiment, percent, fill = 0) %>% 
  ungroup()

# brewer.pal for creating color palettes
# n = number of colors to use, name = which set to use
# (8) to return the vector of 8 colors chosen
cols <- colorRampPalette(brewer.pal(n = 8, name = "Set1"))(8)

emotion_box %>% 
  select(-album) %>% 
  boxplot2(col = cols, lty = 1, shrink = 0.8, textcolor = "red", 
           xlab = "Emotion Terms", ylab = "Emotion words count (as %)", 
           main = "Distribution of emotion words count in Thrice lyrics across all albums")

# black bar = mean, white circles = outliers
# n = 11 shows the total # of albums 
# try with ggpubr?
library(ggpubr)
emotions_lyrics %>% 
  ggboxplot(x = "sentiment", y = "percent", 
            title = "EmoWOrd", palette = "jco", color = "sentiment", legend = "none") +
  yscale("percent")


##### with bing >>>> only Pos-Neg-Neut BOXPLOT ####

emotions_lyrics_bing <- wordToken2 %>% 
  filter(!grepl('[0-9]', word)) %>% 
  left_join(get_sentiments("bing"), by = "word") %>% 
  mutate(sentiment = ifelse(is.na(sentiment), 'neutral', sentiment)) %>%   # if use "bing", need to add in 'neutral' for all NOT pos/neg
  group_by(album, sentiment) %>% 
  summarize(freq = n()) %>% 
  mutate(percent = round(freq / sum(freq)*100)) %>% 
  select(-freq) %>% 
  ungroup() 

# filter(!(sentiment == "negative" | sentiment == "positive")) %>%   
# for nrc use ^

emotion_box_bing <- emotions_lyrics_bing %>% 
  spread(sentiment, percent, fill = 0) %>% 
  ungroup()

cols <- colorRampPalette(brewer.pal(7, "Set1"), alpha = T)(8)

emotion_box_bing %>% 
  select(-album) %>% 
  boxplot2(col = cols, lty = 1, shrink = 0.8, textcolor = "red", 
         xlab = "Emotion Terms", ylab = "Emotion words count (as %)", 
         main = "Distribution of emotion words count in Thrice lyrics across all albums")

# not very informative...

ggplot(emotions_lyrics_bing, 
       aes(x = album, y = percent, color = sentiment, group = sentiment)) + 
  geom_line(size = 1) + 
  geom_point(size = 3) +
  xlab("Album") + ylab("Emotion Words Count (as %)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# neutral is majority of words so can't visualize well...
# try only Pos-Neg below...


# with bing >>>> Pos.Neg words distribution BOXPLOT: ####
# now without neutral...
posneg_lyrics <- wordToken2 %>% 
  filter(!grepl('[0-9]', word)) %>% 
  left_join(get_sentiments("bing"), by = "word") %>%  
  filter((sentiment == "negative" | sentiment == "positive")) %>%    # still filter non-POS/NEG for both
  group_by(album, sentiment) %>% 
  summarize(freq = n()) %>% 
  mutate(percent = round(freq / sum(freq)*100)) %>%   # round() and *100
  select(-freq) %>% 
  ungroup() 

# make "tidy" format with spread():
posneg_box <- posneg_lyrics %>% 
  spread(sentiment, percent, fill = 0) %>% 
  ungroup()

library(gplots)
cols <- colorRampPalette(brewer.pal(7, "Set3"), alpha = T)(8)

boxplot2(posneg_box[ , c(2:3)], col = cols, lty = 1, shrink = 0.8, textcolor = "red", 
         xlab = "Positive or Negative", ylab = "PosNeg/Total (as %)", 
         main = "Distribution of Pos.Neg in Thrice lyrics across all albums")
# bing: categorize binary ONLY POS/NEG
# nrc: categorize ^num of categories POS/NEG/anger/anticipation/disgust/fear/joy/surprise/sadness/trust

# Pos.Neg over TIME (albums):
ggplot(posneg_lyrics, 
       aes(x = album, y = percent, color = sentiment, group = sentiment)) + 
  geom_line(size = 1) + 
  geom_point(size = 3) +
  xlab("Album") + ylab("Emotion Words Count (as %)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# has gotten negative over time... wonderful!




# bing = only POS-NEG-NEUT       ####
emotions_lyrics <- wordToken2 %>% 
  filter(!grepl('[0-9]', word)) %>% 
  left_join(get_sentiments("bing"), by = "word") %>% 
  mutate(sentiment = ifelse(is.na(sentiment), 'neutral', sentiment)) %>%   # if use "bing", need to add in 'neutral' for all NOT pos/neg
  filter((sentiment == "negative" | sentiment == "positive")) %>%     # filter neutral out, cuz it screws up the graph lol
  group_by(album, sentiment) %>% 
  summarize(freq = n()) %>% 
  mutate(percent = round(freq / sum(freq)*100)) %>% 
  select(-freq) %>% 
  ungroup() 

# with NRC >>> ALL sentiments ####
emotions_lyrics <- wordToken2 %>% 
  filter(!grepl('[0-9]', word)) %>% 
  left_join(get_sentiments("nrc"), by = "word") %>% 
  mutate(sentiment = ifelse(is.na(sentiment), 'neutral', sentiment)) %>%   # if use "bing", need to add in 'neutral' for all NOT pos/neg
  filter(sentiment != "neutral") %>% 
  group_by(album, sentiment) %>% 
  summarize(freq = n()) %>% 
  mutate(percent = round(freq / sum(freq)*100)) %>% 
  select(-freq) %>% 
  ungroup() 

# with all the sentiments = use nrc, not ^ code
ggplot(emotions_lyrics, aes(x = album, y = percent/100, color = sentiment, group = sentiment)) + 
  geom_line(size = 1) + 
  geom_point(size = 3) +
  scale_y_continuous(breaks = pretty_breaks(10), labels = percent_format()) +
  xlab("Album") + ylab("Proportion of Emotion Words") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank())

# Rather messy. no noticeable trends to be found...! 
# ALthough "fear" has started to creep up after AI:Fire outlier.
# also sudden dip in "anger" in AI: Earth as seen in previous plot!
 
# ONLY positive-negative over time/album year...   ####
emotions_lyrics %>% 
  filter(sentiment == "positive" | sentiment == "negative") %>% 
  ggplot(aes(album, percent/100, color = sentiment, group = sentiment)) +
  geom_line(size = 1.5) +
  geom_point(size = 3.5) +
  scale_y_continuous(breaks = pretty_breaks(10), labels = percent_format()) +
  xlab("Album") + ylab("Proportion of Emotion Words") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank())

# huge spike in positive sentiment in AI: Earth???

# WITHOUT positive-negative:
# year <- factor(c(2000, 2002, 2003, 2005, 2007.1, 2007.2, 2008.1, 2008.2, 2009, 2011, 2016), ordered = TRUE)
# album <- factor(c("Identity Crisis", "The Illusion of Safety", "The Artist In The Ambulance",
#                  "Vheissu", "The Alchemy Index Fire", "The Alchemy Index Water",
#                  "The Alchemy Index Air", "The Alchemy Index Earth", "Beggars",
#                  "Major Minor", "To Be Everywhere And To Be Nowhere"))
# levels(as.factor(df$year))
# levels(year)
# levels(album)
# levels(df$album)
# paste(levels(df$album), levels(year), sep = " ")
# jar <- emotions_lyrics %>% 
#   filter(sentiment != "positive" & sentiment != "negative") %>% 
#   mutate(album_year = paste(levels(df$album), levels(year), sep = " "))



# NO pos or neg >>> anger, anticipation, disgust, fear, joy, sadness, surprise, trust
####   COLOR EXPERIMENT

emotions_lyrics %>% 
  filter(sentiment != "positive" & sentiment != "negative") %>% 
  ggplot(aes(album, percent/100, color = sentiment, group = sentiment)) +
  geom_line(size = 1.5) +
  geom_point(size = 3.5) +
  scale_y_continuous(breaks = pretty_breaks(10), labels = percent_format()) +
  xlab("Album") + ylab("Proportion of Emotion Words") +
  ggtitle("Lyric Sentiments along Albums", subtitle = "From 2000-2016") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank()) 
  # scale_color_manual(sentiment ) #########

# 10.6.17: experiment with color schemes!

# AI: Earth spike in positive from large decrease in "anger", small increase in "joy"

# AI: Fire seems to have untrendly amount of FEAR: let's take a closer look!
nrcfear <- get_sentiments("nrc") %>% 
  filter(sentiment == "fear")

wordToken2 %>% 
  filter(album == "The Alchemy Index Fire") %>% 
  inner_join(nrcfear) %>% 
  count(word, sort = TRUE)
# FIRE being tagged as "fear" is what's mainly pushing up the trend
# fear, buried, die, gallows etc. are also in which is more in line with "fear" group


#### Avg. emotion words expressed BAR chart + error: ####
overall_mean_sd <- emotions_lyrics %>% 
  group_by(sentiment) %>% 
  summarise(overall_mean = mean(percent/100), sd = sd(percent/100))  # percent to decimals

ggplot(overall_mean_sd, 
       aes(x = reorder(sentiment, -overall_mean), y = overall_mean)) +
  geom_bar(stat = "identity", fill = "darkgreen", alpha = 0.75) + 
  geom_errorbar(aes(ymin = overall_mean - sd, ymax = overall_mean + sd), 
                width = 0.2) +
  scale_y_continuous(breaks = pretty_breaks(), labels = percent_format()) +  # label as percentages
  xlab("Emotion Terms") +
  ylab("Emotion words count (%)") +
  ggtitle("Emotion words expressed in Thrice's lyrics") + 
  theme_bw() +
  theme(panel.grid.major.x = element_line(size = 1.25)) +
  coord_flip()
# error bars showing the 1 standard deviations from the mean on either side

