



# Load wordtoken2





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
lyrics_sentiment <-  tidy_lyrics %>% 
  count(sentiment) %>% 
  spread(key = sentiment, value = n) %>% 
  mutate(sentiment_ratio = (positive - negative) / (positive + negative + neutral)) %>% 
  select(album, year, sentiment_ratio)

lyrics_sentiment %>% ggplot(aes(x = album, y = sentiment_ratio)) + 
  geom_bar(aes(fill = sentiment_ratio < 0), stat = 'identity') +
  geom_text(aes(label = album, hjust = ifelse(sentiment_ratio >= 0, -0.15, 1.15)), vjust = 0.5) +
  scale_fill_manual(guide = FALSE, values = c('#565b63', '#c40909')) +
  scale_y_percent(limits = c(-0.25, 0.05)) +
  coord_flip() +
  theme_ipsum(grid = "X") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
# needs some work........    why negative? look at HOW negative is defined.
# negative talk but with positive action with negative undertones???? 
# order DESC instead...???


# Most common pos.neg words in THrice lyrics!

word_count <- tidy_lyrics %>% 
  count(word, sentiment, sort = T) %>% 
  ungroup()

top_sentiments <-  word_count %>% 
  filter(sentiment != 'neutral') %>% 
  group_by(sentiment) %>% 
  top_n(6, wt = n) %>% 
  mutate(num = ifelse(sentiment == "negative", -n, n)) %>% 
  mutate(word = reorder(word, num)) %>% 
  select(word, sentiment, num)


ggplot(top_sentiments, aes(x = word, y = num, fill = sentiment)) +
  geom_bar(stat = 'identity') + 
  scale_fill_manual(guide = F, values = c("#af8dc3", "#7fbf7b")) +
  scale_y_continuous(limits = c(-40, 70), breaks = c(-40, -25, -10, 0, 10, 25, 40, 55, 70)) +
  labs(y = "Number of Occurrences",
       x = '',
       title = 'Lyrics Sentiment of Thrice',
       subtitle = 'Most Common Positive and Negative Words') +
  theme_ipsum(grid = "Y") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Word cloud:
tidy_lyrics %>% 
  filter(sentiment != 'neutral') %>% 
  count(word, sentiment, sort = T) %>% 
  reshape2::acast(word ~ sentiment, value.var = "n", fill = 0) %>% 
  comparison.cloud(colors = c("#af8dc3", "#7fbf7b"))


# Distribution of emotion words BOXPLOT:

emotions_lyrics <- wordToken2 %>% 
  filter(!grepl('[0-9]', word)) %>% 
  left_join(get_sentiments("nrc"), by = "word") %>% 
  filter(!(sentiment == "negative" | sentiment == "positive")) %>% 
  group_by(album, sentiment) %>% 
  summarize(freq = n()) %>% 
  mutate(percent = round(freq / sum(freq)*100)) %>% 
  select(-freq) %>% 
  ungroup() 

emotion_box <- emotions_lyrics %>% 
  spread(sentiment, percent, fill = 0) %>% 
  ungroup()

cols <- colorRampPalette(brewer.pal(7, "Set3"), alpha = T)(8)

boxplot2(emotion_box[ , c(2:9)], col = cols, lty = 1, shrink = 0.8, textcolor = "red", 
         xlab = "Emotion Terms", ylab = "Emotion words count (as %)", 
         main = "Distribution of emotion words count in Thrice lyrics across all albums")

# with bing

emotions_lyrics <- wordToken2 %>% 
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

emotion_box <- emotions_lyrics %>% 
  spread(sentiment, percent, fill = 0) %>% 
  ungroup()

cols <- colorRampPalette(brewer.pal(7, "Set3"), alpha = T)(8)

boxplot2(emotion_box[ , c(2:4)], col = cols, lty = 1, shrink = 0.8, textcolor = "red", 
         xlab = "Emotion Terms", ylab = "Emotion words count (as %)", 
         main = "Distribution of emotion words count in Thrice lyrics across all albums")



# Pos.Neg words distribution BOXPLOT:
posneg_lyrics <- wordToken2 %>% 
  filter(!grepl('[0-9]', word)) %>% 
  left_join(get_sentiments("bing"), by = "word") %>%          # larger diference when use "bing" vs. "nrc" database! MUST RESEARCH DIFFERENCES!!!!!!!!
  filter((sentiment == "negative" | sentiment == "positive")) %>%    # still filter non-POS/NEG for both
  group_by(album, sentiment) %>% 
  summarize(freq = n()) %>% 
  mutate(percent = round(freq / sum(freq)*100)) %>% 
  select(-freq) %>% 
  ungroup() 


posneg_box <- posneg_lyrics %>% 
  spread(sentiment, percent, fill = 0) %>% 
  ungroup()

boxplot2(posneg_box[ , c(2:3)], col = cols, lty = 1, shrink = 0.8, textcolor = "red", 
         xlab = "Positive or Negative", ylab = "PosNeg/Total (as %)", 
         main = "Distribution of Pos.Neg in Thrice lyrics across all albums")
# bing: categorize binary ONLY POS/NEG
# nrc: categorize ^num of categories POS/NEG/anger/anticipation/disgust/fear/joy/surprise/sadness/trust



# Sentiments Over TIME (or album in this case):
emotions_lyrics <- wordToken2 %>% 
  filter(!grepl('[0-9]', word)) %>% 
  left_join(get_sentiments("bing"), by = "word") %>% 
  mutate(sentiment = ifelse(is.na(sentiment), 'neutral', sentiment)) %>%   # if use "bing", need to add in 'neutral' for all NOT pos/neg
  filter((sentiment == "negative" | sentiment == "positive")) %>%     # filter neutral out comes it screws up the graph lol
  group_by(album, sentiment) %>% 
  summarize(freq = n()) %>% 
  mutate(percent = round(freq / sum(freq)*100)) %>% 
  select(-freq) %>% 
  ungroup() 


# with all the sentiments = use nrc, not ^ code
ggplot(emotions_lyrics, aes(x = album, y = percent, color = sentiment, group = sentiment)) + 
  geom_line(size = 1) + 
  geom_point(size = 3) +
  xlab("Album") + ylab("Emotion Words Count (as %)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Rather messy. no noticeable trends to be found...! ALthough fear has started to creep up after AI:Fire outlier.

# AI: Fire seems to have untrendly amount of FEAR: let's take a closer look!
nrcfear <- get_sentiments("nrc") %>% 
  filter(sentiment == "fear")

wordToken2 %>% 
  filter(album == "The Alchemy Index Fire") %>% 
  inner_join(nrcfear) %>% 
  count(word, sort = TRUE)


# Pos.Neg over TIME (albums):
ggplot(posneg_lyrics, aes(x = album, y = percent, color = sentiment, group = sentiment)) + 
  geom_line(size = 1) + 
  geom_point(size = 3) +
  xlab("Album") + ylab("Emotion Words Count (as %)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# LOOOOOOOOOOOOOOOOOOOOOOOOOOLLLL 


# Avg. emotion words expressed BAR chart + error:
overall_mean_sd <- emotions_lyrics %>% 
  group_by(sentiment) %>% 
  summarise(overall_mean = mean(percent), sd = sd(percent))

ggplot(overall_mean_sd, aes(x = reorder(sentiment, -overall_mean), y=overall_mean)) +
  geom_bar(stat="identity", fill="darkgreen", alpha=0.7) + 
  geom_errorbar(aes(ymin=overall_mean-sd, ymax=overall_mean+sd), width=0.2,position=position_dodge(.9)) +
  xlab("Emotion Terms") +
  ylab("Emotion words count (%)") +
  ggtitle("Emotion words expressed in Thrice's lyrics") + 
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  coord_flip( )


