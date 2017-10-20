# compare Alchemy Index albums

# separate and combine 

# analyze






# Comparison Alchemy Index albums! ----------------------------------------

# Fire vs. Water
# Earth vs. Air

alchemy_index <- wordToken2 %>% 
  filter(str_detect(album, "The Alchemy Index")) %>% 
  select(title, album, year, word)

alchemy_index %>% 
  left_join(get_sentiments("nrc"), "word") %>% 
  mutate(sentiment = ifelse(is.na(sentiment), 'neutral', sentiment))

# to not clutter up global environment...
tidy_lyrics %>% 
  filter(str_detect(album, "The Alchemy Index")) %>% 
  select(title, album, year, word) %>% 
  left_join(get_sentiments("nrc"), "word") %>% 
  mutate(sentiment = ifelse(is.na(sentiment), 'neutral', sentiment)) %>% 
  filter(sentiment != "neutral") %>% 
  group_by(album, sentiment) %>% 
  summarize(freq = n()) %>% 
  mutate(percent = round(freq / sum(freq)*100)) %>% 
  select(-freq) %>% 
  ungroup() %>% 
  head(20)

# with neutral
tidy_lyrics %>% 
  filter(str_detect(album, "The Alchemy Index")) %>% 
  select(title, album, year, word) %>% 
  left_join(get_sentiments("nrc"), "word") %>% 
  mutate(sentiment = ifelse(is.na(sentiment), 'neutral', sentiment)) %>% 
  group_by(album, sentiment) %>% 
  summarize(freq = n()) %>% 
  mutate(percent = round(freq / sum(freq)*100)) %>% 
  select(-freq) %>% 
  ungroup() %>% 
  filter(sentiment != "neutral") %>% 
  filter(sentiment != "positive") %>% 
  filter(sentiment != "negative") %>% 
  ggplot(aes(x = album, y = percent/100, color = sentiment, group = sentiment)) + 
  geom_line(size = 1) + 
  geom_point(size = 3) +
  scale_y_continuous(breaks = pretty_breaks(10), labels = percent_format()) +
  xlab("Album") + ylab("Proportion of Emotion Words") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank())
# not informative... no timeline order of albums Fire or Water first? 
# ordering changes trends dramatically




