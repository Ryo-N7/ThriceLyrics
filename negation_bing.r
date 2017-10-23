


```{r}

no_words <- biGrams_sep %>% 
  filter(word1 == "no") %>% 
  inner_join(get_sentiments("afinn"), by = c(word2 = "word")) %>% 
  count(word2, score, sort = TRUE) %>% 
  ungroup()

no_words %>% 
  mutate(sentiment = if_else(score > 0, "positive", "negative")) %>% 
  ggplot(aes(reorder(word2, score), score, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  scale_y_continuous(breaks = pretty_breaks()) +
  scale_fill_manual(values = c("black", "darkgreen")) +
  coord_flip() +
  theme_bw() +
  theme(axis.text = element_text(size = 12),
        panel.grid.minor.x = element_line(size = 1.1),
        panel.grid.major.x = element_line(size = 1.1)) +
  labs(x = "Word Preceded by NO", y = "Sentiment Score")

```

how about with the BING lexicon we've been using to sum up Positive/Negative?

```{r}

negated_bigrams <- biGrams_sep %>% 
filter(word1 %in% negation_words) %>% 
inner_join(get_sentiments("bing"), by = c(word2 = "word")) %>% 
count(word1, word2, sentiment, sort = TRUE) %>% 
ungroup()

negated_bigrams %>% head(10)

```

```{r}

bing <- get_sentiments("bing")

bing %>% filter(word %in% negation_words)

```

Let's add in negation words to "Bing" lexicon as negative.

```{r}

bing <- bind_rows(data_frame(word = negation_words,
                             sentiment = rep(x = "negative", length.out = length(negation_words))),
                  bing)

bing %>% filter(word %in% negation_words)

```


```{r}

biGrams_sep %>% 
  filter(word1 %in% negation_words) %>% 
  inner_join(bing, by = c(word1 = "word")) %>% 
  inner_join(bing, by = c(word2 = "word")) %>% 
  rename(sentiment_word1 = sentiment.x, sentiment_word2 = sentiment.y) %>% 
  count(word1, word2, sentiment_word1, sentiment_word2, sort = TRUE)

```

Note that this is only for word1 and word2 pairs that both are categorized by the Bing lexicon. Without categorizing word2 there are about 368 bigrams that start with the various negation words.



```{r}
negated_bigrams_full <- biGrams_sep %>% 
  filter(word1 %in% negation_words) %>% 
  inner_join(get_sentiments("bing"), by = c(word1 = "word")) %>%
  inner_join(get_sentiments("bing"), by = c(word2 = "word")) %>% 
  count(word1, word2, sentiment, sort = TRUE) %>% 
  ungroup()

negated_bigrams_full %>% head(10)

```




