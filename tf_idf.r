# tf-idf



biGrams <- df %>% 
  select(album, title, year, lyrics) %>% 
  mutate(lyrics = iconv(lyrics, to = 'latin1')) %>%                                # convert to ASCII for better separation into ngrams
  unnest_tokens(line, lyrics, token = stringr::str_split, pattern = ' <br>') %>%   # split lines on the <br> tags
  unnest_tokens(ngram, line, token = "ngrams", n = 2)

biGrams_sep <- biGrams %>% 
  separate(ngram, c("word1", "word2"), sep = "[^-'\\w]")  

biGrams_sep

# Now we have a dataframe with the bigrams separated into their individual words. Let's try counting the most common bigrams:

biGrams_sep %>% count(word1, word2, sort = TRUE) %>% head(10)

# bigrams of "in   the", "of   the", "we   are" dominate. Not very meaningful!

# Now let's filter our the stop_words from both `word1` and `word2`!
  
biGrams_sep_filtered <- biGrams_sep %>% 
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word)


biGrams_tfidf <- biGrams_sep_filtered %>%
  unite(ngram, word1, word2, sep = " ") %>% 
  count(album, ngram) %>% 
  bind_tf_idf(ngram, album, n) %>% 
  arrange(desc(tf_idf))

biGrams_tfidf


biGrams_tfidf %>% 
  group_by(album) %>% 
  top_n(5) %>% 
  ungroup() %>% 
  ggplot(aes(reorder(ngram, tf_idf), tf_idf, fill = album)) + 
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~album, ncol = 3, scales = "free") +
  coord_flip() +
  scale_fill_brewer(type = "qual", palette = "Paired")




>>> tf-idf of fire in fire-theme album...



geom_col() instead of geom_bar(stat = "identity") 
>>> basically the same thing but much better to not have to type `stat = "identity"` every single time!