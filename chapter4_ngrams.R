# chapter 4 bigrams

tidy_bigrams <- mod_data %>%
  select(line = id, line_text_mod, everything(), -line_text, -actions, -deleted) %>% 
  unnest_tokens(bigram, line_text_mod, token = "ngrams", n = 2)

# get NAs when there is only 1 word in the line
# is this ok or do i need to rethink it
tidy_bigrams %>%
  filter(speaker %in% main_characters, !is.na(bigram)) %>% 
  count(speaker, bigram, sort = TRUE)

tidy_bigrams_no_stop <- tidy_bigrams %>% 
  filter(!is.na(bigram)) %>% 
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word) %>%
  count(word1, word2, sort = TRUE)

bigram_tf_idf <- tidy_bigrams %>%
  filter(speaker %in% main_characters, !is.na(bigram)) %>%
  count(speaker, bigram) %>%
  bind_tf_idf(bigram, speaker, n) %>%
  arrange(desc(tf_idf))

bigram_tf_idf_no_stop <- tidy_bigrams %>% 
  filter(!is.na(bigram)) %>% 
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word) %>% 
  unite(bigram, word1, word2, sep = " ") %>% 
  count(speaker, bigram) %>%
  bind_tf_idf(bigram, speaker, n) %>%
  arrange(desc(tf_idf))

bigram_tf_idf %>%
  #arrange(desc(tf_idf)) %>%
  #mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  mutate_at(vars(speaker), factor, levels = main_characters) %>%
  group_by(speaker) %>% 
  top_n(10) %>% 
  ungroup %>%
  ggplot(aes(reorder_within(bigram, tf_idf, speaker), tf_idf, fill = speaker)) +
  geom_col(show.legend = FALSE) +
  scale_x_reordered() +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~ speaker, scales = "free") +
  coord_flip()

bigram_tf_idf_no_stop %>%
  #arrange(desc(tf_idf)) %>%
  #mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  mutate_at(vars(speaker), factor, levels = main_characters) %>%
  group_by(speaker) %>% 
  top_n(10) %>% 
  ungroup %>%
  ggplot(aes(reorder_within(bigram, tf_idf, speaker), tf_idf, fill = speaker)) +
  geom_col(show.legend = FALSE) +
  scale_x_reordered() +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~ speaker, scales = "free") +
  coord_flip()