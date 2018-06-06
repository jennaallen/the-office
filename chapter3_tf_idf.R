# chapter 3: tf-idf

# term frequency

tidy_tokens2 <- mod_data %>%
  select(line = id, line_text_mod, everything(), -line_text, -actions, -deleted) %>% 
  unnest_tokens(word, line_text_mod) %>% 
  count(speaker, word, sort = TRUE) %>%
  ungroup()
  

total_words <- tidy_tokens2 %>% 
  group_by(speaker) %>% 
  summarize(total = sum(n))

tidy_tokens2 <- left_join(tidy_tokens2, total_words)

# term frequency plot by top 10 characters
tidy_tokens2 %>% 
  filter(speaker %in% main_characters[1:10]) %>% 
  ggplot(aes(n/total, fill = speaker)) +
  geom_histogram(show.legend = FALSE) +
  #xlim(NA, 0.0009) +
  facet_wrap(~ factor(speaker, levels = main_characters), ncol = 2, scales = "free_y")

freq_by_rank <- tidy_tokens2 %>% 
  group_by(speaker) %>% 
  mutate(rank = row_number(), 
         term_frequency = n/total)

freq_by_rank %>% 
  filter(speaker %in% main_characters) %>% 
  ggplot(aes(rank, term_frequency, color = speaker)) + 
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
  scale_x_log10() +
  scale_y_log10()


rank_subset <- freq_by_rank %>% 
  filter(rank < 500,
         rank > 10)

lm(log10(term_frequency) ~ log10(rank), data = rank_subset)

# The deviations we see here at high rank are not uncommon for many kinds of language; 
# a corpus of language often contains fewer rare words than predicted by a single power law.
# I think this means the office uses a higher percentage of the most common words 
# than many collections of language.
freq_by_rank %>% 
  filter(speaker %in% main_characters) %>% 
  ggplot(aes(rank, term_frequency, color = speaker)) + 
  geom_abline(intercept = -0.82, slope = -1.3, color = "gray50", linetype = 2) +
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
  scale_x_log10() +
  scale_y_log10()

# Calculating tf-idf attempts to find the words that are important (i.e., common) in a text,
# but not too common.

# if you don't filter for main characters here you get that common words are more important than 
# they should be because it is comparing words for all characters (even ones with few lines)

# The inverse document frequency (and thus tf-idf) is very low (near zero) for words that occur in many of the 
# documents in a collection
# The inverse document frequency will be a higher number for words that occur in fewer of the
# documents(speakers in this case) in the collection.
tidy_tokens2 <- tidy_tokens2 %>%
  filter(speaker %in% main_characters) %>% 
  bind_tf_idf(word, speaker, n)

tidy_tokens2 %>%
  select(-total) %>%
  arrange(desc(tf_idf))

# winner of chart right here
tidy_tokens2 %>%
  arrange(desc(tf_idf)) %>%
  #mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  mutate_at(vars(speaker), factor, levels = main_characters) %>%
  group_by(speaker) %>% 
  top_n(10) %>% 
  ungroup %>%
  ggplot(aes(reorder_within(word, tf_idf, speaker), tf_idf, fill = speaker)) +
  geom_col(show.legend = FALSE) +
  scale_x_reordered() +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~ speaker, scales = "free") +
  coord_flip()

# we see la quite a bit, wonder what that is all about
# also m&m's should be one word and costa rica

la <- mod_data %>% 
  filter(str_detect(line_text_mod, "\\bla\\b")) %>% 
  select(speaker, line_text_mod)