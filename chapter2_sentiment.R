library(wordcloud)

# chapter 2: sentiment analysis

#A text the size of many paragraphs can often have positive and negative sentiment averaged out to 
#about zero, while sentence-sized or paragraph-sized text often works better.

nrc_sentiment <- tidy_tokens %>% 
  inner_join(get_sentiments("nrc"))

nrc_sentiment %>% 
  group_by(sentiment, season) %>% 
  count(sort =  TRUE)

nrc_sentiment %>% 
  filter(speaker == "michael") %>% 
  group_by(sentiment) %>% 
  count(sort =  TRUE)

nrc_sentiment %>% 
  filter(speaker == "jim") %>% 
  group_by(sentiment) %>% 
  count(sort =  TRUE)

nrc_sentiment %>% 
  filter(speaker == "dwight") %>% 
  group_by(sentiment) %>% 
  count(sort =  TRUE)

nrc_sentiment %>% 
  filter(speaker == "michael", sentiment == "positive") %>% 
  count(word, sort = TRUE)

nrc_sentiment %>% 
  filter(speaker == "michael", sentiment == "negative") %>% 
  count(word, sort = TRUE)


the_office_sentiment <- tidy_tokens %>%
  inner_join(get_sentiments("bing")) %>%
  count(season, episode, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

ggplot(the_office_sentiment, aes(episode, sentiment, fill = season)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ season, ncol = 2, scales = "free_x")

# comparing lexicons for the office

afinn <- tidy_tokens %>% 
  inner_join(get_sentiments("afinn")) %>% 
  group_by(season, episode) %>% 
  summarise(sentiment = sum(score)) %>% 
  mutate(method = "AFINN")

bing_and_nrc <- bind_rows(tidy_tokens %>% 
                            inner_join(get_sentiments("bing")) %>%
                            mutate(method = "Bing et al."),
                          tidy_tokens %>% 
                            inner_join(get_sentiments("nrc") %>% 
                                         filter(sentiment %in% c("positive", 
                                                                 "negative"))) %>%
                            mutate(method = "NRC")) %>%
  count(method, season, episode, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

#the NRC sentiment is high, the AFINN sentiment has more variance, the Bing et al. sentiment 
# appears to find longer stretches of similar text

# the NRC and Bing lexicons have more negative than positive words, but the ratio of negative to 
# positive words is higher in the Bing lexicon than the NRC lexicon.

lexicon_comparison <- bind_rows(afinn, 
          bing_and_nrc) %>%
  ggplot(aes(episode, sentiment, fill = method)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(c("method", "season"), ncol = 9, scales = "free")


bing_word_counts <- tidy_tokens %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()


bing_word_counts_by_season <- tidy_tokens %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, season, sentiment, sort = TRUE) %>%
  ungroup()

# interesting that funny and joke are negative words

bing_word_counts_by_season %>%
  group_by(season, sentiment) %>%
  top_n(10, n) %>%
  #ungroup() %>%
  #mutate(word = reorder(word, n)) %>%
  ggplot(aes(reorder_within(word, n, season), n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  scale_x_reordered() +
  facet_wrap(c("season", "sentiment"), scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()


pal <- brewer.pal(11,"PRGn")
tidy_tokens %>%
  count(word, sort = TRUE) %>%
  with(wordcloud(word, n, scale = c(2, .5), max.words = 100, colors = pal))

library(reshape2)

tidy_tokens %>%
  inner_join(get_sentiments("bing")) %>%
  filter(speaker == "pam") %>% 
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray80"),
                   max.words = 100)

# which seasons were the most negative/positive

bingnegative <- get_sentiments("bing") %>% 
  filter(sentiment == "negative")

bingpositive <- get_sentiments("bing") %>% 
  filter(sentiment == "positive")

wordcounts <- tidy_tokens %>%
  group_by(season) %>%
  summarize(words = n())

# what does this semi join do?
tidy_tokens %>%
  inner_join(bingnegative) %>%
  group_by(season) %>%
  summarize(negativewords = n()) %>%
  left_join(wordcounts, by = "season") %>%
  mutate(ratio = negativewords/words) #%>%
  #filter(chapter != 0) %>%
  #top_n(1)

# season 3 is the most negative season

tidy_tokens %>%
  inner_join(bingpositive) %>%
  group_by(season) %>%
  summarize(positivewords = n()) %>%
  left_join(wordcounts, by = "season") %>%
  mutate(ratio = positivewords/words)

# season 1 is the most positive

# which character is the most positive
wordcounts_speaker <- tidy_tokens %>%
  group_by(speaker) %>%
  summarize(words = n())

tidy_tokens %>%
  filter(speaker %in% main_characters) %>% 
  inner_join(bingnegative) %>%
  group_by(speaker) %>%
  summarize(negativewords = n()) %>%
  left_join(wordcounts_speaker, by = "speaker") %>%
  mutate(ratio = negativewords/words) %>% 
  arrange(desc(ratio))
# kelly is the most negative

tidy_tokens %>%
  filter(speaker %in% main_characters) %>% 
  inner_join(bingpositive) %>%
  group_by(speaker) %>%
  summarize(positivewords = n()) %>%
  left_join(wordcounts_speaker, by = "speaker") %>%
  mutate(ratio = positivewords/words) %>% 
  arrange(desc(ratio))

# holly is the most positive

# most negative episode

wordcounts_episode <- tidy_tokens %>%
  group_by(season, episode) %>%
  summarize(words = n())

tidy_tokens %>%
  inner_join(bingnegative) %>%
  group_by(season, episode) %>%
  summarize(negativewords = n()) %>%
  left_join(wordcounts_episode, by = c("season", "episode")) %>%
  mutate(ratio = negativewords/words) %>% 
  arrange(desc(ratio))

tidy_tokens %>%
  inner_join(bingpositive) %>%
  group_by(season, episode) %>%
  summarize(positivewords = n()) %>%
  left_join(wordcounts_episode, by = c("season", "episode")) %>%
  mutate(ratio = positivewords/words) %>% 
  arrange(desc(ratio))