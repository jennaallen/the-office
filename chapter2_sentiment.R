library(wordcloud)
library(data.table)
library(reshape2)

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

tidy_tokens %>%
  inner_join(get_sentiments("bing")) %>%
  filter(speaker == "michael") %>% 
  count(season, sentiment) %>%
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

# how does character sentiment change over season
tidy_tokens %>%
  inner_join(get_sentiments("bing")) %>%
  filter(speaker %in% main_characters) %>%
  mutate_at(vars(speaker), factor, levels = main_characters) %>%
  count(season, speaker, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative) %>% 
  ggplot(aes(x = season, y = sentiment, color = speaker)) +
  geom_point() +
  geom_line() +
  facet_wrap(~ speaker) +
  scale_x_continuous(breaks = seq(1, 9, 1))

# sentiment by episode for each season; michael has the largest range of sentiment over time
tidy_tokens %>%
  inner_join(get_sentiments("bing")) %>%
  filter(speaker %in% main_characters) %>%
  mutate_at(vars(speaker), factor, levels = main_characters) %>%
  count(season, episode, speaker, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative) %>% 
  ggplot(aes(x = as.character(season), y = sentiment, color = speaker)) +
  geom_boxplot() +
  facet_wrap(c("speaker")) +
  scale_x_continuous(breaks = seq(1, 9, 1))
  
median(c(-18, 5, 8, -9, -1, 12))

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

# parse by sentence instead of word

tidy_sentences <- mod_data %>%
  select(line = id, line_text_mod, everything(), -line_text, -actions, -deleted) %>% 
  unnest_tokens(sentence, line_text_mod, token = "sentences")

# how many times was "that's what she said" said

thats_what_she_said <- tidy_sentences %>% 
  group_by(speaker) %>% 
  filter(str_detect(sentence, "that(')?s what she( said)?(?! would)")) %>% 
  count(sort = TRUE) 

tidy_sentences %>% 
  group_by(season) %>% 
  filter(str_detect(sentence, "that(')?s what she( said)?(?! would)")) %>% 
  count() 

# get sentence before that's what she said
tidy_sentences <- rowid_to_column(tidy_sentences, var = "sentence_id")

  twss_sentence <- tidy_sentences %>% 
    filter(sentence %like% "^that(')?s what she( said)?") %>% 
    pull(sentence_id)
  
  twss_line <- tidy_sentences %>% 
    filter(sentence %like% "^that(')?s what she( said)?") %>% 
    pull(line)
  
  line_before <- twss_line - 1
  
  sentence_before <- twss_sentence - 1
  
  sentence_before_twss <- tidy_sentences %>% 
    filter(sentence_id %in% sentence_before) 
  
  line_before_twss <- tidy_sentences %>% 
    filter(line %in% line_before)
  
# one of these is [dwight putting grapes in his mouth] 11981 and another is second cindy whispering in michael's ear 20816 and Mr. Schneider: And you were directly under her the entire time? Mr. Scott: 
  # and I'm not saying it won't be hard. But we can make it work.
  # And in the future, if I want to say something funny or witty or do an impression, I will no longer, ever, do any of those things.
  # Jim: Does that include 'That's What She Said'?
  # I can't stay on top of you 24/7 - does he say it here?
  # Hold it in your mouth if you can't swallow weight loss - did he say it?
  # 46655 that's what he said
  # I can't force you to go down but I can entice you cafe disco did he say it
correct_sentence_before_twss <- c(4235, 4337, 4338, 4339, 8305, 9832, 13965, 15298, 17610, 22176, 24100, 24225, 26023, 28165, 30242, 32684, 34659, 34665, 34917, 34919, 34932, 35673, 36960, 40069, 41485, 41998, 42864, 46655, 48741, 63636, 71371, 74304, 75904, 75905, 78720, 96061, 106875)
almost_sentence_before_twss <- c(4238, 38566, 54538)

final_sentence_before_twss <- tidy_sentences %>% 
  filter(sentence_id %in% correct_sentence_before_twss)

final_almost_sentence_before_twss <- tidy_sentences %>% 
  filter(sentence_id %in% almost_sentence_before_twss)
c(correct_sentence_before_twss, almost_sentence_before_twss)
# that's what she said was said 39 times. One of those times includes jim asking a question about it, so not really in the spirit of twss
# another of those times inclue the court reporter reading back what michael had said
# it was almost said 3 more times by michael but either he cut himself off or jim cut him off
all_sentences_before_twss <- tidy_sentences %>% 
  filter(sentence_id %in% c(correct_sentence_before_twss, almost_sentence_before_twss))

# sentiment by sentence using coreNLP backend