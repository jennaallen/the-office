library(tidyverse)
library(tidytext)
library(ggplot2)
library(scales)
library(RColorBrewer)
library(googlesheets)
library(rlang)


# chapter 1: The tidy text format
gs_auth()
gs_user()
# get key for data sheet
sheet_key <- gs_ls("the-office-lines") %>% 
  pull(sheet_key)

# register sheet to access it
reg <- sheet_key %>%
  gs_key()

# read sheet data into R
raw_data <- reg %>%
  gs_read(ws = "scripts")

# filter out deleted scenes
# remove text in [] and put in a new column
#  there are 4000+ instances of ??? found in the data mainly in the last two seasons
# the ??? replaces ... - ' and "
# for now I'm just going to replace all instances with with ' since that seems to be the majority of the cases
# I may need to rethink this later; it won't matter if striping puntuation

# speaker is an important field in the data and there is some clean up to do. It looks like some entries for speakers have have actions []
# also there are some miss spellings micheal instead of michael
# and some unnecessary punctionation dwight:
# cleaning these up isn't a big deal because there are that many lines impacted
# tolower speaker because of inconsistent capitalization
mod_data <- raw_data %>% 
  filter(deleted == "FALSE") %>% 
  mutate(actions = str_extract_all(line_text, "\\[.*?\\]"),
         line_text_mod = str_trim(str_replace_all(line_text, "\\[.*?\\]", ""))) %>% 
  mutate_at(vars(line_text_mod), funs(str_replace_all(mod_data$line_text_mod, "���","'"))) %>% 
  mutate_at(vars(speaker), funs(tolower)) %>% 
  mutate_at(vars(speaker), funs(str_trim(str_replace_all(., "\\[.*?\\]", "")))) %>% 
  mutate_at(vars(speaker), funs(str_replace_all(., "micheal|michel|michae$", "michael")))


# searching around on the interwebs indicates that there were 201 episodes of the office. Wikipedia 
# counts some episodes like "A Benihana Christmas" as two, which I am not sure why
# the data from officequotes.net pretty much lines up with IMdB with the exception of season 6 because
# officequotes.net counts niagra parts 1 & 2 as one episode and the delivery parts 1 & 2 as one episode instead of two
# going with the idea that there were 186 episodes total
# proportion of episodes each character was in
total_episodes <- mod_data %>% 
  unite(season_ep, season, episode, remove = FALSE) %>% 
  summarise(num_episodes = n_distinct(season_ep)) %>% 
  as.integer()
  
episode_proportion <- mod_data %>% 
  unite(season_ep, season, episode, remove = FALSE) %>% 
  group_by(speaker) %>% 
  summarise(num_episodes = n_distinct(season_ep)) %>% 
  mutate(proportion = round((num_episodes / total_episodes) * 100, 1)) %>% 
  arrange(desc(num_episodes))

total_scenes <- mod_data %>% 
  unite(season_ep_scene, season, episode, scene, remove = FALSE) %>% 
  summarise(num_scenes = n_distinct(season_ep_scene)) %>% 
  as.integer()

# proportion of scenes each character was in 
scene_proportion <- mod_data %>% 
  unite(season_ep_scene, season, episode, scene, remove = FALSE) %>% 
  group_by(speaker) %>% 
  summarise(num_scenes = n_distinct(season_ep_scene)) %>% 
  mutate(proportion = round((num_scenes / total_scenes) * 100, 1)) %>% 
  arrange(desc(num_scenes))
  
# which characters had the most lines
# this is for all lines and episodes; not every character was in every episode/season

line_proportion <- mod_data %>% 
  count(speaker) %>% 
  mutate(proportion = round((n / sum(n)) * 100, 1)) %>% 
  arrange(desc(n))

line_proportion_by_season <- mod_data %>% 
  group_by(season) %>% 
  count(speaker) %>% 
  mutate(proportion = round((n / sum(n)) * 100, 1)) %>% 
  arrange(season, desc(proportion))

line_proportion_over_time_main <- line_proportion_by_season %>% 
  filter(speaker %in% main_characters[1:10]) %>% 
  mutate_at(vars(speaker), factor, levels = main_characters) %>% 
  ggplot(aes(x = season, y = proportion, color = speaker)) +
  geom_point(size = 2) +
  geom_line() +
  scale_x_continuous(breaks = seq(1, 9, 1)) +
  theme_minimal() +
  #scale_color_brewer(palette = "PRGn") +
  facet_wrap(~ speaker, ncol = 3)

line_proportion_over_time_secondary <- line_proportion_by_season %>% 
  filter(speaker %in% main_characters[11:21]) %>% 
  mutate_at(vars(speaker), factor, levels = main_characters) %>% 
  ggplot(aes(x = season, y = proportion, color = speaker)) +
  geom_point(size = 2) +
  geom_line() +
  scale_x_continuous(breaks = seq(1, 9, 1)) +
  theme_minimal() +
  #scale_color_brewer(palette = "Spectral") +
  facet_wrap(~ speaker, ncol = 3)

# display.brewer.all()
# brewer.pal(10, "PRGn")
fct_inorder(main_characters)
 levels(main_characters)
# tokenize lines and remove stop words

tidy_tokens <- mod_data %>%
  select(line = id, line_text_mod, everything(), -line_text, -actions, -deleted) %>% 
  unnest_tokens(word, line_text_mod, strip_numeric = TRUE) %>%
  anti_join(stop_words)


# plot absolute word counts for all lines
top_50_word_freq <- tidy_tokens %>%
  count(word, sort = TRUE) %>% 
  mutate(proportion = round(n / sum(n), 3)) %>%
  top_n(50, proportion) %>%
  mutate(word = reorder(word, proportion)) %>%
  ggplot(aes(word, percent(proportion))) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  theme_minimal()


# defining main characters based on line proportion
main_characters <- factor(line_proportion %>% 
                            top_n(21, n) %>% 
                            pull(speaker) %>% 
                            fct_inorder()
                          )

reorder_within <- function(x, by, within, fun = mean, sep = "___", ...) {
  new_x <- paste(x, within, sep = sep)
  stats::reorder(new_x, by, FUN = fun)
}

scale_x_reordered <- function(..., sep = "___") {
  reg <- paste0(sep, ".+$")
  ggplot2::scale_x_discrete(labels = function(x) gsub(reg, "", x), ...)
}

scale_y_reordered <- function(..., sep = "___") {
  reg <- paste0(sep, ".+$")
  ggplot2::scale_y_discrete(labels = function(x) gsub(reg, "", x), ...)
}

# plot top 10 absolute word counts by main character
top_10_word_freq_character <- tidy_tokens %>%
  filter(speaker %in% main_characters) %>%
  count(speaker, word, sort = TRUE) %>% 
  group_by(speaker) %>% 
  mutate(proportion = round(n / sum(n), 3)) %>%
  top_n(10, proportion) %>% 
  ggplot(aes(reorder_within(word, proportion, speaker), percent(proportion), fill = speaker)) +
  geom_col() +
  scale_x_reordered() +
  xlab(NULL) +
  coord_flip() +
  theme_minimal() + 
  facet_wrap(~ factor(speaker, levels = main_characters), scales = "free") + 
  theme(legend.position = "none")

frequency_by_character_list <- setNames(map(as.character(main_characters), ~ tidy_tokens %>%
                                              filter(speaker %in% main_characters) %>% 
                                              count(speaker, word, sort = TRUE) %>% 
                                              group_by(speaker) %>% 
                                              mutate(proportion = n / sum(n)) %>% 
                                              select(-n) %>% 
                                              spread(speaker, proportion) %>% 
                                              gather(speaker, proportion, -.x, -word)), as.character(main_characters))


correlations_michael <- setNames(map(as.character(main_characters[-1]), ~
                                                       cor.test(data = as_tibble(frequency_by_character_list[["michael"]]) %>%
                                                                  filter(speaker == .x),
                                                               ~ proportion + michael)), as.character(main_characters[-1]))

df_cor_michael <- correlations_michael %>% 
  map_df(tidy) %>% 
  mutate(character = names(correlations_michael),
         comparison = map_chr(correlations_michael, 8)) %>% 
  arrange(desc(estimate)) %>% 
  select(character, everything())


correlations_jim <- setNames(map(as.character(main_characters[-3]), ~
                                   cor.test(data = as_tibble(frequency_by_character_list[["jim"]]) %>%
                                              filter(speaker == .x),
                                            ~ proportion + jim)), as.character(main_characters[-3]))

df_cor_jim <- correlations_jim %>% 
  map_df(tidy) %>% 
  mutate(character = names(correlations_jim),
         comparison = map_chr(correlations_jim, 8)) %>% 
  arrange(desc(estimate)) %>% 
  select(character, everything())

correlations_pam <- setNames(map(as.character(main_characters[-4]), ~
                                   cor.test(data = as_tibble(frequency_by_character_list[["pam"]]) %>%
                                              filter(speaker == .x),
                                            ~ proportion + pam)), as.character(main_characters[-4]))
df_cor_pam <- correlations_pam %>% 
  map_df(tidy) %>% 
  mutate(character = names(correlations_pam),
         comparison = map_chr(correlations_pam, 8)) %>% 
  arrange(desc(estimate)) %>% 
  select(character, everything())

correlations_dwight <- setNames(map(as.character(main_characters[-2]), ~
                                   cor.test(data = as_tibble(frequency_by_character_list[["dwight"]]) %>%
                                              filter(speaker == .x),
                                            ~ proportion + dwight)), as.character(main_characters[-2]))
df_cor_dwight <- correlations_dwight %>% 
  map_df(tidy) %>% 
  mutate(character = names(correlations_dwight),
         comparison = map_chr(correlations_dwight, 8)) %>% 
  arrange(desc(estimate)) %>% 
  select(character, everything())


correlations <- setNames(map(as.character(main_characters[-1]), ~
  cor.test(data = frequency_by_character %>%
             filter(speaker == .x),
                         ~ proportion + michael)), as.character(main_characters[-1]))


correlations %>% 
  map_df(tidy) %>% 
  mutate(character = names(correlations),
         comparison = map_chr(correlations, 8))



# there is a way to do this will all combination of characters, but I haven't figured it out yet
frequency_by_character_list <- setNames(map(as.character(main_characters), ~ tidy_tokens %>%
  filter(speaker %in% main_characters) %>% 
  count(speaker, word, sort = TRUE) %>% 
  group_by(speaker) %>% 
  mutate(proportion = n / sum(n)) %>% 
  select(-n) %>% 
  spread(speaker, proportion) %>% 
  gather(speaker, proportion, -.x, -word)), as.character(main_characters))

correlations_list <- for (i in seq_along(frequency_by_character_list)) {
  characters <- as.character(main_characters[-i])
  tibble <- as_tibble(frequency_by_character_list[[i]])
  #compare <- names(tibble)[2]
  #compare <- quo(!! names(tibble)[2])
  setNames(map(characters, ~
                 cor.test(data = tibble %>%
                            filter(speaker == .x),
                          ~ 4 + 2)), characters)
}

tibble %>% 
  filter(speaker == .x) %>% 
  cor.test(tibble[2], tibble[4])

cor.test(data = tibble %>%
           filter(speaker == .x),
         ~ proportion + compare))


frequency_by_character <- tidy_tokens %>%
  filter(speaker %in% main_characters) %>% 
  count(speaker, word, sort = TRUE) %>% 
  group_by(speaker) %>% 
  mutate(proportion = n / sum(n)) %>% 
  select(-n) %>% 
  spread(speaker, proportion) %>% 
  gather(speaker, proportion, -michael, -word)
