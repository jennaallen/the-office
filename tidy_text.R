library(tidyverse)
library(tidytext)
library(ggplot2)
library(googlesheets)

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
mod_data <- raw_data %>% 
  filter(deleted == "FALSE") %>% 
  mutate(actions = str_extract_all(line_text, "\\[.*?\\]"),
         line_text_mod = str_replace_all(line_text, "\\[.*?\\]", ""))

#  there are 4000+ instances of ??? found in the data mainly in the last two seasons
# the ??? replaces ... - ' and "
# for now I'm just going to replace all instances with with ' since that seems to be the majority of the cases
# I may need to rethink this later
mod_data <- mod_data %>% 
  mutate_at(vars(line_text_mod), funs(str_replace_all(mod_data$line_text_mod, "���","'")))


# tokenize lines and remove stop words

tidy_tokens <- mod_data %>%
  select(line = id, line_text_mod, everything(), -line_text, -actions, -deleted) %>% 
  unnest_tokens(word, line_text_mod) %>%
  anti_join(stop_words)


tidy_tokens %>%
  count(word, sort = TRUE) %>% 
  filter(n > 600) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

main_characters <- c("michael", "dwight", "jim", "pam", "andy", "karen", "toby", "ryan", "kelly", "jan", "angela")

reorder_within <- function(x, by, within, fun = mean, sep = "___", ...) {
  new_x <- paste(x, within, sep = sep)
  stats::reorder(new_x, by, FUN = fun)
}

scale_x_reordered <- function(..., sep = "___") {
  reg <- paste0(sep, ".+$")
  ggplot2::scale_x_discrete(labels = function(x) gsub(reg, "", x), ...)
}


test <- tidy_tokens %>%
  mutate(speaker = tolower(speaker)) %>% 
  filter(speaker %in% main_characters) %>%
  mutate_if(is.character, as.factor) %>% 
  count(speaker, word, sort = TRUE) %>% 
  group_by(speaker) %>% 
  top_n(10, n) %>% 
  ggplot(aes(reorder_within(word, n, speaker), n)) +
  geom_col() +
  scale_x_reordered() +
  xlab(NULL) +
  coord_flip() +
  facet_wrap(~ speaker, scales = "free")

