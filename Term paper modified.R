setwd("C:/Users/Wanshu/Desktop")
library(tidyverse)
library(tidytext)
theme_set(theme_bw())
list.files()
read_csv("https://raw.githubusercontent.com/phon-dicts-project/comparative_andic_dictionary_database/master/andic_dicts.csv", 
                  col_types = cols(ipa = col_character(),
                                   concepticon = col_character(),
                                   borrowing_source_word = col_character(),
                                   borrowing_source_comment = col_character()))%>%
  filter(glottocode != "toki1238") ->
  andic

glimpse(andic)

andic %>%
  filter(is.na(bor)) %>% 
  mutate(glottocode = ifelse(glottocode == "botl1242", str_c(glottocode, " ", reference), glottocode)) %>% 
  distinct(ipa, glottocode) %>% 
  mutate(id = 1:n()) %>% 
  unnest_tokens(output = "segment", input = ipa, token = stringr::str_split, pattern = "-", drop = FALSE) %>% 
  filter(!is.na(segment)) ->
  unnested_andic
glimpse(unnested_andic)

#------nasalization--------------------------
nasalisation <- "̃"

unnested_andic %>% 
  # THIS CAUSE MULTIPLE TRUBLES!!! (30.09.2021)
  #distinct(ipa, glottocode, segment) %>% # remove repetitions
  filter(str_detect(segment, "[aoiue]")) %>%
  mutate(segment2 = case_when(
    str_detect(segment, "[ie]") ~ "front",
    str_detect(segment, "a") ~ "mid",
    str_detect(segment, "[uo]") ~ "back")) ->
  unnested_andic_featured
glimpse(unnested_andic_featured)

unnested_andic_featured %>% 
  count(glottocode, segment2) %>% 
  group_by(glottocode) %>% 
  mutate(overall = sum(n),
         ratio = n/overall) ->
  unnested_andic_featured_frequencies
glimpse(unnested_andic_featured_frequencies)

unnested_andic_featured_frequencies %>% 
  select(-n, -overall) %>% 
  pivot_wider(names_from = glottocode, values_from = ratio)

unnested_andic_featured_frequencies %>% 
  group_by(segment2) %>%  # this and next 3 lines are for ordering segments
  mutate(mean_ratio = mean(ratio, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(segment2 = fct_reorder(segment2, mean_ratio)) %>% 
  top_n(100) %>% 
  ggplot(aes(ratio, segment2, color = glottocode))+
  geom_point()

unnested_andic_featured_frequencies %>% 
  mutate(segment2 = reorder_within(segment2, ratio, glottocode)) %>% 
  group_by(glottocode) %>% 
  top_n(25) %>% 
  ggplot(aes(ratio, segment2))+
  geom_point()+
  facet_wrap(~glottocode, scales = "free_y")+
  scale_y_reordered()

unnested_andic_featured %>%
  select(-segment) %>% 
  group_by(glottocode, ipa) %>%
  nest(data = segment2) %>% 
  mutate(data = map(data, unlist),
         data = map(data, table),
         data2 = map(data, names),
         data2 = map(data2, unlist),
         data = map(data, function(x) x[x > 1]),
         data = map(data, names),
         data3 = map2(data, data2, function(x, y){c(unlist(x), unlist(y))}),
         data3 = map(data3, sort),
         length = map_dbl(data3, length)) %>% 
  filter(length > 1) %>% 
  mutate(pairs = map(data3, combn, m = 2, FUN = str_c, collapse = " "),
         pairs = map(pairs, unique)) %>% 
  unnest_longer(pairs) %>% 
  ungroup() %>% 
  count(glottocode, pairs, sort = TRUE) %>% 
  separate(pairs, into = c("segment_1", "segment_2"), sep = " ", remove = FALSE) ->
  pairs

glimpse(pairs)

pairs %>% 
  group_by(glottocode) %>% 
  mutate(overall = sum(n),
         ratio = n/overall) %>% 
  top_n(20) %>% 
  mutate(pairs = reorder_within(pairs, ratio, glottocode)) %>% 
  ggplot(aes(ratio, pairs))+
  geom_point()+
  facet_wrap(~glottocode, scales = "free")+
  scale_y_reordered()

andic %>%
  filter(is.na(bor)) %>% 
  mutate(glottocode = ifelse(glottocode == "botl1242", str_c(glottocode, " ", reference), glottocode)) %>% 
  distinct(ipa, glottocode) %>% 
  mutate(id = 1:n(),
         ipa = str_c(ipa, "-#")) %>% 
  unnest_tokens(output = "segment", input = ipa, token = stringr::str_split, pattern = "-", drop = FALSE) %>% 
  filter(!is.na(segment)) %>% 
  filter(str_detect(segment, "[aoiue\\#]")) %>%
  mutate(segment = case_when(
    str_detect(segment, nasalisation) ~ "nasalized",
    str_detect(segment, "\\#") ~ "#",
    (!str_detect(segment, nasalisation) & !str_detect(segment, "\\#")) ~ "nonnasalized")) %>% 
  mutate(next_segment = lead(segment)) %>% 
  filter(segment != "#",
         next_segment != "#") %>% 
  count(glottocode, segment, next_segment) ->
  unnested_andic_featured_with_hash

unnested_andic_featured_with_hash %>% 
  mutate(pairs = str_c(segment, " ", next_segment)) %>% 
  group_by(glottocode) %>% 
  mutate(overall = sum(n),
         ratio = n/overall) %>% 
  top_n(20) %>% 
  mutate(pairs = reorder_within(pairs, ratio, glottocode)) %>% 
  ggplot(aes(ratio, pairs))+
  geom_point()+
  facet_wrap(~glottocode, scales = "free")+
  scale_y_reordered()

unnested_andic_featured %>% 
  count(glottocode, segment2) %>% 
  rename(raw_n = n,
         segment = segment2) %>% 
  group_by(glottocode) %>% 
  mutate(frequency = raw_n/sum(raw_n)) %>% 
  select(-raw_n) ->
  pure_counts

pairs %>% 
  rename(segment = segment_1,
         next_segment = segment_2) %>% 
  full_join(pure_counts) %>% 
  rename(f1 = frequency) %>% 
  full_join(pure_counts, by = c("glottocode" = "glottocode", "next_segment" = "segment")) %>% 
  rename(f2 = frequency) %>% 
  group_by(glottocode) %>% 
  mutate(observed_probability = n/sum(n, na.rm = TRUE),
         multiply = segment != next_segment,
         expected_probability = f1*f2,
         expected_probability = ifelse(multiply, expected_probability*2, expected_probability),
         method = "bag of words") %>% 
  select(glottocode, pairs, observed_probability, expected_probability, method) ->
  probabilities_bag_of_words
glimpse(probabilities_bag_of_words)

unnested_andic_featured_with_hash %>% 
  full_join(pure_counts) %>% 
  rename(f1 = frequency) %>% 
  full_join(pure_counts, by = c("glottocode" = "glottocode", "next_segment" = "segment")) %>% 
  rename(f2 = frequency) %>% 
  group_by(glottocode) %>% 
  mutate(observed_probability = n/sum(n, na.rm = TRUE),
         expected_probability = f1*f2,
         method = "markov chain",
         pairs = str_c(segment, " ", next_segment)) %>% 
  select(glottocode, pairs, observed_probability, expected_probability, method) ->
  probabilities_markov_chain
glimpse(probabilities_markov_chain)

probabilities_bag_of_words %>% 
  bind_rows(probabilities_markov_chain) %>% 
  ggplot(aes(observed_probability, expected_probability, label = pairs))+
  geom_point()+
  ggrepel::geom_text_repel()+
  geom_abline(intercept = 0, slope = 1, linetype = 2) +
  facet_grid(glottocode~method, scales = "free")
ggsave('nasalization.png',height=12, width=9)

#--------stress--------------------

unnested_andic %>% 
  #distinct(ipa, glottocode, segment) %>% # remove repetitions
  filter(str_detect(segment, "[aoiue]")) %>%
  mutate(segment2 = case_when(
    str_detect(segment, "'", negate = FALSE)  ~ "stressed",
    str_detect(segment, "'", negate = TRUE)  ~ "unstressed")) ->
  unnested_andic_featured
glimpse(unnested_andic_featured)

unnested_andic_featured %>% 
  count(glottocode, segment2) %>% 
  group_by(glottocode) %>% 
  mutate(overall = sum(n),
         ratio = n/overall) ->
  unnested_andic_featured_frequencies
glimpse(unnested_andic_featured_frequencies)

unnested_andic_featured_frequencies %>% 
  select(-n, -overall) %>% 
  pivot_wider(names_from = glottocode, values_from = ratio)

unnested_andic_featured_frequencies %>% 
  group_by(segment2) %>%  # this and next 3 lines are for ordering segments
  mutate(mean_ratio = mean(ratio, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(segment2 = fct_reorder(segment2, mean_ratio)) %>% 
  top_n(100) %>% 
  ggplot(aes(ratio, segment2, color = glottocode))+
  geom_point()

unnested_andic_featured_frequencies %>% 
  mutate(segment2 = reorder_within(segment2, ratio, glottocode)) %>% 
  group_by(glottocode) %>% 
  top_n(25) %>% 
  ggplot(aes(ratio, segment2))+
  geom_point()+
  facet_wrap(~glottocode, scales = "free_y")+
  scale_y_reordered()

unnested_andic_featured %>%
  select(-segment) %>% 
  group_by(ipa) %>%  # remove those words that do have only one feature value per word
  mutate(n = n()) %>% 
  filter(n > 1) %>% 
  select(-n) %>% 
  nest(data = segment2) %>% 
  mutate(data = map(data, unlist),
         data = map(data, unique),
         data = map(data, sort), # prevent from different orderings within the pair
         length = map_dbl(data, length),
         data = ifelse(length == 1, map(data,function(x){c(x, x)}), data)) %>% 
  mutate(pairs = map(data, combn, m = 2, FUN = str_c, collapse = " ")) %>% 
  unnest_longer(pairs) %>% 
  ungroup() %>% 
  count(glottocode, pairs, sort = TRUE) %>% 
  separate(pairs, into = c("segment_1", "segment_2"), sep = " ", remove = FALSE) ->
  pairs

glimpse(pairs)

pairs %>% 
  group_by(glottocode) %>% 
  mutate(overall = sum(n),
         ratio = n/overall) %>% 
  top_n(20) %>% 
  mutate(pairs = reorder_within(pairs, ratio, glottocode)) %>% 
  ggplot(aes(ratio, pairs))+
  geom_point()+
  facet_wrap(~glottocode, scales = "free")+
  scale_y_reordered()

andic %>%
  filter(is.na(bor)) %>% 
  mutate(glottocode = ifelse(glottocode == "botl1242", str_c(glottocode, " ", reference), glottocode)) %>% 
  distinct(ipa, glottocode) %>% 
  mutate(id = 1:n(),
         ipa = str_c(ipa, "-#")) %>% 
  unnest_tokens(output = "segment", input = ipa, token = stringr::str_split, pattern = "-", drop = FALSE) %>% 
  filter(!is.na(segment)) %>% 
  filter(str_detect(segment, "[aoiue\\#]")) %>%
  mutate(segment = case_when(
    str_detect(segment, "'", negate = FALSE)  ~ "stressed",
    str_detect(segment, "'", negate = TRUE)  ~ "unstressed",
    str_detect(segment, "\\#") ~ "#")) %>% 
  mutate(next_segment = lead(segment)) %>% 
  filter(segment != "#",
         next_segment != "#") %>% 
  count(glottocode, segment, next_segment) ->
  unnested_andic_featured_with_hash

unnested_andic_featured_with_hash %>% 
  mutate(pairs = str_c(segment, " ", next_segment)) %>% 
  group_by(glottocode) %>% 
  mutate(overall = sum(n),
         ratio = n/overall) %>% 
  top_n(20) %>% 
  mutate(pairs = reorder_within(pairs, ratio, glottocode)) %>% 
  ggplot(aes(ratio, pairs))+
  geom_point()+
  facet_wrap(~glottocode, scales = "free")+
  scale_y_reordered()

unnested_andic_featured %>% 
  count(glottocode, segment2) %>% 
  rename(raw_n = n,
         segment = segment2) %>% 
  group_by(glottocode) %>% 
  mutate(frequency = raw_n/sum(raw_n)) %>% 
  select(-raw_n) ->
  pure_counts

pairs %>% 
  rename(segment = segment_1,
         next_segment = segment_2) %>% 
  full_join(pure_counts) %>% 
  rename(f1 = frequency) %>% 
  full_join(pure_counts, by = c("glottocode" = "glottocode", "next_segment" = "segment")) %>% 
  rename(f2 = frequency) %>% 
  group_by(glottocode) %>% 
  mutate(observed_probability = n/sum(n, na.rm = TRUE),
         multiply = segment != next_segment,
         expected_probability = f1*f2,
         expected_probability = ifelse(multiply, expected_probability*2, expected_probability),
         method = "bag of words") %>% 
  select(glottocode, pairs, observed_probability, expected_probability, method) ->
  probabilities_bag_of_words
glimpse(probabilities_bag_of_words)

unnested_andic_featured_with_hash %>% 
  full_join(pure_counts) %>% 
  rename(f1 = frequency) %>% 
  full_join(pure_counts, by = c("glottocode" = "glottocode", "next_segment" = "segment")) %>% 
  rename(f2 = frequency) %>% 
  group_by(glottocode) %>% 
  mutate(observed_probability = n/sum(n, na.rm = TRUE),
         expected_probability = f1*f2,
         method = "markov chain",
         pairs = str_c(segment, " ", next_segment)) %>% 
  select(glottocode, pairs, observed_probability, expected_probability, method) ->
  probabilities_markov_chain
glimpse(probabilities_markov_chain)

probabilities_bag_of_words %>% 
  bind_rows(probabilities_markov_chain) %>% 
  ggplot(aes(observed_probability, expected_probability, label = pairs))+
  geom_point()+
  ggrepel::geom_text_repel()+
  geom_abline(intercept = 0, slope = 1, linetype = 2) +
  facet_grid(glottocode~method, scales = "free")
ggsave('stress.png',height=12, width=9)

#-----backness------------------------------------------
unnested_andic %>% 
  # THIS CAUSE MULTIPLE TRUBLES!!! (30.09.2021)
  #distinct(ipa, glottocode, segment) %>% # remove repetitions
  filter(str_detect(segment, "[aoiue]")) %>%
  mutate(segment2 = case_when(
    str_detect(segment, "[iu]") ~ "high",
    str_detect(segment, "[eo]") ~ "mid",
    str_detect(segment, "a") ~ "low")) ->
  unnested_andic_featured
glimpse(unnested_andic_featured)
glimpse(unnested_andic_featured)

unnested_andic_featured %>% 
  count(glottocode, segment2) %>% 
  group_by(glottocode) %>% 
  mutate(overall = sum(n),
         ratio = n/overall) ->
  unnested_andic_featured_frequencies
glimpse(unnested_andic_featured_frequencies)

unnested_andic_featured_frequencies %>% 
  select(-n, -overall) %>% 
  pivot_wider(names_from = glottocode, values_from = ratio)

unnested_andic_featured_frequencies %>% 
  group_by(segment2) %>%  # this and next 3 lines are for ordering segments
  mutate(mean_ratio = mean(ratio, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(segment2 = fct_reorder(segment2, mean_ratio)) %>% 
  top_n(100) %>% 
  ggplot(aes(ratio, segment2, color = glottocode))+
  geom_point()

unnested_andic_featured_frequencies %>% 
  mutate(segment2 = reorder_within(segment2, ratio, glottocode)) %>% 
  group_by(glottocode) %>% 
  top_n(25) %>% 
  ggplot(aes(ratio, segment2))+
  geom_point()+
  facet_wrap(~glottocode, scales = "free_y")+
  scale_y_reordered()

unnested_andic_featured %>%
  select(-segment) %>% 
  group_by(glottocode, ipa) %>%
  nest(data = segment2) %>% 
  mutate(data = map(data, unlist),
         data = map(data, table),
         data2 = map(data, names),
         data2 = map(data2, unlist),
         data = map(data, function(x) x[x > 1]),
         data = map(data, names),
         data3 = map2(data, data2, function(x, y){c(unlist(x), unlist(y))}),
         data3 = map(data3, sort),
         length = map_dbl(data3, length)) %>% 
  filter(length > 1) %>% 
  mutate(pairs = map(data3, combn, m = 2, FUN = str_c, collapse = " "),
         pairs = map(pairs, unique)) %>% 
  unnest_longer(pairs) %>% 
  ungroup() %>% 
  count(glottocode, pairs, sort = TRUE) %>% 
  separate(pairs, into = c("segment_1", "segment_2"), sep = " ", remove = FALSE) ->
  pairs

glimpse(pairs)

pairs %>% 
  group_by(glottocode) %>% 
  mutate(overall = sum(n),
         ratio = n/overall) %>% 
  top_n(20) %>% 
  mutate(pairs = reorder_within(pairs, ratio, glottocode)) %>% 
  ggplot(aes(ratio, pairs))+
  geom_point()+
  facet_wrap(~glottocode, scales = "free")+
  scale_y_reordered()

andic %>%
  filter(is.na(bor)) %>% 
  mutate(glottocode = ifelse(glottocode == "botl1242", str_c(glottocode, " ", reference), glottocode)) %>% 
  distinct(ipa, glottocode) %>% 
  mutate(id = 1:n(),
         ipa = str_c(ipa, "-#")) %>% 
  unnest_tokens(output = "segment", input = ipa, token = stringr::str_split, pattern = "-", drop = FALSE) %>% 
  filter(!is.na(segment)) %>% 
  filter(str_detect(segment, "[aoiue\\#]")) %>%
  mutate(segment = case_when(
    str_detect(segment, "[iu]") ~ "high",
    str_detect(segment, "[eo]") ~ "mid",
    str_detect(segment, "a") ~ "low",
    str_detect(segment, "\\#") ~ "#")) %>% 
  mutate(next_segment = lead(segment)) %>% 
  filter(segment != "#",
         next_segment != "#") %>% 
  count(glottocode, segment, next_segment) ->
  unnested_andic_featured_with_hash

unnested_andic_featured_with_hash %>% 
  mutate(pairs = str_c(segment, " ", next_segment)) %>% 
  group_by(glottocode) %>% 
  mutate(overall = sum(n),
         ratio = n/overall) %>% 
  top_n(20) %>% 
  mutate(pairs = reorder_within(pairs, ratio, glottocode)) %>% 
  ggplot(aes(ratio, pairs))+
  geom_point()+
  facet_wrap(~glottocode, scales = "free")+
  scale_y_reordered()

unnested_andic_featured %>% 
  count(glottocode, segment2) %>% 
  rename(raw_n = n,
         segment = segment2) %>% 
  group_by(glottocode) %>% 
  mutate(frequency = raw_n/sum(raw_n)) %>% 
  select(-raw_n) ->
  pure_counts

pairs %>% 
  rename(segment = segment_1,
         next_segment = segment_2) %>% 
  full_join(pure_counts) %>% 
  rename(f1 = frequency) %>% 
  full_join(pure_counts, by = c("glottocode" = "glottocode", "next_segment" = "segment")) %>% 
  rename(f2 = frequency) %>% 
  group_by(glottocode) %>% 
  mutate(observed_probability = n/sum(n, na.rm = TRUE),
         multiply = segment != next_segment,
         expected_probability = f1*f2,
         expected_probability = ifelse(multiply, expected_probability*2, expected_probability),
         method = "bag of words") %>% 
  select(glottocode, pairs, observed_probability, expected_probability, method) ->
  probabilities_bag_of_words
glimpse(probabilities_bag_of_words)

unnested_andic_featured_with_hash %>% 
  full_join(pure_counts) %>% 
  rename(f1 = frequency) %>% 
  full_join(pure_counts, by = c("glottocode" = "glottocode", "next_segment" = "segment")) %>% 
  rename(f2 = frequency) %>% 
  group_by(glottocode) %>% 
  mutate(observed_probability = n/sum(n, na.rm = TRUE),
         expected_probability = f1*f2,
         method = "markov chain",
         pairs = str_c(segment, " ", next_segment)) %>% 
  select(glottocode, pairs, observed_probability, expected_probability, method) ->
  probabilities_markov_chain
glimpse(probabilities_markov_chain)

probabilities_bag_of_words %>% 
  bind_rows(probabilities_markov_chain) %>% 
  ggplot(aes(observed_probability, expected_probability, label = pairs))+
  geom_point()+
  ggrepel::geom_text_repel()+
  geom_abline(intercept = 0, slope = 1, linetype = 2) +
  facet_grid(glottocode~method, scales = "free")
ggsave('height.png',height=12, width=9)

#--------------
