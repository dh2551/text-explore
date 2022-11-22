library(tidyverse)
library(tidytext)
library(ROCR)
library(rvest)
library(foreach)
library(strsplit)
# library(MASS)
#### read files

# plot_lines <- tibble(lines = readr::read_lines('../data/plots'))
# title_lines <- tibble(title = readr::read_lines('../data/titles'))



# make a check list
story_title <- title_lines$title

# count story numbers with lines 
story_num <- cumsum(plot_lines$lines == "<EOS>") + 1

plot_text <- tibble(title = story_title[story_num], story_number = story_num)

# check line number
# plot_lines_filtered <- filter(plot_lines, lines != "<EOS>")

plot_text$text <- plot_lines$lines

plot_text <- plot_text %>%
  filter(text != "<EOS>")



#split using unnest_tokens()

plot_words <- plot_text %>%
  unnest_tokens(word, text)

# plot_words <- plot_words %>%
#   distinct(word, story_number, title)

# words_length <- plot_words %>%
#   group_by(story_number) %>%
#   count(word)

story_word_length <- plot_words %>%
  group_by(story_number) %>%
  count()

plot_words <- left_join(plot_words, story_word_length)

plot_words <- plot_words %>% 
  group_by(story_number) %>%
  mutate(position = row_number(),
         word_position = paste0(position, '/', n),
         word_pos_frac = position/n)

median_word_positions <- plot_words %>%
  group_by(word) %>%
  mutate(median_position = median(word_pos_frac),
         number = n()) 

# test1 <- median_word_positions %>%
#   dplyr::select(-n, - position)

median_word_positions <- median_word_positions %>%
  distinct(word, median_position, number) %>%
  filter(number > 2500) %>%
  arrange(desc(median_position)) 

#row_number() / n()

# median_word_positions <- median_word_positions %>%
#   subset(n > 2500)


## graph on words

# top_10 <- median_word_positions %>%
#   arrange(desc(median_position))
# 
# num_tib = 10
# 
# top_10 %>% 
#   top_n(10)

top_10_words <- head(median_word_positions, 10)

bottom_10_words <- tail(median_word_positions, 10)

interesting_words <- rbind(top_10_words,bottom_10_words)


ggplot(interesting_words, aes(x = reorder(word, -median_position), y = median_position))+
  geom_bar(stat = 'identity') + 
  labs(title = 'Median position by words', x = 'Words', y = 'Median Positions' )
ggsave('../figures/median_position_by_word.png')

ggplot(interesting_words, aes(x = reorder(word, -number), y = number))+
  geom_bar(stat = 'identity') + 
  labs(title = 'Count_by_word', x = 'Words', y = 'Number of Occurrences' )

ggsave('../figures/count_by_word.png')


## words distribution 

word_decile_counts <- plot_words %>% 
  mutate(decile = (case_when(word_pos_frac <= 0.1 ~ '1',
                             word_pos_frac > 0.1 & word_pos_frac <= 0.2  ~ '2',
                             word_pos_frac > 0.2 & word_pos_frac <= 0.3 ~ '3',
                             word_pos_frac > 0.3 & word_pos_frac <= 0.4 ~ '4',
                             word_pos_frac > 0.4 & word_pos_frac <= 0.5 ~ '5',
                             word_pos_frac > 0.5 & word_pos_frac <= 0.6 ~ '6',
                             word_pos_frac > 0.6 & word_pos_frac <= 0.7 ~ '7',
                             word_pos_frac > 0.7 & word_pos_frac <= 0.8 ~ '8',
                             word_pos_frac > 0.8 & word_pos_frac <= 0.9 ~ '9',
                             word_pos_frac > 0.9 & word_pos_frac <= 1 ~ '10'))) 

word_decile_count <- word_decile_counts %>%
  mutate(decile = as.numeric(decile))%>%
  group_by(word, decile) %>%
  summarize(ct = n()) %>%
  arrange(desc(ct))


#join

new_word_tib <- inner_join(interesting_words, word_decile_count)

new_word_tib <- new_word_tib %>%
  arrange(desc(median_position)) %>%
  mutate(group_color = ifelse(median_position > 0.5, "High", "Low"))

# geom_bar(stat = 'identity') + 


ggplot(new_word_tib)+
  geom_line( aes(x = decile, y = ct/number, color = group_color, group = group_color)) +
  facet_wrap(vars(word))+
  labs(title = 'word_distribution_over_deciles', x = 'decile', y = 'frequency')

# ggsave('../figures/word_distribution_over_deciles.png')


##sentiment analysis 
# get_sentiments("bing")

plot_words_with_sentiments <- plot_words %>%
  left_join(get_sentiments("bing")) 


unique_plot_words <- plot_words %>%
  distinct(word)

# nrow(unique_plot_words)
## 19864744

inner_join_sentiments <- plot_words %>% 
  inner_join(get_sentiments("bing"))

## nrow() if inner_join -> 2368187

nrow(inner_join_sentiments)/nrow(unique_plot_words)

##words sentiments

plot_words_with_sentiments <- plot_words_with_sentiments %>% 
  mutate(decile = (case_when(word_pos_frac <= 0.1 ~ '1',
                             word_pos_frac > 0.1 & word_pos_frac <= 0.2  ~ '2',
                             word_pos_frac > 0.2 & word_pos_frac <= 0.3 ~ '3',
                             word_pos_frac > 0.3 & word_pos_frac <= 0.4 ~ '4',
                             word_pos_frac > 0.4 & word_pos_frac <= 0.5 ~ '5',
                             word_pos_frac > 0.5 & word_pos_frac <= 0.6 ~ '6',
                             word_pos_frac > 0.6 & word_pos_frac <= 0.7 ~ '7',
                             word_pos_frac > 0.7 & word_pos_frac <= 0.8 ~ '8',
                             word_pos_frac > 0.8 & word_pos_frac <= 0.9 ~ '9',
                             word_pos_frac > 0.9 & word_pos_frac <= 1 ~ '10'))) 



plot_words_with_sentiments <- plot_words_with_sentiments %>%
  mutate(senti_recode = case_when(sentiment == 'positive' ~ 1,
                                  sentiment == 'negative' ~ 0,
                                  TRUE ~ NA_real_))

plots <- plot_words_with_sentiments %>%
  filter(!is.na(senti_recode))

plots <- plots %>%
  group_by(story_number, decile) %>%
  mutate(mean_sentiment = mean(senti_recode))

plots <-plots %>%
  group_by(decile) %>%
  distinct(story_number, title, mean_sentiment) 


## process 1


modeling_data <- plots %>%
  spread(key = 'decile', value = 'mean_sentiment', sep = '_')

# colnames(modeling_data) <- c("title","story_number", "word", "n", "posotion", "word_position", "word_pos_frac", "sentiment","senti_recode", "decile_1", "decile_10",
#                              "decile_2", "decile_3", "decile_4", "decile_5", "decile_6",
#                              "decile_7", "decile_8", "decile_9")



##process2
modeling_data <- modeling_data %>%
  group_by(title) %>%
  filter(!is.na(decile_10)) %>%
  mutate(decile_10 = decile_10)

col_order <- c("title","story_number","decile_1", "decile_2", "decile_3", "decile_4", "decile_5", "decile_6","decile_7", "decile_8", "decile_9", "decile_10")
modeling_data<- modeling_data[, col_order]
modeling_data

modeling_data<- cbind(modeling_data, ct = rowSums(is.na(modeling_data[3:11]), na.rm = FALSE))

modeling_data<-  modeling_data %>%
  filter(ct < 4)

modeling_data<- cbind(modeling_data, r_sums = rowSums(modeling_data[3:11], na.rm = TRUE), r_means = rowMeans(modeling_data[3:11], na.rm = TRUE))



modeling_data<- modeling_data%>% 
  rowwise() %>%
  mutate_at(vars(c("decile_1","decile_2", "decile_3", "decile_4", "decile_5", "decile_6","decile_7", "decile_8", "decile_9", "decile_10")),
            funs(replace(., is.na(.), r_means))) %>%
  ungroup()

##fit logit


modeling_data <- modeling_data %>%
  mutate(is_happy_ending = ifelse(decile_10 >= 0.5, 1, 0))  

set.seed(2048)
modeling_data_fit <- modeling_data %>% select(-decile_10,-r_sums, - r_means, - ct, -title, - story_number) %>%slice(sample(1:n()))

train_data <- modeling_data_fit %>%
  slice(1:floor(nrow(.)*0.8))

test_data <- modeling_data_fit %>%
  slice(floor(nrow(.)*0.8)+1:n())


ending_fit <- glm(is_happy_ending ~ .,
                  data = train_data,
                  family = binomial)
summary(ending_fit)


test_data_test <- test_data %>%
  select(-is_happy_ending)

test_data$pred_prob <- predict(ending_fit, test_data_test, type = 'response')

pred_test_data <- prediction(test_data$pred_prob,test_data$is_happy_ending)
pred_test_data  <- performance(pred_test_data, 'auc')
test_auc <- pred_test_data @y.values[[1]]
test_auc 


##rewrite ending 

temp <- read_html('https://en.wikipedia.org/wiki/A_Star_Is_Born_(2018_film)')

plot_txt<- temp %>%
  html_nodes("p") %>%
  html_text(trim = T) 

plot_txt <- tibble('story' = plot_txt )
plt_txt <- slice(plot_txt, 5:9)


star_was_born_word <- plt_txt %>%
  unnest_tokens(word, story)

star_was_born_word <- star_was_born_word %>%
  mutate(word_pos = row_number() / nrow(.),
         decile = (case_when(word_pos <= 0.1 ~ '1',
                             word_pos > 0.1 & word_pos <= 0.2  ~ '2',
                             word_pos > 0.2 & word_pos <= 0.3 ~ '3',
                             word_pos > 0.3 & word_pos <= 0.4 ~ '4',
                             word_pos > 0.4 & word_pos <= 0.5 ~ '5',
                             word_pos > 0.5 & word_pos <= 0.6 ~ '6',
                             word_pos > 0.6 & word_pos <= 0.7 ~ '7',
                             word_pos > 0.7 & word_pos <= 0.8 ~ '8',
                             word_pos > 0.8 & word_pos <= 0.9 ~ '9',
                             word_pos > 0.9 & word_pos <= 1 ~ '10')))

star_word_with_sentiments <- star_was_born_word %>%
  inner_join(get_sentiments("bing")) %>%
  mutate(senti_recode = case_when(sentiment == 'positive' ~ 1,
                                  sentiment == 'negative' ~ 0,
                                  TRUE ~ NA_real_))%>%
  group_by(decile) %>%
  mutate(mean_senti = mean(senti_recode)) 

star_word_avg_senti <- star_word_with_sentiments %>%
  distinct(decile, mean_senti) %>%
  spread(key = 'decile', value = 'mean_senti', sep = '_')

star_test <- star_word_avg_senti %>% select(-decile_10)

star_test$pred_prob <- predict(ending_fit, star_test, type = 'response')

check_decile_10 <- filter(star_was_born_word, decile == 10)
# head(check_decile_10)

### read new plot with txt
star_new_plot <- tibble(lines = readr::read_lines('../data/plot_rewritten.txt'))


star_new_plot  <- star_new_plot  %>%
  unnest_tokens(word, lines)

star_new_words <- star_new_plot %>%
  mutate(word_pos = row_number() / nrow(.),
         decile = (case_when(word_pos <= 0.1 ~ '1',
                             word_pos > 0.1 & word_pos <= 0.2  ~ '2',
                             word_pos > 0.2 & word_pos <= 0.3 ~ '3',
                             word_pos > 0.3 & word_pos <= 0.4 ~ '4',
                             word_pos > 0.4 & word_pos <= 0.5 ~ '5',
                             word_pos > 0.5 & word_pos <= 0.6 ~ '6',
                             word_pos > 0.6 & word_pos <= 0.7 ~ '7',
                             word_pos > 0.7 & word_pos <= 0.8 ~ '8',
                             word_pos > 0.8 & word_pos <= 0.9 ~ '9',
                             word_pos > 0.9 & word_pos <= 1 ~ '10')))

star_new_word_with_sentiments <- star_new_words  %>%
  inner_join(get_sentiments("bing")) %>%
  mutate(senti_recode = case_when(sentiment == 'positive' ~ 1,
                                  sentiment == 'negative' ~ 0,
                                  TRUE ~ NA_real_))%>%
  group_by(decile) %>%
  mutate(mean_senti = mean(senti_recode)) 

star_new_word_avg_senti <- star_new_word_with_sentiments %>%
  distinct(decile, mean_senti) %>%
  spread(key = 'decile', value = 'mean_senti', sep = '_')






