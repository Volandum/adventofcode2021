input = readLines("C:\\Users\\volan\\Documents\\Advent of Code 2021\\14\\input.txt")

initial_string = input[1]
rules_strings = input[3:length(input)]

split_vectorised = Vectorize(function(text, separators, position){
  unlist(strsplit(text, separators))[position]
})

initial_string_vector = unlist(strsplit(initial_string, ''))
rules = data.frame(first_character = split_vectorised(rules_strings, '', 1),
                   second_character = split_vectorised(rules_strings, '', 2),
                   new_character = split_vectorised(rules_strings, ' -> ', 2))

characters = unique(rules$first_character)

library(tidyverse)

pair_transition_rules = union_all(rules %>% mutate(initial_pair = paste0(first_character, second_character), 
                                         new_pair = paste0(first_character, new_character)),
                                  rules %>% mutate(initial_pair = paste0(first_character, second_character), 
                                                   new_pair = paste0(new_character, second_character))) %>% 
  select(initial_pair, new_pair)

initial_pairs = paste0(initial_string_vector[1:(length(initial_string_vector) - 1)], 
                       initial_string_vector[2:length(initial_string_vector)])

pair_dataset = data.frame(current_pair = initial_pairs, count = 1)

for(n in 1:10){
  pair_dataset = pair_dataset %>%
    inner_join(pair_transition_rules, by = c("current_pair" = "initial_pair")) %>%
    transmute(current_pair = new_pair, count) %>%
    group_by(current_pair) %>%
    summarise(count = sum(count), .groups = 'drop')
}

max(pair_dataset$count) - min(pair_dataset$count) #1262

pair_dataset %>% mutate(initial_element_of_pair = substr(current_pair, 1,1)) %>% 
  group_by(initial_element_of_pair) %>%
  summarise(count = sum(count), .groups = 'drop') ->
  initial_element_counts

characters[10] #need to add a B if it's max or min which it isn't
max(initial_element_counts$count) - min(initial_element_counts$count)

pair_dataset = data.frame(current_pair = initial_pairs, count = 1)

for(n in 1:40){
  pair_dataset = pair_dataset %>%
    inner_join(pair_transition_rules, by = c("current_pair" = "initial_pair")) %>%
    transmute(current_pair = new_pair, count) %>%
    group_by(current_pair) %>%
    summarise(count = sum(count), .groups = 'drop')
}

pair_dataset %>% mutate(initial_element_of_pair = substr(current_pair, 1,1)) %>% 
  group_by(initial_element_of_pair) %>%
  summarise(count = sum(count), .groups = 'drop') ->
  initial_element_counts

# again no need to add the B
(max(initial_element_counts$count) - min(initial_element_counts$count)) %>%
  sprintf('%.0f', .) #2959788056212

# Actual answer is 2959788056211 because K is the last character