input = readLines("C:\\Users\\volan\\Documents\\Advent of Code 2021\\2\\input.txt")

# Part 1

library(tidyverse)

input_tibble = dplyr::tibble(input)

input_tibble$split_string = strsplit(input, split = ' ')
input_tibble$instruction = as.character(purrr::map(input_tibble$split_string, 
                               function(item){item[[1]]}))
input_tibble$number = as.numeric(purrr::map(input_tibble$split_string, 
                                            function(item){item[[2]]}))

input_tibble %>% group_by(instruction) %>% summarise(total_effect = sum(number))
# 1946 down, 2003 forward, 966 up
(2003 * (1946 - 966)) #1962940

input_tibble = input_tibble %>% mutate(
  aim_delta = case_when(instruction == 'down' ~ number,
                        instruction == 'up' ~ -number,
                        TRUE ~ 0),
  aim = cumsum(aim_delta),
  depth_delta = ifelse(instruction == 'forward',
                       aim * number, 0)
)
input_tibble %>% filter(instruction == 'forward') %>%
  summarise(horizontal = sum(number), depth = sum(depth_delta)) %>%
  mutate(product = horizontal * depth)
