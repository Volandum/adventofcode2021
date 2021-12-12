input = readLines("C:\\Users\\volan\\Documents\\Advent of Code 2021\\12\\input.txt")

split_edge_string = Vectorize(function(edge_string, position){
  unlist(strsplit(edge_string, '-'))[position]
})

starts = split_edge_string(input, 1)
ends = split_edge_string(input,2)

all_edges = unique(data.frame(start = c(starts,ends), end = c(ends, starts)))
# Observation: there are no directly connected large caverns

all_edges$small_start = tolower(all_edges$start) == all_edges$start
all_edges$small_end = tolower(all_edges$end) == all_edges$end

library(tidyverse)

small_to_small_basic = all_edges %>% filter(small_start, small_end) %>% select(start, end) %>%mutate(original = TRUE)
small_to_small_via_large = all_edges %>%
  filter(!small_end) %>% rename(large = end) %>%
  inner_join(all_edges %>% filter(!small_start) %>% rename(large = start), by = 'large') %>%
  select(start, end) %>% mutate(original = FALSE)

all_small_to_small = union_all(small_to_small_basic, small_to_small_via_large)

small_to_small_multiplicities = all_small_to_small %>% group_by(start, end) %>% 
  summarise(multiplicity = n(), .groups = 'drop') %>% filter(start != end)

# There are 9 nodes including start and end so we can do a recursive depth-first search as 9! is < 10^6.

routes_to_end = function(current_node, visited_nodes){
  # Returns routes to end from inputs not revisiting any nodes already visited
  if(current_node == 'end'){
    return(1)
  }
  neighbourhood = small_to_small_multiplicities %>%
    filter(start == current_node, !(end %in% visited_nodes))
  neighbours = neighbourhood %>% pull(end)
  if(length(neighbours) == 0){
    return(0)
  }
  return_value = 0
  for(option_row in 1:nrow(neighbourhood)){
    option_neighbour = neighbours[option_row]
    return_value = return_value + neighbourhood$multiplicity[option_row] *
      routes_to_end(option_neighbour, c(visited_nodes, option_neighbour))
  }
  return(return_value)
}

routes_to_end('start', 'start') #4549

# See if we can do the recursion again for the twice problem - may not be feasible

#do not allow return to start, but do allow loops

disallow_return_to_start = all_small_to_small %>% group_by(start, end) %>% 
  summarise(multiplicity = n(), .groups = 'drop') %>% filter(end != 'start', start != 'end')

install.packages("memoise")

routes_to_end_allow_2 = memoise::memoise(function(current_node, visited_nodes){
  # Returns routes to end from inputs not revisiting any nodes already visited
  if(current_node == 'end'){
    return(1)
  }
  excluded_nodes = names(table(visited_nodes)[table(visited_nodes) == 2])
  neighbourhood = disallow_return_to_start %>%
    filter(start == current_node, !(end %in% excluded_nodes))
  neighbours = neighbourhood %>% pull(end)
  if(length(neighbours) == 0){
    return(0)
  }
  return_value = 0
  for(option_row in 1:nrow(neighbourhood)){
    option_neighbour = neighbours[option_row]
    return_value = return_value + neighbourhood$multiplicity[option_row] *
      routes_to_end_allow_2(option_neighbour, sort(c(visited_nodes, option_neighbour)))
  }
  return(return_value)
})

routes_to_end_allow_2('start', 'start') #149043467

# memoise makes a huge difference here

# Ah I misread the question - only one cave can be visited more than once

routes_to_end_allow_one_2 = memoise::memoise(function(current_node, visited_nodes){
  # Returns routes to end from inputs not revisiting any nodes already visited
  if(current_node == 'end'){
    return(1)
  }
  any_twos = 2 %in% table(visited_nodes)
  if(any_twos){
  neighbourhood = disallow_return_to_start %>%
    filter(start == current_node, !(end %in% visited_nodes))
  } else{
    neighbourhood = disallow_return_to_start %>%
      filter(start == current_node)
  }
  neighbours = neighbourhood %>% pull(end)
  if(length(neighbours) == 0){
    return(0)
  }
  return_value = 0
  for(option_row in 1:nrow(neighbourhood)){
    option_neighbour = neighbours[option_row]
    return_value = return_value + neighbourhood$multiplicity[option_row] *
      routes_to_end_allow_one_2(option_neighbour, sort(c(visited_nodes, option_neighbour)))
  }
  return(return_value)
})

routes_to_end_allow_one_2('start', 'start') #120535
