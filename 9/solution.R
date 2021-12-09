input = readLines("C:\\Users\\volan\\Documents\\Advent of Code 2021\\9\\input.txt")

library(tidyverse)

heightmap_as_matrix = matrix(as.numeric(unlist(strsplit(input, ''))),
                                nrow = length(input), byrow = TRUE)

# lines_as_data_frames = lapply(1:length(input),
#                               function(row_number){
#                                 row_string = input[row_number]
#                                 row_parsed = unlist(strsplit(row_string, ''))
#                                 return(data.frame(
#                                   row = row_number,
#                                   column = 1:length(row_parsed),
#                                   height = row_parsed))
#                               })
# heightmap_as_data_frame = do.call(rbind, lines_as_data_frames)

locally_minimal_horizontally = heightmap_as_matrix < 
  cbind(heightmap_as_matrix[,2:ncol(heightmap_as_matrix)], rep(Inf, nrow(heightmap_as_matrix))) &
  heightmap_as_matrix < 
  cbind(rep(Inf, nrow(heightmap_as_matrix)), heightmap_as_matrix[,1:(ncol(heightmap_as_matrix) - 1)])

locally_minimal_vertically = heightmap_as_matrix < 
  rbind(heightmap_as_matrix[2:nrow(heightmap_as_matrix),], rep(Inf, ncol(heightmap_as_matrix))) &
  heightmap_as_matrix < 
  rbind(rep(Inf, ncol(heightmap_as_matrix)), heightmap_as_matrix[1:(nrow(heightmap_as_matrix) - 1),])

locally_minimal = locally_minimal_horizontally & locally_minimal_vertically

sum(locally_minimal * (heightmap_as_matrix + 1))

library(igraph)

coordinates = data.frame(start_row = rep(1:nrow(heightmap_as_matrix), times = ncol(heightmap_as_matrix)),
                         start_col = rep(1:ncol(heightmap_as_matrix), each = nrow(heightmap_as_matrix)))

adjacent_coordinates = rbind(
  coordinates %>% mutate(end_row = start_row, end_col = start_col + 1),
  coordinates %>% mutate(end_row = start_row, end_col = start_col - 1),
  coordinates %>% mutate(end_row = start_row + 1, end_col = start_col),
  coordinates %>% mutate(end_row = start_row - 1, end_col = start_col)
) %>%
  filter(end_row <= 100, end_row > 0, end_col <= 100, end_col > 0)

adjacent_values = adjacent_coordinates %>%
  mutate(start_value = mapply(function(row, col){heightmap_as_matrix[row, col]}, start_row, start_col),
         end_value = mapply(function(row, col){heightmap_as_matrix[row, col]}, end_row, end_col))

potential_edges = adjacent_values %>% filter(end_value < start_value & start_value != 9) %>%
  mutate(start_coord = paste0(start_row, ',', start_col),
         end_coord = paste0(end_row, ',', end_col))

digraph = graph_from_data_frame(potential_edges %>% select(start_coord, end_coord, everything()), 
                                directed = TRUE, vertices = NULL)
components = components(digraph)
prod(sort(components$csize, decreasing = TRUE)[1:3]) #900900
