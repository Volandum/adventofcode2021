test_input = readLines('clipboard')
input = test_input
input = readLines("C:\\Users\\volan\\Documents\\Advent of Code 2021\\25\\input.txt")

input_vector = unlist(strsplit(input, ''))
number_rows = length(input)
number_columns = nchar(input[1])
input_matrix = matrix(input_vector, nrow = number_rows, byrow = TRUE)

library(tidyverse)

increment_index = function(index, columns = 0, rows = 0){
  row_indexes_minus_one = (index - 1) %% number_rows
  column_indexes_minus_one = (index - 1) %/% number_rows
  new_row_indexes_minus_one = (row_indexes_minus_one + rows) %% number_rows
  new_column_indexes_minus_one = (column_indexes_minus_one + columns) %% number_columns
  new_indexes = new_column_indexes_minus_one * number_rows + new_row_indexes_minus_one + 1
  return(new_indexes)
}

update_matrix = function(matrix_to_update){
  right_move = which(matrix_to_update == '>')
  empty_slots = which(matrix_to_update == '.')
  to_move = intersect(right_move, increment_index(empty_slots, columns = -1))
  matrix_to_update[to_move] = '.'
  matrix_to_update[increment_index(to_move, columns = 1)] = '>'
  moved_right = length(to_move)
  
  down_move = which(matrix_to_update == 'v')
  empty_slots = which(matrix_to_update == '.')
  to_move = intersect(down_move, increment_index(empty_slots, rows = -1))
  matrix_to_update[to_move] = '.'
  matrix_to_update[increment_index(to_move, rows = 1)] = 'v'
  moved_down = length(to_move)
  return(list(new_matrix = matrix_to_update, moved_right = moved_right, moved_down = moved_down))
}

working_matrix = input_matrix
steps = 0
continue = TRUE

while(continue){
  steps = steps + 1
  updated_matrix = update_matrix(working_matrix)
  if(updated_matrix$moved_right == 0 & updated_matrix$moved_down == 0){
    continue = FALSE
  } else {
    working_matrix = updated_matrix$new_matrix
  }
}
