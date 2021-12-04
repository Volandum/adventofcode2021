input = readLines("C:\\Users\\volan\\Documents\\Advent of Code 2021\\4\\input.txt")

input_numbers = as.numeric(unlist(strsplit(input[1], ',')))

boards_start = 3 + 6 * 0:99
boards_raw = lapply(boards_start, function(starting_row){
  input[starting_row:(starting_row + 4)]
})

board_as_df = function(raw_board){
  converted_to_list = lapply(strsplit(gsub('  ', ' ', trimws(raw_board)), ' '), as.numeric)
  list_of_dfs = lapply(1:5, function(row){
    data.frame(row = row, column = 1:5, cell = converted_to_list[[row]])
  })
  return(do.call(rbind, list_of_dfs))
}

all_boards_as_dfs = lapply(1:100, function(board_index){
  board_df = board_as_df(boards_raw[[board_index]])
  board_df$board = board_index
  return(board_df)
})

library(tidyverse)

all_boards_df = do.call(rbind, all_boards_as_dfs)

check_if_bingo = function(df, remaining_boards){
  # Returns the winning board(s)
  remaining_rows = df %>% distinct(board, row)
  all_possible_rows = inner_join(data.frame(board = remaining_boards), data.frame(row = 1:5), by = character())
  missing_rows = setdiff(all_possible_rows, remaining_rows)
  row_winners = missing_rows$board
  
  remaining_columns = df %>% distinct(board, column)
  all_possible_columns = inner_join(data.frame(board = remaining_boards), data.frame(column = 1:5), by = character())
  missing_columns = setdiff(all_possible_columns, remaining_columns)
  column_winners = missing_columns$board

  return(union(row_winners, column_winners))
}

# run through the bingo
working_boards_df = all_boards_df
remaining_boards = 1:100
for(last_called_number in input_numbers){
  working_boards_df = working_boards_df %>% filter(cell != last_called_number)
  winning_boards = check_if_bingo(working_boards_df, remaining_boards)
  if(length(winning_boards) > 0) break
}
last_called_number # 99
winning_boards #42
unmarked_numbers_total = working_boards_df %>% filter(board == winning_boards) %>% pull(cell) %>% sum #899

last_called_number * unmarked_numbers_total

# run through the bingo again

# Do all the boards win? No: 
uncalled_numbers = setdiff(unique(all_boards_df$cell), input_numbers) # is empty - all boards will win
non_winning_boards = filter(all_boards_df, cell %in% uncalled_numbers) %>% pull(board) %>% unique

working_boards_df = all_boards_df
remaining_boards = 1:100
for(last_called_number in input_numbers){
  working_boards_df = working_boards_df %>% filter(cell != last_called_number)
  winning_boards = check_if_bingo(working_boards_df, remaining_boards)
  remaining_boards = setdiff(remaining_boards, winning_boards)
  if(length(remaining_boards) == 0) break
}
last_called_number # 38
winning_boards #45
unmarked_numbers_total = working_boards_df %>% filter(board == winning_boards) %>% pull(cell) %>% sum #192
last_called_number * unmarked_numbers_total
