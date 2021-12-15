input = readLines("C:\\Users\\volan\\Documents\\Advent of Code 2021\\15\\input.txt")

costs_by_line = as.numeric(unlist(lapply(input, strsplit, split = '')))

cost_matrix = matrix(costs_by_line, nrow = 100, byrow = TRUE)

costs_dataframe = data.frame(row = rep(1:100, each = 100),
                             col = rep(1:100, times = 100),
                             cost = costs_by_line)

working_path_dataframe = data.frame(row = 1, col = 1, total_cost = 0)

new_path_dataframe = working_path_dataframe

library(tidyverse)

while(nrow(new_path_dataframe) > 0){
  
  potential_new_paths_starting_point = new_path_dataframe %>% 
    rename(previous_row = row, previous_col = col, previous_total_cost = total_cost) 
  
  potential_new_paths = union_all(union_all(potential_new_paths_starting_point %>% 
                                              mutate(row = previous_row, col = previous_col + 1),
                                            potential_new_paths_starting_point %>% 
                                              mutate(row = previous_row, col = previous_col - 1)),
                                  union_all(potential_new_paths_starting_point %>% 
                                              mutate(row = previous_row + 1, col = previous_col),
                                            potential_new_paths_starting_point %>% 
                                              mutate(row = previous_row - 1, col = previous_col))) %>%
    inner_join(costs_dataframe, by = c("row", "col")) %>%
    mutate(total_cost = previous_total_cost + cost) %>%
    select(row, col, total_cost)
  
  new_path_dataframe = union_all(potential_new_paths %>% 
                                   anti_join(working_path_dataframe, by = c("row", "col")),
                                 potential_new_paths %>% 
                                   inner_join(working_path_dataframe %>% rename(previous_total_cost = total_cost),
                                              by = c("row", "col")) %>%
                                   filter(total_cost < previous_total_cost) %>%
                                   select(row, col, total_cost)) %>%
    group_by(row, col) %>% 
    slice_min(total_cost, with_ties = FALSE) %>% 
    ungroup()
  
  working_path_dataframe = union_all(working_path_dataframe %>%
                                       anti_join(new_path_dataframe, by = c("row", "col")),
                                     new_path_dataframe)
  
}

working_path_dataframe %>%
  filter(row == 100, col == 100)

shift_dataframe = function(input_df, rows_shift, cols_shift){
  increment = rows_shift + cols_shift
  input_df %>% mutate(row = row + rows_shift * 100,
                      col = col + cols_shift * 100,
                      cost = (cost + rows_shift + cols_shift - 1) %% 9 + 1) %>%
    return
}

costs_dataframe_list = mapply(shift_dataframe, 
                              input_df = list(costs_dataframe),
                              rows_shift = rep(0:4, times = 5),
                              cols_shift = rep(0:4, each = 5),
                              SIMPLIFY = FALSE)

costs_dataframe = do.call(rbind, costs_dataframe_list)

working_path_dataframe = data.frame(row = 1, col = 1, total_cost = 0)

new_path_dataframe = working_path_dataframe

current_step = 0

while(nrow(new_path_dataframe) > 0){
  
  potential_new_paths_starting_point = new_path_dataframe %>% 
    rename(previous_row = row, previous_col = col, previous_total_cost = total_cost) 
  
  potential_new_paths = union_all(union_all(potential_new_paths_starting_point %>% 
                                              mutate(row = previous_row, col = previous_col + 1),
                                            potential_new_paths_starting_point %>% 
                                              mutate(row = previous_row, col = previous_col - 1)),
                                  union_all(potential_new_paths_starting_point %>% 
                                              mutate(row = previous_row + 1, col = previous_col),
                                            potential_new_paths_starting_point %>% 
                                              mutate(row = previous_row - 1, col = previous_col))) %>%
    inner_join(costs_dataframe, by = c("row", "col")) %>%
    mutate(total_cost = previous_total_cost + cost) %>%
    select(row, col, total_cost)
  
  new_path_dataframe = union_all(potential_new_paths %>% 
                                   anti_join(working_path_dataframe, by = c("row", "col")),
                                 potential_new_paths %>% 
                                   inner_join(working_path_dataframe %>% rename(previous_total_cost = total_cost),
                                              by = c("row", "col")) %>%
                                   filter(total_cost < previous_total_cost) %>%
                                   select(row, col, total_cost)) %>%
    group_by(row, col) %>% 
    slice_min(total_cost, with_ties = FALSE) %>% 
    ungroup()
  
  working_path_dataframe = union_all(working_path_dataframe %>%
                                       anti_join(new_path_dataframe, by = c("row", "col")),
                                     new_path_dataframe)
  least_point_affected = min(new_path_dataframe$row + new_path_dataframe$col)
  current_step = current_step + 1
  if(current_step %% 100 == 0){
    message(paste0('Working - least point affected has x + y at ', least_point_affected))
  }
}

working_path_dataframe %>%
  filter(row == 500, col == 500)
