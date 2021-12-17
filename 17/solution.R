input = readLines("C:\\Users\\volan\\Documents\\Advent of Code 2021\\17\\input.txt")

x_range = c(209, 238)
y_range = c(-86, -59)

x_steps = function(x_velocity){
  #assume x_velocity > 0
  sapply(1:x_velocity, 
         function(time_step){
           x_velocity * time_step - (time_step * (time_step - 1) / 2)
         })
}

x_positions = lapply(1:238, x_steps)
time_steps_valid_min_max = lapply(x_positions, function(positions){
  acceptable_positions = which(positions >= x_range[1] & positions <= x_range[2])
  c(min(acceptable_positions), max(acceptable_positions))
})

# Highest acceptable y-velocity is +85 (x-velocity of 20 lets us loiter at x = 210)
# This gets us to a highest y position of 
86 * 85 / 2 #3655

time_steps_valid = lapply(x_positions, function(positions){
  acceptable_positions = which(positions >= x_range[1] & positions <= x_range[2])
  acceptable_positions
})

#20 and 21 x-velocity keep us in the target x-area indefinitely

# Our range of acceptable y-velocities is +85 to -86

y_steps = function(y_velocity){
  sapply(1:200, 
         function(time_step){
           y_velocity * time_step - (time_step * (time_step - 1) / 2)
         })
}

y_positions = lapply(-86:85, y_steps)

time_steps_y_valid = lapply(y_positions, function(positions){
  acceptable_positions = which(positions >= y_range[1] & positions <= y_range[2])
  acceptable_positions
})

unlist(time_steps_y_valid)
time_steps_x_valid = time_steps_valid
time_steps_x_valid[[20]] = 19:172
time_steps_x_valid[[21]] = 15:172

library(tidyverse)

list_of_vectors_to_df = function(input_list){
  list_length = length(input_list)
  lapply(1:list_length, function(list_item_index){
    data.frame(time = input_list[[list_item_index]], 
               list_item_index = rep(list_item_index, times = length(input_list[[list_item_index]])))
  })
}

time_steps_x_valid_df = list_of_vectors_to_df(time_steps_x_valid) %>%
  do.call(rbind, .) %>% rename(x_velocity_index = list_item_index)

time_steps_y_valid_df = list_of_vectors_to_df(time_steps_y_valid) %>%
  do.call(rbind, .) %>% rename(y_velocity_index = list_item_index)

inner_join(time_steps_x_valid_df, time_steps_y_valid_df, by = 'time') %>% nrow #1484 possibilities
inner_join(time_steps_x_valid_df, time_steps_y_valid_df, by = 'time') %>% 
  distinct(x_velocity_index, y_velocity_index) %>% nrow #1447
