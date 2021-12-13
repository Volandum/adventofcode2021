input = readLines("C:\\Users\\volan\\Documents\\Advent of Code 2021\\13\\input.txt")

input_break = which(input == "")

split_vectorised = Vectorize(function(text, separators, position){
  unlist(strsplit(text, separators))[position]
})

library(tidyverse)

coordinates = data.frame(x = as.numeric(split_vectorised(input[1:(input_break - 1)], ',', 1)),
                         y = as.numeric(split_vectorised(input[1:(input_break - 1)], ',', 2)))

instructions = data.frame(axis = split_vectorised(input[(input_break + 1):length(input)], '=| ', 3),
                          position = as.numeric(split_vectorised(input[(input_break + 1):length(input)], '=| ', 4)))


apply_instruction = function(current_coordinates, axis, position){
  if(axis == 'x'){
    current_coordinates %>% mutate(
      current_x = ifelse(current_x > position,
                         position + (position - current_x),
                         current_x)) %>% 
      return
  } else if (axis == 'y'){
    current_coordinates %>% mutate(
      current_y = ifelse(current_y > position,
                         position + (position - current_y),
                         current_y)) %>% 
        return
  }
}

working_coordinates = 
  coordinates %>% transmute(starting_x = x, starting_y = y, current_x = x, current_y = y)

working_coordinates %>% distinct(current_x, current_y) %>% count #928

for(instruction_row in 1){
  axis = instructions$axis[instruction_row]
  position = instructions$position[instruction_row]
  working_coordinates = apply_instruction(working_coordinates, axis, position)
}
working_coordinates %>% distinct(current_x, current_y) %>% count #790

working_coordinates = 
  coordinates %>% transmute(starting_x = x, starting_y = y, current_x = x, current_y = y)

for(instruction_row in 1:nrow(instructions)){
  axis = instructions$axis[instruction_row]
  position = instructions$position[instruction_row]
  working_coordinates = apply_instruction(working_coordinates, axis, position)
}

library(ggplot2)

working_coordinates %>% distinct(current_x, current_y) %>%
  ggplot(aes(x = current_x, y = -current_y)) +
  geom_point()
#PGHZBFJC