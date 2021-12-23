# test_input = readLines(con = 'clipboard')
# input = test_input
input = readLines("C:\\Users\\volan\\Documents\\Advent of Code 2021\\22\\input.txt")

split_vectorised = Vectorize(function(text, separators, position){
  unlist(strsplit(text, separators))[position]
})

input_parsed = data.frame(
  change = split_vectorised(input, '[ xyz.,=]+', 1),
  xmin = as.numeric(split_vectorised(input, '[ xyz.,=]+', 2)),
  xmax = as.numeric(split_vectorised(input, '[ xyz.,=]+', 3)),
  ymin = as.numeric(split_vectorised(input, '[ xyz.,=]+', 4)),
  ymax = as.numeric(split_vectorised(input, '[ xyz.,=]+', 5)),
  zmin = as.numeric(split_vectorised(input, '[ xyz.,=]+', 6)),
  zmax = as.numeric(split_vectorised(input, '[ xyz.,=]+', 7)))

dimension_limits = c(101,101,101)
relevant_cubes = array(data = 0, dim = c(101,101,101))
# relevant_cubes[c(), 23:23, 25:60] = 1

map_dimensions = function(input_min, input_max, dimension){
  if(input_max < -50|input_min > 50){
    return(c())
  } else {
    new_min = max(input_min, -50) + 51
    new_max = min(input_max, 50) + 51
    return(new_min:new_max)
  }
}

for(i in 1:nrow(input_parsed)){
  xrange = map_dimensions(input_parsed$xmin[i], input_parsed$xmax[i], 1)
  yrange = map_dimensions(input_parsed$ymin[i], input_parsed$ymax[i], 2)
  zrange = map_dimensions(input_parsed$zmin[i], input_parsed$zmax[i], 3)
  if(input_parsed$change[i] == 'on'){
    new_value = 1
  } else {
    new_value = 0
  }
  relevant_cubes[xrange, yrange, zrange] = new_value
}
sum(relevant_cubes) #588120

# Part 2, with much more complex dimension mapping
# off_instructions = input_parsed$change == 'off'
# x_points_checked = sort(unique(c(input_parsed$xmin, input_parsed$xmax, 
#                      input_parsed$xmin[off_instructions] - 1, 
#                      input_parsed$xmax[off_instructions] + 1)))
# y_points_checked = sort(unique(c(input_parsed$ymin, input_parsed$ymax, 
#                                  input_parsed$ymin[off_instructions] - 1, 
#                                  input_parsed$ymax[off_instructions] + 1)))
# z_points_checked = sort(unique(c(input_parsed$zmin, input_parsed$zmax, 
#                                  input_parsed$zmin[off_instructions] - 1, 
#                                  input_parsed$zmax[off_instructions] + 1)))
# 
# dimension_limits = c(length(x_points_checked),
#                      length(y_points_checked),
#                      length(z_points_checked))
# dimension_map_lists = list(x_points_checked, y_points_checked, z_points_checked)
# new_cubes = array(data = 0, dim = dimension_limits)
# 
# map_dimensions = function(input_min, input_max, dimension){
#   match(input_min, dimension_map_lists[[dimension]]):
#     match(input_max, dimension_map_lists[[dimension]])
# }
# Each new cube represents (a, b] in each dimension

# for(i in 1:nrow(input_parsed)){
#   xrange = map_dimensions(input_parsed$xmin[i], input_parsed$xmax[i], 1)
#   yrange = map_dimensions(input_parsed$ymin[i], input_parsed$ymax[i], 2)
#   zrange = map_dimensions(input_parsed$zmin[i], input_parsed$zmax[i], 3)
#   if(input_parsed$change[i] == 'on'){
#     new_value = 1
#   } else {
#     new_value = 0
#   }
#   new_cubes[xrange, yrange, zrange] = new_value
# }
# 
# sum(new_cubes)
# lapply(input_parsed, summary)
# x_points_mentioned = sort(unique(c(input_parsed$xmin, input_parsed$xmax)))
library(tidyverse)
library(data.table)

reverse_input = input_parsed[nrow(input_parsed):1,] %>%
  mutate(change = ifelse(change == 'on', 1, 0)) %>% data.table 

setkey(reverse_input, xmin, xmax, ymin, ymax, zmin, zmax)
stopCluster(cl)

library(parallel)

cl <- makeCluster(detectCores() - 2)

on_off_status = function(z_value, reverse_instructions){
  answer = head(reverse_instructions[z_value >= reverse_instructions$zmin & 
                                       z_value <= reverse_instructions$zmax,'change'],n = 1)
  if(length(answer) == 0){
    return(0)
  }
  return(answer)
}

get_cubes_for_line = function(y_value, reverse_relevant_instructions){
  reverse_further_relevant_instructions = 
    reverse_relevant_instructions[reverse_relevant_instructions$ymin <= y_value &
                                    reverse_relevant_instructions$ymax >= y_value,]
  z_values = sort(unique(c(reverse_further_relevant_instructions$zmin, 
                           reverse_further_relevant_instructions$zmax)))
  cubes = 0
  for(i in 1:length(z_values)){
    cubes = cubes + on_off_status(z_values[i], reverse_further_relevant_instructions)
    if(i > 1){
      gap = z_values[i] - z_values[i-1] - 1
      if(gap > 0){
        cubes = cubes + gap * on_off_status(z_values[i] - 1, 
                                            reverse_further_relevant_instructions)
      }
    }
  }
  return(cubes)
}

get_cubes_for_plane = function(x_value, reverse_instructions){
  reverse_relevant_instructions = 
    reverse_instructions[reverse_instructions$xmin <= x_value & 
                           reverse_instructions$xmax >= x_value,]
  y_values = sort(unique(c(reverse_relevant_instructions$ymin, 
                           reverse_relevant_instructions$ymax)))
  cubes = 0
  for(i in 1:length(y_values)){
    cubes = cubes + 
      get_cubes_for_line(y_values[i], reverse_relevant_instructions)
    if(i > 1){
      gap = y_values[i] - y_values[i-1] - 1
      if(gap > 0){
        cubes = cubes + gap * get_cubes_for_line(y_values[i] - 1, 
                                            reverse_relevant_instructions)
      }
    }
  }
  return(cubes)
}

get_cubes = function(reversed_input){
  x_values = sort(unique(c(reversed_input$xmin, 
                           reversed_input$xmax)))
  # cubes = 0
  clusterExport(cl, c("x_values", "reversed_input"), envir=environment())
  cube_list = parLapply(cl,
                        1:length(x_values),
                        function(i){
                          cubes = 0
                          cubes = cubes + 
                            get_cubes_for_plane(x_values[i], reversed_input)
                          if(i > 1){
                            gap = x_values[i] - x_values[i-1] - 1
                            if(gap > 0){
                              cubes = cubes + gap * get_cubes_for_plane(x_values[i] - 1, 
                                                                        reversed_input)
                            }
                          }
                          return(cubes)
                        })
  return(cube_list)
  # for(i in 1:length(x_values)){
  #   cubes = cubes + 
  #     get_cubes_for_plane(x_values[i], reversed_input)
  #   if(i > 1){
  #     gap = x_values[i] - x_values[i-1] - 1
  #     if(gap > 0){
  #       cubes = cubes + gap * get_cubes_for_plane(x_values[i] - 1, 
  #                                                 reversed_input)
  #     }
  #   }
  # }
  # return(cubes)
}

clusterExport(cl, c("on_off_status", "get_cubes_for_line", "get_cubes_for_plane"))

cubes = get_cubes(reverse_input)
sprintf('%20.0f', sum(unlist(cubes))) #1134088247046731
