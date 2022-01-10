input = readLines("C:\\Users\\volan\\Documents\\Advent of Code 2021\\24\\input.txt")

split_vectorised = Vectorize(function(text, separators, position){
  unlist(strsplit(text, separators))[position]
})

input_parsed = 
  data.frame(input = input,
             instruction = split_vectorised(input, ' ', 1),
             operand_1 = split_vectorised(input, ' ', 2),
             operand_2 = split_vectorised(input, ' ', 3))

simulate = function(input_vector, commands = input_parsed){
  rows = nrow(commands)
  output = commands
  output$x = 0
  output$y = 0
  output$z = 0
  output$w = 0
  
  x = 0
  y = 0
  z = 0
  w = 0
  
  input_position = 1
  variable_names = c('x', 'y', 'z', 'w')
  for(row in 1:rows){
    instruction = output$instruction[row]
    operand_1 = output$operand_1[row]
    operand_2 = output$operand_2[row]
    
    if(instruction == 'inp'){
      new_value = input_vector[input_position]
      input_position = input_position + 1
      assign(operand_1, new_value)
    } else {
      if(operand_2 %in% variable_names){
        operand_2_parsed = get(operand_2)
      } else {
        operand_2_parsed = as.numeric(operand_2)
      }
      operand_1_parsed = get(operand_1)
      if(instruction == 'add'){
        result = operand_1_parsed + operand_2_parsed
      } else if (instruction == 'mul'){
        result = operand_1_parsed * operand_2_parsed
      } else if (instruction == 'div'){
        result = sign(operand_1_parsed) * (abs(operand_1_parsed) %/% operand_2_parsed)
      } else if (instruction == 'mod'){
        result = operand_1_parsed %% operand_2_parsed
      } else if (instruction == 'eql'){
        if(operand_1_parsed == operand_2_parsed){
          result = 1
        } else {
          result = 0
        }
      } else {
        stop('Unknown instruction')
      }
      assign(operand_1, result)
    }
    
    output$x[row] = x
    output$y[row] = y
    output$z[row] = z
    output$w[row] = w
  }
  return(output)
}

test = simulate(c(9,9,1,9,6,9,9,
                  7,9,8,5,9,4,2)) #99196997985942 is correct #note equals at 7 points

test = simulate(c(1,1,1,9,1,6,2,
                  1,3,1,2,6,1,2)) #11191621312612 is too low #note equals at 6 points

# The first 3 can't be equal so we need to be equal 7/11

test = simulate(c(1, #no effect
                  9, #only affects line 96
                  1, #needs to be 1 for line 60
                  9, #needs to be 9 for line 60
                  6,5, #affects line 186
                  9,
                  7,9,1,5,6,1,1)) #11191621312612 is too low

test[c(which(test$instruction == 'eql' & test$operand_2 == 'w') - 1, 252),]

input_lines = which(test$instruction == 'inp')
code_chunks = lapply(1:14,
                     function(chunk_index){
                       if(chunk_index == 14){
                         input_parsed[input_lines[chunk_index]:nrow(input_parsed),]
                       } else {
                         input_parsed[input_lines[chunk_index]:(input_lines[chunk_index + 1] - 1),]
                       }
                     })

simulate1 = function(input_vector, commands = input_parsed, z_start = 0){
  rows = nrow(commands)
  output = commands
  output$x = 0
  output$y = 0
  output$z = z_start
  output$w = 0
  
  x = 0
  y = 0
  z = z_start
  w = 0
  
  input_position = 1
  variable_names = c('x', 'y', 'z', 'w')
  for(row in 1:rows){
    instruction = output$instruction[row]
    operand_1 = output$operand_1[row]
    operand_2 = output$operand_2[row]
    
    if(instruction == 'inp'){
      new_value = input_vector[input_position]
      input_position = input_position + 1
      assign(operand_1, new_value)
    } else {
      if(operand_2 %in% variable_names){
        operand_2_parsed = get(operand_2)
      } else {
        operand_2_parsed = as.numeric(operand_2)
      }
      operand_1_parsed = get(operand_1)
      if(instruction == 'add'){
        result = operand_1_parsed + operand_2_parsed
      } else if (instruction == 'mul'){
        result = operand_1_parsed * operand_2_parsed
      } else if (instruction == 'div'){
        result = sign(operand_1_parsed) * (abs(operand_1_parsed) %/% operand_2_parsed)
      } else if (instruction == 'mod'){
        result = operand_1_parsed %% operand_2_parsed
      } else if (instruction == 'eql'){
        if(operand_1_parsed == operand_2_parsed){
          result = 1
        } else {
          result = 0
        }
      } else {
        stop('Unknown instruction')
      }
      assign(operand_1, result)
    }
    
    output$x[row] = x
    output$y[row] = y
    output$z[row] = z
    output$w[row] = w
  }
  return(output)
}

simulate_and_get_z = function(input, z_start, code_chunk_index){
  Vectorize(function(input_vector, commands, z_start){
    simulate1(input_vector, commands, z_start)$z[[18]]
  }, vectorize.args = c("input_vector", "z_start"))(
    input_vector = input, commands = code_chunks[[code_chunk_index]], z_start = z_start
  )
}
  
after_1_options = data.frame(input = 1:9, 
                             z_result = simulate_and_get_z(1:9, 0, 1))

after_options = list()
after_options[[1]] = after_1_options

library(tidyverse)

for(i in 2:7){
  current_options = after_options[[i-1]]
  new_options_list = lapply(1:9, function(new_input){
    current_options %>% mutate(new_input = new_input)})
  new_options_df = do.call(rbind, new_options_list)
  new_options = new_options_df %>% mutate(input = input * 10 + new_input, 
                                            new_z_result = simulate_and_get_z(new_input,
                                                                              z_result,
                                                                              i))
  new_options_short = new_options %>% transmute(input, z_result = new_z_result) %>%
    group_by(z_result) %>% summarise(input = min(input), .groups = 'drop')
  after_options[[i]] = new_options_short
}

after_options_backup = after_options

for(i in 8:14){
  current_options = after_options[[i-1]] %>%
    filter(z_result < 1000000)
  new_options_list = lapply(1:9, function(new_input){
    current_options %>% mutate(new_input = new_input)})
  new_options_df = do.call(rbind, new_options_list)
  new_options = new_options_df %>% mutate(input = input * 10 + new_input, 
                                          new_z_result = simulate_and_get_z(new_input,
                                                                            z_result,
                                                                            i))
  new_options_short = new_options %>% transmute(input, z_result = new_z_result) %>%
    group_by(z_result) %>% summarise(input = min(input), .groups = 'drop')
  after_options[[i]] = new_options_short
}


after_options[[14]] %>% filter(z_result == 0) %>% pull(input) %>% 
  sprintf('%14.f', .)
