input = readLines("C:\\Users\\volan\\Documents\\Advent of Code 2021\\10\\input.txt")

opening_characters = c('(','[', '{', '<')
closing_characters = c(')', ']', '}', '>')

get_error_from_line = function(line){
  line_as_characters = unlist(strsplit(line, ''))
  stack_so_far = c()
  for(character in line_as_characters){
    if(character %in% opening_characters){
      stack_so_far = c(stack_so_far, character)
    } else if (character %in% closing_characters){
      corresponding_opening_character = opening_characters[match(character, closing_characters)]
      if(stack_so_far[length(stack_so_far)] == corresponding_opening_character){
        if(length(stack_so_far) == 0){
          return('error - ran out of opening characters')
        } else if (length(stack_so_far) == 1){
          stack_so_far = c()
        } else {
          stack_so_far = stack_so_far[1:(length(stack_so_far) - 1)]
        }
      } else {
        return(paste0('illegal character - ', character))
      }
    }
  }
  return('no illegal character')
}

errors = sapply(input, get_error_from_line)

table(errors)
3 * 9 + 57 * 17 + 1197 * 13 + 25137 * 6


incomplete_lines = input[errors == 'no illegal character']

get_value_for_completing_line = function(line){
  line_as_characters = unlist(strsplit(line, ''))
  stack_so_far = c()
  for(character in line_as_characters){
    if(character %in% opening_characters){
      stack_so_far = c(stack_so_far, character)
    } else if (character %in% closing_characters){
      corresponding_opening_character = opening_characters[match(character, closing_characters)]
      if(stack_so_far[length(stack_so_far)] == corresponding_opening_character){
        if(length(stack_so_far) == 0){
          return('error - ran out of opening characters')
        } else if (length(stack_so_far) == 1){
          stack_so_far = c()
        } else {
          stack_so_far = stack_so_far[1:(length(stack_so_far) - 1)]
        }
      } else {
        return(paste0('illegal character - ', character))
      }
    }
  }
  reversed_stack = stack_so_far[length(stack_so_far):1]
  current_score = 0
  for(character in reversed_stack){
    current_score = current_score * 5
    current_score = current_score + match(character, opening_characters)
  }
  return(current_score)
}

completion_values = sapply(incomplete_lines, get_value_for_completing_line)

median(completion_values) #2776842859

test_value = 0

if(test_value == 0){
  print('test value is 0')
} else if (test_value > 1){
  print('test value is > 1')
} else if (test_value > 3){
  print('test value is > 3')
} else {
  print('other conditions not met')
}
