input = readLines("C:\\Users\\volan\\Documents\\Advent of Code 2021\\18\\input.txt")

#Let's try storing these numbers as lists of (ordered) lists. Exploding will be a pain but oh well
parse_expression = function(character_vector){
  if(!any(character_vector == ',')){
    return(strtoi(paste0(character_vector, collapse = ''), base = 10))
  } else {
    return(parse_snailfish_number(paste0(character_vector, collapse = '')))
  }
}

parse_snailfish_number = function(string_input){
  #take off leading single '[' and trailing single ']'
  character_vector = unlist(strsplit(string_input, ''))
  character_vector = character_vector[2:(length(character_vector) - 1)]
  nesting_depth = cumsum(character_vector == '[') - cumsum(character_vector == ']')
  comma_position = which(character_vector == ',' & nesting_depth == 0)
  first_expression_character_vector = character_vector[1:(comma_position - 1)]
  second_expression_character_vector = character_vector[(comma_position + 1):length(character_vector)]
  return(list(left = parse_expression(first_expression_character_vector),
              right = parse_expression(second_expression_character_vector)))
}

snailfish_number_to_string = function(snailfish_number){
  expression_to_string = function(component){
    if(is.numeric(component)){
      return(as.character(component))
    } else {
      return(snailfish_number_to_string(component))
    }
  }
  return(paste0('[',
                expression_to_string(snailfish_number$left),
                ',',
                expression_to_string(snailfish_number$right),
                ']'))
}

# snailfish_number_to_string(parse_snailfish_number('[[1,2],3]'))

try_explode_snailfish_number = function(snailfish_number, depth = 1){
  try_add_leftmost = function(snailfish_number, to_add){
    return_snailfish_number = snailfish_number
    if(is.numeric(return_snailfish_number)){
      return_snailfish_number = return_snailfish_number + to_add
    } else {
      return_snailfish_number$left = try_add_leftmost(return_snailfish_number$left, to_add)
    }
    return(return_snailfish_number)
  }
  
  try_add_rightmost = function(snailfish_number, to_add){
    return_snailfish_number = snailfish_number
    if(is.numeric(return_snailfish_number)){
      return_snailfish_number = return_snailfish_number + to_add
    } else {
      return_snailfish_number$right = try_add_rightmost(return_snailfish_number$right, to_add)
    }
    return(return_snailfish_number)
  }
  
  #returns a list with up to 4 items
  #add_left (number)
  #add_right (number)
  #changed (boolean - not present if no change)
  #new_snailfish_number - if all changes done in nested stages
  return_value = list()
  
  if(is.numeric(snailfish_number)){ #do nothing if it's just a literal number
    return(return_value)
  } 
  
  if(depth > 4){ #explode
    return_value$add_left = snailfish_number$left
    return_value$add_right = snailfish_number$right
    return_value$changed = TRUE
    return_value$new_snailfish_number = 0
    return(return_value)
  }
  return_value$new_snailfish_number = snailfish_number
  left_try = try_explode_snailfish_number(snailfish_number$left, depth = depth + 1)
  if('add_right' %in% names(left_try)){
    return_value$new_snailfish_number$right = try_add_leftmost(return_value$new_snailfish_number$right,
                                                               left_try$add_right)
    return_value$changed = TRUE
  }
  if('add_left' %in% names(left_try)){
    return_value$add_left = left_try$add_left
  }
  if('changed' %in% names(left_try)){
    return_value$new_snailfish_number$left = left_try$new_snailfish_number
    return_value$changed = TRUE
  }
  if('changed' %in% names(return_value)){
    return(return_value)
  }
  
  right_try = try_explode_snailfish_number(snailfish_number$right, depth = depth + 1)
  if('add_right' %in% names(right_try)){
    return_value$add_right = right_try$add_right
  }
  if('add_left' %in% names(right_try)){
    return_value$new_snailfish_number$left = try_add_rightmost(return_value$new_snailfish_number$left,
                                                               right_try$add_left)
    return_value$changed = TRUE
  }
  if('changed' %in% names(right_try)){
    return_value$new_snailfish_number$right = right_try$new_snailfish_number
    return_value$changed = TRUE
  }
  if('changed' %in% names(return_value)){
    return(return_value)
  }
  return(return_value)
  
}

try_explode_snailfish_number(parse_snailfish_number('[[[[[9,8],1],2],3],4]'))

try_explode_snailfish_number(parse_snailfish_number('[1,2]'))

snailfish_number_to_string(
  try_explode_snailfish_number(parse_snailfish_number('[7,[6,[5,[4,[3,2]]]]]'))$new_snailfish_number)


snailfish_number_to_string(
  try_explode_snailfish_number(parse_snailfish_number('[[6,[5,[4,[3,2]]]],1]'))$new_snailfish_number)

try_split_snailfish_number = function(snailfish_number){
  #returns changed and new_snailfish_number
  return_value = list()
  return_value$new_snailfish_number = snailfish_number
  if(is.numeric(snailfish_number)){
    if(snailfish_number >= 10){
      return_value$new_snailfish_number = list(left = floor(snailfish_number/2),
                                  right = ceiling(snailfish_number/2))
      return_value$changed = TRUE
      return(return_value)
    } 
    return(return_value)
  }
  try_split_left = try_split_snailfish_number(snailfish_number$left)
  if('changed' %in% names(try_split_left)){
    return_value$changed = TRUE
    return_value$new_snailfish_number$left = try_split_left$new_snailfish_number
    return(return_value)
  }
  try_split_right = try_split_snailfish_number(snailfish_number$right)
  if('changed' %in% names(try_split_right)){
    return_value$changed = TRUE
    return_value$new_snailfish_number$right = try_split_right$new_snailfish_number
    return(return_value)
  }
  return(return_value)
}

try_split_snailfish_number(parse_snailfish_number('[6,11]'))

snailfish_number_to_string(
  try_split_snailfish_number(parse_snailfish_number('[10,11]'))$new_snailfish_number)


reduce_snailfish_number = function(snailfish_number){
  working_snailfish_number = snailfish_number
  further_reduction_needed = TRUE
  while(further_reduction_needed){
    try_explode = try_explode_snailfish_number(working_snailfish_number)
    if('changed' %in% names(try_explode)){
      working_snailfish_number = try_explode$new_snailfish_number
    } else {
      try_split = try_split_snailfish_number(working_snailfish_number)
      if('changed' %in% names(try_split)){
        working_snailfish_number = try_split$new_snailfish_number
      } else {
        further_reduction_needed = FALSE
      }
    }
  }
  return(working_snailfish_number)
}

snailfish_number_to_string(
  reduce_snailfish_number(parse_snailfish_number('[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]')))

add_and_reduce_snailfish_number = function(first_snailfish_number, second_snailfish_number){
  sum = list(left = first_snailfish_number, right = second_snailfish_number)
  return(reduce_snailfish_number(sum))
}

snailfish_number_to_string(
  add_and_reduce_snailfish_number(parse_snailfish_number('[[[[4,3],4],4],[7,[[8,4],9]]]'),
                                parse_snailfish_number('[1,1]')))

get_magnitude = function(snailfish_number){
  if(is.numeric(snailfish_number)){
    return(snailfish_number)
  }
  return(3 * get_magnitude(snailfish_number$left) + 
           2 * get_magnitude(snailfish_number$right))
}

get_magnitude(parse_snailfish_number('[[1,2],[[3,4],5]]'))

input = readLines(con = 'clipboard')

parsed_snailfish_numbers = lapply(input, parse_snailfish_number)

working_snailfish_number = parsed_snailfish_numbers[[1]]
for(n in 2:length(parsed_snailfish_numbers)){
  message(paste0('processing number ', n, 
                 ' previous length is ', length(unlist(working_snailfish_number))))
  working_snailfish_number = add_and_reduce_snailfish_number(
    working_snailfish_number,
    parsed_snailfish_numbers[[n]]
  )
  message(  snailfish_number_to_string(working_snailfish_number))
}
get_magnitude(working_snailfish_number) #4469
snailfish_number_to_string(add_and_reduce_snailfish_number(
  parse_snailfish_number('[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]'),
  parse_snailfish_number('[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]')
))

try_add_dataframe = data.frame(first_item = rep(1:100, times = 100),
                               second_item = rep(1:100, each = 100))
try_add_dataframe = try_add_dataframe[try_add_dataframe$first_item != try_add_dataframe$second_item,]

try_add_dataframe$magnitudes = mapply(function(index_1, index_2){
  get_magnitude(add_and_reduce_snailfish_number(
    parsed_snailfish_numbers[[index_1]], 
    parsed_snailfish_numbers[[index_2]]
  ))
}, try_add_dataframe$first_item, try_add_dataframe$second_item)

try_add_dataframe$magnitudes = 0
for(n in 1:nrow(try_add_dataframe)){
  message(paste0('processing number ', n))
  try_add_dataframe$magnitudes[n] = get_magnitude(add_and_reduce_snailfish_number(
    parsed_snailfish_numbers[[try_add_dataframe$first_item[n]]], 
    parsed_snailfish_numbers[[try_add_dataframe$second_item[n]]]
  ))
}
try_add_dataframe_backup = try_add_dataframe
try_add_dataframe = try_add_dataframe_backup
max(try_add_dataframe$magnitudes) #4770
