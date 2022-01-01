library(tidyverse)

amphipod_cost_mapping = c(A = 1, B = 10, C = 100, D = 1000)

starting_position_bigger = list(
  corridor = data.frame(position = c(0, 1, 3, 5, 7, 9, 10), contents = '.'),
  rooms = data.frame(position = c(2,4,6,8), 
                     data = c('ADDD', 'CCBD', 'BBAB', 'AACC')))

update_position_move_string = function(position, move_string){
  rooms = position$rooms
  corridor = position$corridor
  start = as.numeric(unlist(strsplit(move_string, '-'))[1])
  end = as.numeric(unlist(strsplit(move_string, '-'))[2])
  
  room_numbers = c(2, 4, 6, 8)
  from_room = start %in% room_numbers
  to_room = end %in% room_numbers
  
  if(from_room){
    room_string = rooms[rooms$position == start, "data"]
    split_room_string = unlist(strsplit(room_string, ''))
    from_room_depth = 1 + sum(split_room_string == '.')
    moving_object = split_room_string[from_room_depth]
    split_room_string[from_room_depth] = '.'
    rooms[rooms$position == start, "data"] = paste0(split_room_string, collapse = '')
  } else {
    moving_object = corridor[corridor$position == start, "contents"]
    from_room_depth = 0
    corridor[corridor$position == start, "contents"] = '.'
  }
  
  if(to_room){
    room_string = rooms[rooms$position == end, "data"]
    split_room_string = unlist(strsplit(room_string, ''))
    to_room_depth = sum(split_room_string == '.')
    split_room_string[to_room_depth] = moving_object
    rooms[rooms$position == end, "data"] = paste0(split_room_string, collapse = '')
  } else {
    to_room_depth = 0
    corridor[corridor$position == end, "contents"] = moving_object
  }
  moving_distance = abs(start - end) + from_room_depth + to_room_depth
  cost = amphipod_cost_mapping[moving_object] * moving_distance
  
  new_position = list(corridor = corridor, rooms = rooms)
  return(list(new_position = new_position, cost = cost))
}

print_position = function(position){
  rooms = position$rooms
  corridor = position$corridor
  room_size = max(nchar(position$rooms$data))
  output_string = paste0(rep('#', 13), collapse = '')
  output_string[2] = paste0('#',
                            corridor[corridor$position == 0, 'contents'],
                            corridor[corridor$position == 1, 'contents'],
                            '.',
                            corridor[corridor$position == 3, 'contents'],
                            '.',
                            corridor[corridor$position == 5, 'contents'],
                            '.',
                            corridor[corridor$position == 7, 'contents'],
                            '.',
                            corridor[corridor$position == 9, 'contents'],
                            corridor[corridor$position == 10, 'contents'],
                            '#'
                            )
  output_string[3] = paste0('###',
                            substr(rooms[rooms$position == 2, 'data'], 1, 1), '#',
                            substr(rooms[rooms$position == 4, 'data'], 1, 1), '#',
                            substr(rooms[rooms$position == 6, 'data'], 1, 1), '#',
                            substr(rooms[rooms$position == 8, 'data'], 1, 1), 
                            '###')
  for(n in 2:room_size){
    output_string[n + 2] = paste0('  #',
                                  substr(rooms[rooms$position == 2, 'data'], n, n), '#',
                                  substr(rooms[rooms$position == 4, 'data'], n, n), '#',
                                  substr(rooms[rooms$position == 6, 'data'], n, n), '#',
                                  substr(rooms[rooms$position == 8, 'data'], n, n), 
                                  '#  ')
  }
  output_string = c(output_string, '  #########  ')
  cat(paste0(output_string, collapse = '\n'))
}

print_position(starting_position_bigger)

output_state = function(moves){
  position = starting_position_bigger
  cost = 0
  for(move in moves){
    update = update_position_move_string(position, move)
    position = update$new_position
    cost = cost + update$cost
  }
  print_position(position)
  print(cost)
}

output_state(c())

output_state(c('4-10',
               '4-0',
               '4-3',
               '4-9',
               '3-4',
               '6-4',
               '6-4',
               '6-1',
               '6-4',
               '8-3',
               '8-5',
               '8-6',
               '8-6',
               '9-8',
               '10-6'))

output_state(c('6-10',
               '6-9',
               '6-0',
               '6-1',
               '4-6',
               '4-6',
               '4-7',
               '4-3',
               '7-4',
               '9-4',
               '10-4',
               '8-10',
               '8-9',
               '8-6',
               '8-6',
               '3-8',
               '1-4',
               '2-1',
               '2-8',
               '2-8',
               '2-8',
               '1-2',
               '0-2',
               '9-2',
               '10-2'))


output_state(c('6-7',
               '6-9',
               '6-0',
               '6-1',
                '4-6',
                '4-6',
                '4-5',
                '4-3',
                '5-4',
                '7-4',
                '9-4',
                '8-10',
                '8-9',
                '8-6',
                '8-6',
                '3-8',
                '1-4',
                '2-1',
                '2-8',
                '2-8',
                '2-8',
                '1-2',
                '0-2',
                '9-2',
                '10-2'
               ))
