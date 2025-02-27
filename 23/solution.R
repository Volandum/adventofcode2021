#############
#01.3.5.7.910#
###2#4#6#8###
  #.#.#.#.#
  #########

#Test:
  #############
#...........#
###B#C#B#D###
  #A#D#C#A#
  #########

library(tidyverse)

end_position_test = list(
  corridor = data.frame(position = c(0, 1, 3, 5, 7, 9, 10),
                        contents = c(NA_character_, NA_character_,
                                     NA_character_, NA_character_, NA_character_,
                                     NA_character_, NA_character_)),
  rooms = data.frame(position = c(2,4,6,8), 
                     first = c('A', 'B', 'C', 'D'),
                     second = c('A', 'B', 'C', 'D'),
                     accepting = c(TRUE, TRUE, TRUE, TRUE))
)
basic_position_test = list(
  corridor = data.frame(position = c(0, 1, 3, 5, 7, 9, 10),
                        contents = c('A', NA_character_,
                                     NA_character_, NA_character_, NA_character_,
                                     NA_character_, NA_character_)),
  rooms = data.frame(position = c(2,4,6,8), 
                     first = c(NA, 'B', 'C', 'D'),
                     second = c('A', 'B', 'C', 'D'),
                     accepting = c(TRUE, TRUE, TRUE, TRUE))
)

starting_position_test = list(
  corridor = data.frame(position = c(0, 1, 3, 5, 7, 9, 10), contents = NA_character_),
  rooms = data.frame(position = c(2,4,6,8), 
                     first = c('B', 'C', 'B', 'D'),
                     second = c('A', 'D', 'C', 'A'),
                     accepting = FALSE))
starting_position = starting_position_test
moves = data.frame(start = numeric(0), end = numeric(0), amphipod = character(0), 
                   distance = numeric(0), type = character())
amphipod_room_mapping = c(A = 2, B = 4, C = 6, D = 8)
amphipod_cost_mapping = c(A = 1, B = 10, C = 100, D = 1000)

amphipod_to_room = Vectorize(function(amphipod){
  amphipod_room_mapping[amphipod]
})

not_blocked_move = Vectorize(memoise::memoise(function(start, end, occupied_spaces){
  if(length(occupied_spaces) == 0){
    return(TRUE)
  }
  !(any(occupied_spaces %in% setdiff(start:end, c(start,end))))
}), vectorize.args = c("start", "end"))

if(FALSE){
  not_blocked_move(c(3,1), c(5,3), c(4,6,3))
}

get_legal_moves = function(position){
  current_corridor_positions = position$corridor
  current_room_positions = position$rooms
  occupied_corridor_positions = current_corridor_positions %>% 
    filter(!is.na(contents)) %>% pull(position)
  acceptor_rooms = current_room_positions %>% filter(accepting) %>% 
    transmute(target_room = position, 
              extra_final_move = ifelse(is.na(first), 1, 0))
  
  

  any_rooms_accepting = any(current_room_positions$accepting)
  any_rooms_emitting = !all(current_room_positions$accepting)
  

  if(length(occupied_corridor_positions) == 0 | !any_rooms_accepting){
    corridor_to_room_moves = data.frame()
  } else {
    pre_filter = corridor_to_room_moves = current_corridor_positions %>% 
      filter(!is.na(contents)) %>% 
      mutate(target_room = amphipod_to_room(contents)) %>%
      inner_join(acceptor_rooms, by = "target_room") 
    if(nrow(pre_filter) > 0){
      corridor_to_room_moves = pre_filter %>% 
        filter(not_blocked_move(position, target_room, occupied_corridor_positions)) %>%
        transmute(start = position, end = target_room, 
                  amphipod = contents, distance = abs(start - end) + 1 + extra_final_move,
                  type = 'cr')
    } else {
      corridor_to_room_moves = data.frame()
    }
  }
  
  # if(nrow(corridor_to_room_moves) > 0){
  #   return(corridor_to_room_moves[1,])
  # }
  
  if(any_rooms_accepting & any_rooms_emitting){
    pre_filter = current_room_positions %>% 
      filter(!accepting) %>%
      mutate(lead_amphipod = coalesce(first, second)) %>%
      mutate(extra_start = ifelse(is.na(first), 1, 0)) %>%
      mutate(target_room = amphipod_to_room(lead_amphipod)) %>%
      inner_join(acceptor_rooms, by = "target_room") 
    if(nrow(pre_filter) > 0){
      room_to_room_moves = pre_filter %>%
        filter(not_blocked_move(position, target_room, occupied_corridor_positions)) %>%
        transmute(start = position, end = target_room, 
                  amphipod = lead_amphipod, 
                  distance = abs(start - end) + 2 + extra_start + extra_final_move,
                  type = 'rr')
      
    } else {
      room_to_room_moves = data.frame()
    }
  } else {
    room_to_room_moves = data.frame()
  }
  
  # if(nrow(room_to_room_moves) > 0){
  #   return(room_to_room_moves[1,])
  # }
  
  if(any_rooms_emitting){
    pre_filter = current_room_positions %>% 
    filter(!accepting) %>%
    mutate(lead_amphipod = coalesce(first, second)) %>%
    mutate(extra_start = ifelse(is.na(first), 1, 0)) %>%
    inner_join(current_corridor_positions %>% filter(is.na(contents)) %>%
                 transmute(end = position), by = character())
    if(nrow(pre_filter) > 0){
      room_to_corridor_moves = pre_filter %>%
        filter(not_blocked_move(position, end, occupied_corridor_positions)) %>%
        transmute(start = position, end, 
                  amphipod = lead_amphipod, distance = abs(start - end) + 1 + extra_start,
                  type = 'rc')
    } else {
      room_to_corridor_moves = data.frame()
    }
  } else {
    room_to_corridor_moves = data.frame()
  }
  return(rbind(corridor_to_room_moves, room_to_room_moves, room_to_corridor_moves))
}

if(FALSE){
  get_legal_moves(starting_position)
}

is_complete = function(position){
  current_corridor_positions = position$corridor
  current_room_positions = position$rooms
  occupied_corridor_positions = current_corridor_positions %>% 
    filter(!is.na(contents)) %>% pull(position)
  any_rooms_emitting = !all(current_room_positions$accepting)
  if(!any_rooms_emitting & length(occupied_corridor_positions) == 0){
    return(TRUE)
  }
  return(FALSE)
}

update_position = function(position, move){
  current_corridor_positions = position$corridor
  current_room_positions = position$rooms
  start_position = move$start
  # Move from source point
  if(move$type == 'cr'){
    current_corridor_positions$contents[current_corridor_positions$position == start_position] = NA
  } else (
    # Update accepting status of source room as well if moving from a room
    if(is.na(current_room_positions$first[current_room_positions$position == start_position])){
      current_room_positions$second[current_room_positions$position == start_position] = NA
      current_room_positions$accepting[current_room_positions$position == start_position] = TRUE
    } else {
      current_room_positions$first[current_room_positions$position == start_position] = NA
      if(amphipod_to_room(
        current_room_positions$second[current_room_positions$position == start_position]) ==
        start_position){
        current_room_positions$accepting[current_room_positions$position == start_position] = TRUE
      }
    }
  )
  
  end_position = move$end
  moving_amphipod = move$amphipod
  # Move into destination point
  if(move$type == 'rc'){
    current_corridor_positions$contents[current_corridor_positions$position == end_position] =
      moving_amphipod
  } else {
    if(is.na(current_room_positions$second[current_room_positions$position == end_position])){
      current_room_positions$second[current_room_positions$position == end_position] =
        moving_amphipod
    } else {
      current_room_positions$first[current_room_positions$position == end_position] =
        moving_amphipod
    }
  }
  return(list(corridor = current_corridor_positions, rooms = current_room_positions))
}

get_move_cost = function(move){
  amphipod_cost_mapping[move$amphipod]*move$distance
}

games_from_point = function(position, depth){
  message(paste0('depth at ', depth))
  if(is_complete(position)){
    return(TRUE)
  }
  legal_moves = get_legal_moves(position)
  if(nrow(legal_moves) == 0){
    return(FALSE)
  }
  list_of_moves = split(legal_moves, 1:nrow(legal_moves))
  new_positions = lapply(1:nrow(legal_moves), function(index){
    update_position(position, list_of_moves[[index]])
  })
  has_ways_to_go = FALSE
  games_as_df = lapply(1:nrow(legal_moves), function(index){
    #gets lists of moves for subsequent positions
    new_position = new_positions[[index]]
    subsequent_games = games_from_point(new_position, depth + 1)
    if(!is.list(subsequent_games)){
      if(subsequent_games){
        has_ways_to_go = TRUE
        return(list(move = list_of_moves[[index]], done = TRUE))
      } else {
        return(list())
      }
    }
    has_ways_to_go = TRUE
    return(list(move = list_of_moves[[index]],
                done = FALSE,
                subsequent = subsequent_games))
  })
  list_of_games = games_as_df
  if(!has_ways_to_go){
    return(FALSE)
  }
  return(list_of_games)
}

x = games_from_point(starting_position_test, 0)


encode_position_as_string = function(position){
  rbind(position$corridor %>% 
          transmute(position, string = ifelse(is.na(contents), '.', contents)),
        position$rooms %>% 
          mutate(first_with_dots = ifelse(is.na(first), '.', first),
                 second_with_dots = ifelse(is.na(second), '.', second)) %>%
          transmute(position, string = paste0(first_with_dots, second_with_dots))) %>%
    arrange(position) %>% pull(string) %>%
    paste0(collapse = '') %>%
    paste0('P',.)
  }

positions_to_check = data.frame(position = character(0), min_cost = numeric(0))
positions_env = new.env()
finished = FALSE
# positions here will be a named list with:
# Position as the position
# Cost as the lowest observed cost
# Moves as a set of moves realising the lowest observed cost

add_position = function(position, cost, moves){
  position_string = encode_position_as_string(position)
  if(position_string %in% positions_to_check$position){
    old_cost = positions_to_check[position == position_string,"min_cost"]
    if(old_cost > cost){
      positions_to_check[position == position_string,"min_cost"] = cost
    }
  }
  if(position_string %in% names(positions_env)){
    if(positions_env[[position_string]]$cost > cost){
      positions_env[[position_string]]$cost = cost
      positions_env[[position_string]]$moves = moves
    }
  } else {
    positions_env[[position_string]] = list(
      position = position,
      cost = cost,
      moves = moves
    )
    positions_to_check <<- rbind(positions_to_check,
                               data.frame(position = position_string,
                                          min_cost = cost))
  }
}

add_position(starting_position_test, 0, data.frame())

while(!finished){
  cheapest_position_to_check = positions_to_check %>% 
    arrange(min_cost) %>% head(1) %>% pull(position)
  actual_position = positions_env[[cheapest_position_to_check]]$position
  if(is_complete(position)){
    finished = TRUE
    break()
  }
  moves_to_position = positions_env[[cheapest_position_to_check]]$moves
  cost_to_position = positions_env[[cheapest_position_to_check]]$cost
  
  moves_from_position = get_legal_moves(actual_position)
  if(nrow(moves_from_position) > 0){
    
  }
}

# #############
# #01.3.5.7.9X#
# ###2#4#6#8###
# #.#.#.#.#
# #########
# 
# 6-X (5B)
# 6-9 (5B)
# 6-0 (9A)
# 6-1 (9B)
# 4-6 (7C)
# 4-6 (7C)
# 4-7 (6B)
# 4-3 (5D)
# 7-4 (7B)
# 9-4 (8B)
# X-4 (8B)
# 8-X (3A)
# 8-9 (3A)
# 8-6 (7C)
# 8-6 (7C)
# 3-8 (9D)
# 1-4 (4B)
# 2-1 (2A)
# 2-8 (11D)
# 2-8 (11D)
# 2-8 (11D)
# 1-2 (5A)
# 0-2 (5A)
# 9-2 (9A)
# X-2 (9A)

