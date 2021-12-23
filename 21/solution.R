input = readLines("C:\\Users\\volan\\Documents\\Advent of Code 2021\\21\\input.txt")
starting_position_1 = 4
starting_position_2 = 8


starting_position_1 = 6
starting_position_2 = 7

increment_die = function(die){
  ifelse(die == 100, 1, die + 1)
}
update_position = function(position, die_sum){
  (position + die_sum - 1) %% 10 + 1
}

die = 100
has_winner = FALSE
position = c(starting_position_1, starting_position_2)
score = c(0,0)
rolls = 0
player = 1
while(!has_winner){
  rolls = rolls + 3
  die = increment_die(die)
  die_sum = die
  die = increment_die(die)
  die_sum = die_sum + die
  die = increment_die(die)
  die_sum = die_sum + die
  position[player] = update_position(position[player], die_sum)
  score[player] = score[player] + position[player]
  if(score[player] >= 1000){
    has_winner = TRUE
    winner = player
  }
  player = 3-player
}

rolls * min(score) #921585

#Part 2: work backwards
# We want to fill in the table of # universes where score1 = X, score2 = Y, 
# position1 = U, position2 = V, player = 1/2
# Scores can go up to 30, so we have 900 * 100 * 2 = 180000 spaces to fill, not too hard

# Distributions of die sums:
# 3: 1/27, 4: 3/27, 5: 6/27, 6: 7/27, 7: 6/27, 8: 3/27, 9: 1/27

die_sums = 3:9
die_sum_worlds = c(1, 3, 6, 7, 6, 3, 1)

library(tidyverse)
win_score = 21
max_score = 30

options = data.frame(
  score1 = 0, score2 = 0, position1 = starting_position_1, position2 = starting_position_2,
  player = 1, worlds = 1
)

previous_position = function(position, die_sum){
  (position - die_sum + 9) %% 10 + 1
}

get_positions_possibilities = function(current_score1, current_score2, positions){
  position_1 = positions %% 10 + 1
  position_2 = positions %/% 10 + 1
  # working with player 1
  possibilities = 0
  if(current_score2 < win_score){
    for(sum_index in 1:7){
      die_sum = die_sums[sum_index]
      worlds_count = die_sum_worlds[sum_index]
      previous_position_1 = previous_position(position_1, die_sum)
      previous_score_1 = current_score1 - position_1
      if(previous_score_1 >= 0 & previous_score_1 < win_score){
        previous_options = options %>% filter(player == 1, 
                                              score1 == previous_score_1,
                                              score2 == current_score2, 
                                              position1 == previous_position_1,
                                              position2 == position_2) %>%
          pull(worlds)
        if(length(previous_options) > 0){
          possibilities = possibilities + previous_options * worlds_count
        }
      }
    }
  }
  if(possibilities > 0){
    new_df_player1 = data.frame(score1 = current_score1, score2 = current_score2, 
                        position1 = position_1, position2 = position_2,
                        player = 2, worlds = possibilities)
  } else {
    new_df_player1 = data.frame()
  }
  # working with player 2
  possibilities = 0
  if(current_score1 < win_score){
    for(sum_index in 1:7){
      die_sum = die_sums[sum_index]
      worlds_count = die_sum_worlds[sum_index]
      previous_position_2 = previous_position(position_2, die_sum)
      previous_score_2 = current_score2 - position_2
      if(previous_score_2 >= 0 & previous_score_2 < win_score){
        previous_options = options %>% filter(player == 2, 
                                              score1 == current_score1,
                                              score2 == previous_score_2, 
                                              position1 == position_1,
                                              position2 == previous_position_2) %>%
          pull(worlds)
        if(length(previous_options) > 0){
          possibilities = possibilities + previous_options * worlds_count
        }
      }
    }
  }
  if(possibilities > 0){
    new_df_player2 = data.frame(score1 = current_score1, score2 = current_score2, 
                                position1 = position_1, position2 = position_2,
                                player = 1, worlds = possibilities)
  } else {
    new_df_player2 = data.frame()
  }
  return(rbind(new_df_player1, new_df_player2))
}

for(total_score in 1:(2*max_score)){
  message(paste0('considering scenarios with total score ', total_score))
  score1_options = max(1, total_score - max_score):min(max_score, total_score)
  all_new_options_list = 
    lapply(score1_options,
         function(score1){
           score2 = total_score - score1
           if(score1 >= win_score & score2 >= win_score){
             return(data.frame())
           }
           message(paste0('considering scenarios with score1 ', score1, ' and score2 ', score2))
           position_options = 
             lapply(0:99,
                    function(number){
                      get_positions_possibilities(score1, score2, number)
                    })
           new_options = do.call(rbind, position_options)
           return(new_options)
         })
  all_new_options = do.call(rbind, all_new_options_list)
  options = rbind(options, all_new_options)
}

options %>% filter(score1 >= 21 | score2 >= 21) %>% group_by(player) %>%
  summarise(total_worlds = sum(worlds)) %>%
  mutate(total_worlds_char= sprintf('%15.0f', total_worlds))
options(digits=20)

#Performance is atrocious (but not bad enough to be infeasible) - much better to have an array and go forwards in the loop rather than go backwards
# Even better strategy - joins!!