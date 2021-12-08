input = readLines("C:\\Users\\volan\\Documents\\Advent of Code 2021\\8\\input.txt")

library(tidyverse)

parse_input_line = function(input_line){
  unlist(strsplit(input_line, ' '))
}

parsed_data_frame = do.call(rbind,
        lapply(1:length(input),
               function(line){
          data.frame(
            line = line,
            position = c(1:10, -1:-4),
            string = parse_input_line(input[line])[c(1:10, 12:15)])
        }))

#1 has 2 segments, 4 has 4, 7 has 3 and 8 has 7

parsed_data_frame %>% filter(position < 0) %>%
  count(characters = nchar(string)) %>% filter(characters %in% c(2, 4, 3, 7)) %>%
  summarise(total = sum(n)) #330

string_to_letters = function(string){
  unlist(strsplit(string, ''))
}

is_superset = function(string1, string2){
  all(string_to_letters(string2) %in% string_to_letters(string1))
}

sort_string = function(string){
  paste0(sort(string_to_letters(string)), collapse = "")
}

deduce_strings = function(row_number){
  strings = parsed_data_frame %>%
    filter(line == row_number, position > 0) %>%
    pull(string)
  #a is the one in 7 but not in 1
  string_7 = strings[nchar(strings) == 3]
  string_1 = strings[nchar(strings) == 2]
  a = setdiff(string_to_letters(string_7), string_to_letters(string_1))
  #9 is the only superset of 4 other than 8
  string_4 = strings[nchar(strings) == 4]
  strings_6_chars = strings[nchar(strings) == 6]
  string_9 = strings_6_chars[sapply(strings_6_chars, is_superset, string2 = string_4)]
  #e is in 8 but not 9
  string_8 = strings[nchar(strings) == 7]
  e = setdiff(string_to_letters(string_8), string_to_letters(string_9))
  #3 has 5 characters and is a superset of 7
  strings_5_chars = strings[nchar(strings) == 5]
  string_3 = strings_5_chars[sapply(strings_5_chars, is_superset, string2 = string_7)]
  #2 has 5 characters and contains e
  string_2 = strings_5_chars[grepl(e, strings_5_chars)]
  #5 is the last 5-character string
  string_5 = setdiff(strings_5_chars, c(string_3, string_2))
  #6 is the 6-character string not a superset of 1
  string_6 = strings_6_chars[!sapply(strings_6_chars, is_superset, string2 = string_1)]
  #0 is the last 6-character string
  string_0 = setdiff(strings_6_chars, c(string_9, string_6))
  
  deduced_strings = data.frame(line = row_number,
                               meaning = 0:9,
                               sorted_string = Vectorize(sort_string)(
                                 c(string_0, string_1, string_2, string_3, string_4, 
                                   string_5, string_6, string_7, string_8, string_9)
                               ))
  return(deduced_strings)
}

string_deductions = do.call(rbind,
                            lapply(1:length(input),
                                   deduce_strings))

strings_to_interpret = parsed_data_frame %>% filter(position < 0) %>%
  mutate(sorted_string = Vectorize(sort_string)(string))

interpreted_strings = strings_to_interpret %>% inner_join(string_deductions, by = c("line", "sorted_string"))

# Get the sum of the output values
with(interpreted_strings,
     sum(meaning * (10^(position + 4)))) #1010472


