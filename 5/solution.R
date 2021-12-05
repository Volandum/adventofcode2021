input = readLines("C:\\Users\\volan\\Documents\\Advent of Code 2021\\5\\input.txt")

# Lines

get_part_of_4_part_item = function(input_string, part){
  unlist(strsplit(input_string, ',|( -> )'))[part]
}

get_part_of_vector = function(input, part){
  Vectorize(function(input_string){get_part_of_4_part_item(input_string, part)})(input)
}

input_df = data.frame(input_lines = input,
                      start_x = as.numeric(get_part_of_vector(input, 1)),
                      start_y = as.numeric(get_part_of_vector(input, 2)),
                      end_x = as.numeric(get_part_of_vector(input,3)),
                      end_y = as.numeric(get_part_of_vector(input,4)))

get_df_from_row = function(df_row){
  with(df_row, return(data.frame(x = start_x:end_x, y = start_y:end_y)))
}

horizontal_and_vertical_only = input_df[input_df$start_x == input_df$end_x | input_df$start_y == input_df$end_y, ]

vents_from_horizontal_and_vertical_only_list = lapply(1:nrow(horizontal_and_vertical_only),
                                                      function(row_number){
                                                        get_df_from_row(horizontal_and_vertical_only[row_number,])
                                                      })

vents_from_horizontal_and_vertical_only = do.call(rbind, vents_from_horizontal_and_vertical_only_list)

coordinates_table_from_horizontal_and_vertical_only = table(paste0(vents_from_horizontal_and_vertical_only$x, ',', vents_from_horizontal_and_vertical_only$y))

sum(coordinates_table_from_horizontal_and_vertical_only > 1) #6007


vents_list = lapply(1:nrow(input_df),
                                                      function(row_number){
                                                        get_df_from_row(input_df[row_number,])
                                                      })

vents = do.call(rbind, vents_list)

coordinates_table = table(paste0(vents$x, ',', vents$y))

sum(coordinates_table > 1) #19349
