input = readLines(con = 'clipboard')

input = readLines("C:\\Users\\volan\\Documents\\Advent of Code 2021\\19\\input.txt")

input_scanner_positions = which(grepl('scanner', input))

input_scanner_coordinates = 
  lapply(1:length(input_scanner_positions),
         function(scanner_index){
           data.frame(scanner = scanner_index, 
                      coordinates = if(scanner_index == length(input_scanner_positions)){
                        input[(input_scanner_positions[scanner_index]+1):length(input)]
                      } else {
                        input[(input_scanner_positions[scanner_index]+1):
                                (input_scanner_positions[scanner_index+1]-2)]
                      })
         })

library(tidyverse)

input_scanner_dataframe  = do.call(rbind, input_scanner_coordinates)

split_vectorised = Vectorize(function(text, separators, position){
  unlist(strsplit(text, separators))[position]
})

input_scanner_dataframe = input_scanner_dataframe %>%
  mutate(coord_1 = as.numeric(split_vectorised(coordinates, ',', 1)),
         coord_2 = as.numeric(split_vectorised(coordinates, ',', 2)),
         coord_3 = as.numeric(split_vectorised(coordinates, ',', 3)),
         row = row_number())

# dataframe_component = input_scanner_dataframe %>% filter(scanner == 23)

invariant_as_string = Vectorize(function(diff1, diff2, diff3){
  paste0(as.character(sort(abs(c(diff1, diff2, diff3)))), collapse = ',')
})

get_invariants = function(dataframe_component){
  coordinates = dataframe_component %>% 
    transmute(scanner, row,
              coord_1, coord_2, coord_3)
  self_joined_coordinates = 
    coordinates %>% inner_join(coordinates, by = "scanner", suffix = c('a', 'b')) %>%
    filter(rowa != rowb) %>%
    mutate(coord1_diff = coord_1a - coord_1b,
           coord2_diff = coord_2a - coord_2b,
           coord3_diff = coord_3a - coord_3b) %>%
    mutate(invariant = invariant_as_string(coord1_diff, coord2_diff, coord3_diff))
  return(list(invariants_short = self_joined_coordinates %>% select(scanner, rowa, rowb, invariant) %>%
                filter(rowa < rowb),
              invariants_full = self_joined_coordinates))
}

invariants = get_invariants(input_scanner_dataframe)$invariants_short
invariants_full = get_invariants(input_scanner_dataframe)$invariants_full

number_of_scanners = length(input_scanner_positions)


is_connected = function(scanner_1, scanner_2, invariants){
  joined_objects = inner_join(invariants %>% filter(scanner == scanner_1),
                              invariants %>% filter(scanner == scanner_2), 
                              by = c("invariant"), suffix = c("_1", "_2"))
  # Let's assume we don't have inconsistencies
  common_objects = length(unique(c(joined_objects$rowa_1, joined_objects$rowb_1)))
  return(ifelse(common_objects >= 12, TRUE, FALSE))
}

get_transformation_matrix = function(matrix_1, matrix_2){
  matrix = matrix(rep(0, 9), nrow = 3)
  for(i in 1:3){
    for(j in 1:3){
      if(all(matrix_1[,i] == matrix_2[,j])){
        matrix[i,j] = 1
      } else if(all(matrix_1[,i] == -1 * matrix_2[,j])){
        matrix[i,j] = -1
      }
    }
  }
  return(matrix)
}

get_relative_position = function(scanner_1, scanner_2){
  # the idea is that scanner_2 coords are equal to scanner_1 coords * transformation_matrix + shift
  joined_objects = inner_join(invariants_full %>% filter(scanner == scanner_1),
                              invariants_full %>% filter(scanner == scanner_2), 
                              by = c("invariant"), suffix = c("_1", "_2"))
  # Let's assume we don't have inconsistencies
  common_objects_1 = unique(c(joined_objects$rowa_1, joined_objects$rowb_1))
  common_objects_2 = unique(c(joined_objects$rowa_2, joined_objects$rowb_2))
  paired_vertices = joined_objects %>% count(rowa_1, rowa_2) %>% filter(n>1) %>% ungroup() %>% 
    transmute(scan_1 = scanner_1, item_1 = rowa_1,
              scan_2 = scanner_2, item_2 = rowa_2)
  proto_matrix = joined_objects %>% select(scan_1 = scanner_1, scan_2 = scanner_2, item_1 = rowa_1, item_2 = rowa_2,
                            coord1_diff_1, coord2_diff_1, coord3_diff_1, coord1_diff_2, coord2_diff_2, coord3_diff_2) %>%
    inner_join(paired_vertices, by = c("scan_1", "item_1", "scan_2", "item_2"))
  proto_matrix %>% select(coord1_diff_1, coord2_diff_1, coord3_diff_1) %>%
    as.matrix() -> matrix_1
  proto_matrix %>% select(coord1_diff_2, coord2_diff_2, coord3_diff_2) %>% 
    as.matrix() -> matrix_2
  transformation_matrix = get_transformation_matrix(matrix_1, matrix_2)
  pair_of_vertices = paired_vertices %>% head(1) %>% 
    inner_join(input_scanner_dataframe %>% transmute(scan_1 = scanner, item_1 = row,
                                         coord1_1 = coord_1,
                                         coord2_1 = coord_2,
                                         coord3_1 = coord_3)) %>%
    inner_join(input_scanner_dataframe %>% transmute(scan_2 = scanner, item_2 = row,
                                         coord1_2 = coord_1,
                                         coord2_2 = coord_2,
                                         coord3_2 = coord_3))
  starting_coords = with(pair_of_vertices, c(coord1_1, coord2_1, coord3_1))
  end_coords = with(pair_of_vertices, c(coord1_2, coord2_2, coord3_2))
  transformed_starting_coords = starting_coords %*% transformation_matrix
  shift = end_coords - transformed_starting_coords
  return(list(transformation_matrix = transformation_matrix, shift = shift))
}


# tested_pairs %>% filter(connected == 1)

# I think we want transformations for each scanner relative to the first
# in terms of transformation_matrix and shift

compose_transformation = function(transformation_1, transformation_2){
  #multiply by first matrix, apply shift 1, multiply by second matrix, apply shift 2
  return(list(transformation_matrix = transformation_1$transformation_matrix %*% 
                transformation_2$transformation_matrix,
              shift = transformation_1$shift %*% 
                transformation_2$transformation_matrix +
                transformation_2$shift))
}

invert_transformation = function(transformation){
  # x - > xA + b -> xAA^(-1) + bA(^-1) - bA^(-1)
  return(list(transformation_matrix = solve(transformation$transformation_matrix),
              shift = -1 * transformation$shift %*% solve(transformation$transformation_matrix)))
}

# Loop through, fill out the tree and as we do it also get the transformations from base
current_component = c(1)
tested_pairs = data.frame(item_1 = rep(1:number_of_scanners, times = number_of_scanners),
                          item_2 = rep(1:number_of_scanners, each = number_of_scanners)) %>%
  filter(item_1 < item_2) %>%
  mutate(tested = FALSE, connected = FALSE)
transformations = list()
transformations[['1']] = 
  list(transformation_matrix = diag(3), shift = c(0,0,0))

while(length(current_component) < number_of_scanners){
  for(row in 1:nrow(tested_pairs)){
    item_1 = tested_pairs$item_1[row]
    item_2 = tested_pairs$item_2[row]
    tested = tested_pairs$tested[row]
    if((item_1 %in% current_component & item_2 %in% current_component) | 
       tested == TRUE |
       !(item_1 %in% current_component | item_2 %in% current_component)
    ){
      #do nothing
    } else {
      test = is_connected(item_1, item_2, invariants)
      tested_pairs$tested[row] = TRUE
      tested_pairs$connected[row] = test
      if(test){
        if(item_1 %in% current_component){
          old_item = item_1
          new_item = item_2
        } else if (item_2 %in% current_component){
          old_item = item_2
          new_item = item_1
        }
        current_component = c(current_component, new_item)
        new_transformation = get_relative_position(old_item, new_item)
        new_transformation_from_start = compose_transformation(
          transformations[[as.character(old_item)]],
          new_transformation)
        transformations[[as.character(new_item)]] =
          new_transformation_from_start
      }
    }
  }
}

inverted_transformations = lapply(transformations, invert_transformation)

transform_to_scanner_1_coords = function(coordinates, transformation){
  number_coordinates = nrow(coordinates)
  expanded_shift = rep(transformation$shift, times = number_coordinates) %>%
    matrix(nrow = number_coordinates, byrow = TRUE)
  transformed_matrix = as.matrix(coordinates) %*% transformation$transformation_matrix +
    expanded_shift
  return(as.data.frame(transformed_matrix))
}

transformed_to_scanner_1_coords_list = 
  lapply(1:number_of_scanners, 
         function(scanner_id){
           coordinate_data = input_scanner_dataframe %>%
             filter(scanner == scanner_id) %>%
             select(coord_1, coord_2, coord_3)
           transformation = inverted_transformations[[as.character(scanner_id)]]
           new_coords = transform_to_scanner_1_coords(coordinate_data, transformation)
           return(new_coords)
         })

transformed_to_scanner_1_coords = do.call(rbind, transformed_to_scanner_1_coords_list)

transformed_to_scanner_1_coords %>% arrange(V1, V2, V3) %>% distinct %>% nrow #438

beacon_positions = transformed_to_scanner_1_coords %>% arrange(V1, V2, V3) %>% distinct
number_of_beacons = nrow(beacon_positions)
beacon_distance_matrix = data.frame(beacon1 = rep(1:number_of_beacons, each = number_of_beacons),
                                    beacon2 = rep(1:number_of_beacons, times = number_of_beacons)) %>%
  filter(beacon1 < beacon2)

# beacon_distance_matrix$distance = 
#   mapply(function(beacon1, beacon2){
#     abs(as.vector(beacon_positions[beacon1,]) - 
#       as.vector(beacon_positions[beacon2,])) %>% sum
#   }, beacon_distance_matrix$beacon1, beacon_distance_matrix$beacon2)
# 
# max(beacon_distance_matrix$distance)

scanner_positions = lapply(inverted_transformations, function(transformation){transformation$shift})

scanner_distance_matrix = data.frame(scanner1 = rep(1:number_of_scanners, each = number_of_scanners),
                                    scanner2 = rep(1:number_of_scanners, times = number_of_scanners)) %>%
  filter(scanner1 < scanner2)

scanner_distance_matrix$distance = 
  mapply(function(scanner1, scanner2){
    abs(as.vector(scanner_positions[[as.character(scanner1)]]) - 
          as.vector(scanner_positions[[as.character(scanner2)]])) %>% sum
  }, scanner_distance_matrix$scanner1, scanner_distance_matrix$scanner2)

max(scanner_distance_matrix$distance) #11985


transformations[['3']]
inverted_transformations[['3']]

if(FALSE){
  test_coords_a = data.frame(X1 = c(-1, -2, -3, -2, 5, 8),
                        X2 = c(-1, -2, -3, -3, 6, 0),
                        X3 = c(1, 2, 3, 1, -4, 7))
  test_coords_b = data.frame(X1 = c(1, 2, 3, 2, -5, -8),
                             X2 = c(-1, -2, -3, -1, 4, -7),
                             X3 = c(1, 2, 3, 3, -6, 0))
  transformation_matrix = get_transformation_matrix(as.matrix(test_coords_a), as.matrix(test_coords_b))
  shift = test_coords_b %>% head(1) %>% as.matrix() -
    (test_coords_a %>% head(1) %>% as.matrix() %*% transformation_matrix)
  transform_to_scanner_1_coords(test_coords_a, list(transformation_matrix = transformation_matrix,
                                                    shift = shift))
  scanner_0_coords = data.frame(X1 = -618, X2 = -824, X3 = -621)
  transform_to_scanner_1_coords(scanner_0_coords,
                                transformations[['2']]) #expect 686, 422, 578
  inverted_transformations[['2']]
}