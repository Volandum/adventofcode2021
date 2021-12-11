input = readLines("C:\\Users\\volan\\Documents\\Advent of Code 2021\\11\\input.txt")
#input = readLines("C:\\Users\\volan\\Documents\\Advent of Code 2021\\11\\testinput.txt")


input_as_matrix = matrix(as.numeric(unlist(strsplit(input, ''))),
                             nrow = length(input), byrow = TRUE)

shift_matrix_horizontally = function(matrix, offset){ #+1 for right, -1 for left
  if(offset == 1){
    cbind(rep(0, nrow(matrix)), matrix[,1:(ncol(matrix) - 1)])
  } else if (offset == -1){
    cbind(matrix[,2:ncol(matrix)], rep(0, nrow(matrix)))
  } else {stop()}
}
shift_matrix_vertically = function(matrix, offset){ #+1 for down, -1 for up
  if(offset == 1){
    rbind(rep(0, ncol(matrix)), matrix[1:(nrow(matrix) - 1),])
  } else if (offset == -1){
    rbind(matrix[2:nrow(matrix),], rep(0, ncol(matrix)))
  } else {stop()}
}



get_surrounding_squares = function(flashes){
  flashes_as_numbers = flashes * 1
  horizontal_spread = flashes_as_numbers + 
    shift_matrix_horizontally(flashes_as_numbers, 1) +
    shift_matrix_horizontally(flashes_as_numbers, -1)
  three_by_three_box = horizontal_spread +
    shift_matrix_vertically(horizontal_spread, 1) +
    shift_matrix_vertically(horizontal_spread, -1)
  surrounding_squares = three_by_three_box - flashes_as_numbers
  return(surrounding_squares)
}

iterate = function(current_step){
  current_step = current_step + 1
  new_flashes = current_step > 9
  flashed = new_flashes
  while(sum(new_flashes) > 0){
    to_increment = get_surrounding_squares(new_flashes)
    current_step = current_step + to_increment
    new_flashes = current_step > 9 & !flashed
    flashed = new_flashes | flashed
  }
  current_step = current_step * !flashed
  flashed_so_far <<- flashed_so_far + sum(flashed)
  return(current_step)
}

flashed_so_far = 0
current_matrix = input_as_matrix
for(step in 1:100){
  current_matrix = iterate(current_matrix)
}
flashed_so_far #1652

flashed_so_far = 0
current_step = 0
current_matrix = input_as_matrix
while(!all(current_matrix == 0) & current_step < 10000){
  current_step = current_step + 1
  current_matrix = iterate(current_matrix)
}
current_step #220
