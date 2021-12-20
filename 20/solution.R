input = readLines('clipboard')
input = c(paste0(input[1:7], collapse = ''), input[8:13])

input = readLines("C:\\Users\\volan\\Documents\\Advent of Code 2021\\20\\input.txt")

enhancement_algorithm = input[1]
enhancement_algorithm_vector = unlist(strsplit(input[1], '')) == '#'

image_rows = length(input) - 2
image_vector = unlist(strsplit(input[3:length(input)], '')) == '#'
# image_columns = length(image_vector)/image_rows

pad = function(image_matrix, n){
  new_matrix = diag(0, nrow(image_matrix) + (2*n), ncol(image_matrix) + (2*n))
  new_matrix[(n+1):(nrow(image_matrix) + n), (n+1):(ncol(image_matrix) + n)] = image_matrix
  return(new_matrix)
}
unpad = function(image_matrix, n){
  return(image_matrix[(n+1):(nrow(image_matrix) - n),
                      (n+1):(ncol(image_matrix) - n)])
}

# all(image_matrix == unpad(pad(pad(image_matrix,2),1),3))

image_matrix = matrix(image_vector, nrow = image_rows, byrow = TRUE)
enhance = function(image_matrix, enhancement_vector, background = 0){
  
  middle_column = rbind(image_matrix[1:2,] * 0 + background, image_matrix) * 64 + 
    rbind(image_matrix[1,] * 0 + background, image_matrix, image_matrix[1,] * 0 + background) * 8 + 
    rbind(image_matrix, image_matrix[1:2,] * 0 + background) * 1
  total_values = cbind(middle_column[,1:2] * 0 + background, middle_column) * 4 + 
    cbind(middle_column[,1] * 0 + background, middle_column, middle_column[,1] * 0 + background + background) * 2 + 
    cbind(middle_column, middle_column[,1:2] * 0 + background) * 1
  total_values_vector = as.vector(t(total_values))
  new_vector = sapply(total_values_vector, function(i){enhancement_vector[i + 1]})
  return(matrix(new_vector, nrow = nrow(image_matrix) + 2, byrow = TRUE))
}

# enhance(image_matrix, enhancement_algorithm_vector)

print_image = function(image_matrix){
  for(i in 1:nrow(image_matrix)){
    print(paste0(
      ifelse(image_matrix[i,], '#', '.'), collapse = ''
    ))
  }
}

# print_image(image_matrix)
# print_image(enhance(image_matrix, enhancement_algorithm_vector))
# print_image(enhance(enhance(image_matrix, enhancement_algorithm_vector), enhancement_algorithm_vector))

enhanced = enhance(image_matrix, enhancement_algorithm_vector, 0)
twice_enhanced = enhance(enhance(pad(image_matrix, 10), enhancement_algorithm_vector), enhancement_algorithm_vector, 1)
twice_enhanced_trimmed = unpad(twice_enhanced, 10)
sum(twice_enhanced_trimmed) #5349

fifty_enhanced = pad(image_matrix, 99)
for(i in 1:50){
  fifty_enhanced = enhance(fifty_enhanced, enhancement_algorithm_vector)
}
fifty_enhanced_trimmed = unpad(fifty_enhanced, 99)
sum(fifty_enhanced_trimmed) #39052
