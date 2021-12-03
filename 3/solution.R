input = readLines("C:\\Users\\volan\\Documents\\Advent of Code 2021\\3\\input.txt")

report_as_matrix = matrix(as.numeric(unlist(strsplit(input, ''))), nrow = length(input), byrow = TRUE)
column_sums = colSums(report_as_matrix)
gamma_binary_digits = column_sums > length(input)/2
epsilon_binary_digits = !gamma_binary_digits
gamma = sum(gamma_binary_digits * 2^(12 - 1:12))
epsilon = sum(epsilon_binary_digits * 2^(12 - 1:12))            
gamma * epsilon #852500

reduce_rows = function(input_submatrix, column, 
                       more_common = TRUE, prefer = 1){
  column_to_consider = input_submatrix[,column]
  ones = sum(column_to_consider == 1)
  zeroes = sum(column_to_consider == 0)
  
  if(ones == zeroes){
    filter_value = prefer
  } else if(ones > zeroes) {
    if(more_common){
      filter_value = 1
    } else {filter_value = 0}
  } else {
    if(more_common){
      filter_value = 0
    } else {
      filter_value = 1
    }
  }
  
  reduced_matrix = input_submatrix[column_to_consider == filter_value, ]
}

recursive_reduce = function(input_submatrix, columns, 
                            more_common, prefer){
  working_matrix = input_submatrix
  for(column in 1:columns){
    working_matrix = reduce_rows(working_matrix, column, more_common, prefer)
    if(!'matrix' %in% class(working_matrix)){
      return(working_matrix)
    }
  }
}

oxygen_generator_bits = recursive_reduce(report_as_matrix, ncol(report_as_matrix),
                                         more_common = TRUE, prefer = 1)
oxygen_generator = sum(oxygen_generator_bits * 2^(12 - 1:12))            

co2_scrubber_bits = recursive_reduce(report_as_matrix, ncol(report_as_matrix),
                                     more_common = FALSE, prefer = 0)

co2_scrubber = sum(co2_scrubber_bits * 2^(12 - 1:12))            

oxygen_generator * co2_scrubber #1007985
