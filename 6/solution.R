input = readLines("C:\\Users\\volan\\Documents\\Advent of Code 2021\\6\\input.txt")

input_parsed = as.numeric(unlist(strsplit(input, ',')))

#Initially represent populations as vectors
input_vector = rep(0, 9)
input_vector[2:6] = table(input_parsed)

evolve_population = function(input_population){
  new_population = rep(0,9)
  new_population[1:8] = input_population[2:9]
  new_population[9] = input_population[1]
  new_population[7] = new_population[7] + input_population[1]
  return(new_population)
}

current_population = input_vector
for(i in 1:80){
  current_population = evolve_population(current_population)
}
sum(current_population) #380612

current_population = input_vector
for(i in 1:256){
  current_population = evolve_population(current_population)
}
as.character(sum(current_population)) #1710166656900
