input = readLines("C:\\Users\\volan\\Documents\\Advent of Code 2021\\1a\\inputa.txt")
# test data input = c(199,200,208,210,200,207,240,269,260,263)

# Part 1: forgot to convert from strings to numbers

input = as.numeric(input)
sum(input[2:length(input)] > input[1:length(input) - 1]) #1373 is too low, answer is 1374
table(sign(input[2:length(input)] - input[1:length(input) - 1]))

# Part 2: How often does x[i+3] > x[i]?

sum(input[4:length(input)] > input[1:(length(input) - 3)]) #1418
