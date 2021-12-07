input = readLines("C:\\Users\\volan\\Documents\\Advent of Code 2021\\7\\input.txt")
input_parsed = as.numeric(unlist(strsplit(input, ',')))

median_input = median(input_parsed)
target = round(median_input)
sum(abs(input_parsed - target)) #348664

range = c(min(input_parsed), max(input_parsed))

costs = sapply(range[1]:range[2],
               function(target){
                 distances = abs(input_parsed - target)
                 sum(distances * (distances + 1))/2
               })

min(costs) #100220525
