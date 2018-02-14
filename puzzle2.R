# puzzle 2
library(tidyverse)

# For each row, determine the difference between the largest value and the smallest value; the checksum is the sum of all of these differences.
input <- read_tsv("day2input.txt", col_names = FALSE)

diff = apply(input, 1, max) - apply(input, 1, min) 
checksum = sum(diff)
checksum
