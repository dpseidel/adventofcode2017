# puzzle 2
library(tidyverse)

# For each row, determine the difference between the largest value and the smallest value; the checksum is the sum of all of these differences.
input <- read_tsv("day2input.txt", col_names = FALSE)

diff = apply(input, 1, max) - apply(input, 1, min) 
checksum = sum(diff)
checksum


## part 2

test <- matrix(nrow=3, ncol=4, c(5, 9, 2, 8, 9, 4, 7, 3, 3, 8, 6, 5), byrow=T)
test

ans <- c(4,3,2)

is.wholenumber <- function(x, tol = .Machine$double.eps^0.5){
  abs(x - round(x)) < tol
  }

divisible <- function(vector){
  for(i in seq(vector)){
   modulus <- (vector[i] %% vector)
   if(sum(modulus==0) == 2){
     pair = which(modulus==0)
     other = subset(pair, pair != i)
     quotients = c(vector[i]/vector[other], vector[other]/vector[i])
     answer = quotients[which(is.wholenumber(quotients))]
     return(ifelse(length(answer) == 1, answer, max(quotients)))
   }
  }
  }

apply(test, 1, divisible) == ans

apply(input, 1, divisible) %>% sum
#282
