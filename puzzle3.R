# puzzle 3 - manhattan distance
library(tidyverse)

manhattan_dist <- function(p1, p2, q1, q2){
  abs(p1-q1) + abs(p2-q2)
}

# we need to know how big the matrix is so as to know where each value is, so as
# to calculate distance

#Data from square 12 is carried 3 steps, such as: down, left, left.
test2 = 12
ans2 = 3

# first question how big is the matrix
# nrow, ncol
length = 12

if(length = 1){steps = 0}

nrow = seq(3, 100000, 2)
touching = 4*nrow - 4


# whats the algorithm -- number of touching values
# 1, 8, 16, 

#each side adds 2 each step 
#n = 1
#n = 3
#n = 5
#n = 7

testmat <- matrix(c(17,16,15,14,13,18,5,4,3,12,19,6,1,2,11,20,7,8,9,10,21,22,23,
                    24,25), nrow=5, ncol=5, byrow=T)

ansmat <- matrix(c(4,3,2,3,4,3,2,1,2,3,2,1,0,1,2,3,2,1,2,3,4,3,2,3,4),nrow=5, 
                 ncol=5, byrow=T)


diff(testmat)

# so the answer matrix is special. the diagonal is even numbers by 2

# one approach would be to simulate it. If i can simulate it I can solve it. 
# not the most efficient but effective I suppose. 

# so for value x, i need to first decide how big my matrix is going to be
# 1x1, 3x3, 5x5, ... nxn assuming it's odd. 
# so I guess first thing is to see which matrix size the value calls for, by assessing which square it is closest. 
# so 12, is greater than 9 but less than 25 so it requires a 5x5

mirror <- function(x){
  xx <- as.data.frame(x)
  xx <- rev(xx)
  xx <- as.matrix(xx)
  xx
}

rotate180 <- function(x){
  xx <- rev(x)
  dim(xx) <- dim(x)
  xx
}

manhattan_steps_matrix<- function(val){
  if(val==1){return(0)}
  n <- seq(1, 10000, 2)
  sq_n <- n^2
  test <- which(val >= sq_n)
  if(any(val == sq_n[test])){
    m <- matrix(ncol = sqrt(sq_n[test[-1]]), nrow = sqrt(sq_n[test[-1]]))
    } else {
      m <- matrix(ncol = sqrt(sq_n[length(test)+1]), nrow = sqrt(sq_n[length(test)+1]))
      }
  m[ceiling(nrow(m)/2),ceiling(ncol(m)/2)] <- 0
# hence
  q1 = ceiling(nrow(m)/2)
  q2 = ceiling(nrow(m)/2)
# so now I just have to figure out how to place, p1,p2. 
  steps <- matrix(nrow=q1,ncol=q2)
  for(i in 1:q1){
    steps[i,] <- c(seq(from = (ncol(m)-i), by=-1, length.out = (ncol(m)-q1 + 1)))
  }
  
  full <- rbind(cbind(steps, mirror(steps)[,-1]), cbind(mirror(rotate180(steps))[-1,], rotate180(steps)[-1,-1]))
  colnames(full) <-NULL
  
  
  
  
  
  
}
# new tact:how far are you from a sq... ncol nrow... will get you steps from center.... 
# steps
# build the grid, 
# ask where n is (which function)
# calculate manhattan distance to center. 


# if you know the size of the square matrix (ncol) that has to be built to accomodate a number
# then you can 


# 4 behaviors  in a pattern
pattern <- function(val){
  if(val==1){return(0)}
  # figure out size of matrix
  n <- seq(1, 10000, 2)
  sq_n <- n^2
  test <- which(val >= sq_n)
  if(any(val == sq_n[test])){
    m <- matrix(ncol = sqrt(sq_n[test[-1]]), nrow = sqrt(sq_n[test[-1]]))
  } else {
    m <- matrix(ncol = sqrt(sq_n[length(test)+1]), nrow = sqrt(sq_n[length(test)+1]))
  }
  m[ceiling(nrow(m)/2),ceiling(ncol(m)/2)] <- 0
  # hence
  q1 = ceiling(nrow(m)/2)
  q2 = ceiling(nrow(m)/2)
  
  # figure out integer placement 
  right <- c(0, 1)
  up <- c(-1, 0)
  left <- c(0, -1)
  down <- c(1, 0)

  direction <- matrix(c(right, up, left, down), ncol=2, byrow=T)
  
  path = matrix(c(0,0), ncol=2)
  for(i in seq(1,2*q1,2)){  # not sure this wouldn't break eventually. need to think through the math
    path = c(path, rep(direction[1,], i), 
                    rep(direction[2,], i), 
                    rep(direction[3,], i+1),
                    rep(direction[4,], i+1))
    path_m = matrix(path, ncol=2, byrow=T)
    }
  trimmed_path = path_m[1:val,]
  table <- tibble(square = 1:val,
                  row_diff = trimmed_path[,1],
                  col_diff= trimmed_path[,2], 
                  row = cumsum(c(q1, row_diff))[-1],
                  col = cumsum(c(q2, col_diff))[-1])
  

  # calculate manhantan distance
  return(abs(pull(table, row)[val] - q1) + abs(pull(table,col)[val] - q2))
  
}


################## TESTS ##########################
#Data from square 1 is carried 0 steps, since it's at the access port.
test1 = 1
ans1 = 0
pattern(test1)

#Data from square 12 is carried 3 steps, such as: down, left, left.
test2 = 12
ans2 = 3
pattern(test2)

#Data from square 23 is carried only 2 steps: up twice.
test3 = 23
ans3 = 2
pattern(test3)

#Data from square 1024 must be carried 31 steps.
test4=1024
ans4=31
pattern(test4)
### super slow but correct!

input <- 312051
pattern(input)







