
# setup -------------------------------------------------------------------

rm(list = ls())

# load library ------------------------------------------------------------

library(tidyverse)

iris <- as_tibble(iris)

# function ----------------------------------------------------------------
##function() is to create a new function

##without function
x <- rnorm(100, mean = 10, sd = 100)
sig <- sd(x) 
mu <- mean(x)
cv <- sig / mu
cv

##with function
fun_cv <- function(x){
  sd(x) / mean(x)
  return(cv) # have to use return() to display the output
}

## standardize

x0 <- x- mean(x) #centering
z <- x0 / sd(x0) #scaling

scl <- function(x) {
  z <- (x - mean(x)) / sd(x- mean(x))
  return(z)
}

##random function with two arguments

f0 <- function(phi, zeta) {
  count <- 2*phi + rnorm(1)*zeta
  return(count)
}

f0(phi = 2, zeta = 3)

# apply family ------------------------------------------------------------

m <- matrix (rnorm(25), nrow = 5, ncol = 5)

apply(m, MARGIN = 1, FUN = mean) #Margin -> 1= row, 2= column
apply(m, MARGIN = 1, FUN = fun_cv)

## for dataframe

apply(iris %>%
        select(1:4),
      MARGIN = 2, FUN = mean)
## sapply() for list,but output is vector
x <- rnorm(10)
y <- rnorm(100)
z <- rnorm(5)

l_xyz <- list(x, y, z) ##making an organized list() by elements has different length of value

sapply(l_xyz, FUN = mean)

##lapply() for list,but output is list too
x <- rpois(10, lambda = 5)
y <- rpois(100, lambda = 5)
z <- rep(letters[1:3], 10)

l_xyz <- list(x, y, z)

### remove diplicates, or get unique elements
unique(x)
unique(y)
unique(z)

lapply(l_xyz, FUN = unique)

##try to get only the first elements from each vector
lapply(l_xyz, 
       FUN = function(x){
         x[1]
       })

# for loop ----------------------------------------------------------------

## for loop is to repeat work inside {}

x <- seq(0, 10, by = 0.25)
y <- NULL

for(i in 1:10) {
 y[i] <- 2*x[i] # if you want to apply this function for each value you should use the [i], without [i] it will show the only the last operation
}
