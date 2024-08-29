
# setup -------------------------------------------------------------------
rm(list = ls())

# exercise 1 - vector -----------------------------------------------------
# a
nv1 <- 1:3
nv1 <- seq(1, 3, by = 1)
nv2 <- 1:6
nv2 <- seq(1, 6, by = 1)
nv3 <-1:20
nv3 <- seq(1, 20, by = 1)
# b
cv1 <- c("apple", "banana", "cherry")
cv1 <- rep("orange", 3)
cv2 <- c("apple", "banana", "cherry", "blueberry", "pineapple", "orange")
cv2 <- rep("apple", 6)
cv3 <- rep("cherry", 20)
#c
set.seed(1)
x <- rnorm(100)
x[x > 2] #element IDs of x that are greater than 2.0
which(x > 2) #element values of x that are greater than 2.0

# exercise 2 - matrix -----------------------------------------------------
#a
nm1 <- cbind(c(rep(1,4)), c(rep(2,4)), c(rep(3,4)), c(rep(4,4)))
#b
nm2 <- rbind(c(rep(1,4)), c(rep(2,4)), c(rep(3,4)), c(rep(4,4)))
nm2 <- matrix(rep(1:4, each =4),
              nrow = 4, 
              ncol = 4, 
              byrow = TRUE)
#c
cm1 <- cbind(c(rep("a",4)), c(rep("b",4)), c(rep("d",4)), c(rep("d",4)))
#d
cm2 <- rbind(c(rep("a",4)), c(rep("b",4)), c(rep("d",4)), c(rep("d",4)))
#e
set.seed(1)
x <- matrix(rnorm(100), nrow = 10, ncol = 10)
which (x > 2, arr.ind = TRUE) ##element ID
x[x > 2]

# exercise 3 - data frame -------------------------------------------------
#a
# Create the data frame
data <- data.frame(
  x = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J"),  
  y = c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100),            
  z = c(1.5, 2.3, 3.1, 4.7, 5.2, 6.8, 7.4, 8.0, 9.6, 10.2)   
  )
#b
class(data) 
#c
set.seed(1)
x <- rnorm(100, mean = 10, sd = 3)
y <- rpois(100, lambda = 10)
z <- rep(c("VA", "NC"), 50)
df0 <- data.frame(temperature = x, abundance = y, state = z)

v_va <-df0$temperature[df0$state == "VA"]
v_nc <-df0$temperature[df0$state == "NC"]

mu_va <- mean(v_va)
mu_nc <- mean(v_nc)

##extra exersice
with(df0, temperature[state == "VA"])
tapply(df0$temperature, INDEX = df0$state, FUN = mean)
