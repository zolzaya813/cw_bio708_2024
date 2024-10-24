
# setup -------------------------------------------------------------------

rm(list = ls())

# load library ------------------------------------------------------------

source(here::here("Code/set_library.R"))


# ex 6.4.1 ----------------------------------------------------------------

head(iris)
##Split
sp1 <- iris %>% 
  filter(Species == "setosa")
sp2 <- iris %>% 
  filter(Species == "versicolor")
sp3 <- iris %>% 
  filter(Species == "virginica")

##Regression
reg_sp1 <- lm(Sepal.Width ~ Petal.Width,
              data = sp1)
summary(reg_sp1)

reg_sp2 <- lm(Sepal.Width ~ Petal.Width,
              data = sp2)
summary(reg_sp2)

reg_sp3 <- lm(Sepal.Width ~ Petal.Width,
              data = sp3)
summary(reg_sp3)

m <-lm(Sepal.Width ~ Petal.Width,
       data = iris)
summary(m)

# ex 6.4.2 ----------------------------------------------------------------

m1 <- lm(Sepal.Width ~ Petal.Width + Petal.Length,
         data = iris)
summary(m1)

##add interaction
m_int <- lm(Sepal.Width ~ Petal.Width + Petal.Length + Petal.Width:Petal.Length,
            data = iris)
summary(m_int)

# ex 6.4.2 extra ----------------------------------------------------------

#Coefficient of variation
# residual variance
ss <- sum(resid(reg_sp1)^2)

v_x <- sp1 %>% 
  pull(Petal.Width)
v_y <- sp1 %>% 
  pull(Sepal.Width)

# null variance
ss_null <- sum((v_y - mean(v_y))^2)

# coefficient of determination
r2_sp1 <- 1 - ss / ss_null

print(r2_sp1)
summary(reg_sp1)

##for loop function
