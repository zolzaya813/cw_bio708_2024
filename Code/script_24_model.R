# set up ------------------------------------------------------------------

rm(list = ls())

# load library ------------------------------------------------------------

source(here::here("Code/set_library.R"))

# model fit and complexity ------------------------------------------------

##simulated data

set.seed(1) # for reproducibility

# hypothetical sample size
n <- 100

# true intercept and slope
b <- c(0.1, 0.5)

# hypothetical explanatory variable
x1 <- rnorm(n = n, mean = 0, sd = 1)

# create a design matrix
X <- model.matrix(~x1)

# expected values of y is a function of x
# %*% means matrix multiplication
# y = X %*% b equals y = b[1] + b[2] * x
# recall linear algebra
y_hat <- drop(X %*% b)

# add normal errors
y <- rnorm(n = n, mean = y_hat, sd = 0.5)

# plot
df0 <- tibble(y = y, x1 = x1)

df0 %>% 
  ggplot(aes(y = y,
             x = x1)) + 
  geom_point()+
  theme_bw()

# correct model used to generate the data
m1 <- lm(y ~ x1, data = df0)
s1 <-summary(m1)

# add a new variable #

# add a column x2 which is irrelevant for y
df0 <- df0 %>% 
  mutate(x2 = rnorm(n = nrow(.),
                    mean = 0,
                    sd = 1))

# add x2 to the model
m2 <- lm(y ~ x1 + x2, data = df0)
s2 <-summary(m2)
c(s1$r.squared, s2$r.squared)

# comparison metrics ------------------------------------------------------

#adjusted R^2#

c(s1$adj.r.squared, s2$adj.r.squared)

# Likelihood Ratio Test# 

#logLik(m1) > logLik(m2) despite x2 being irrelevant
logLik(m1)
logLik(m2)

# test = "Chisq" specifies a chi-square distribution
# as a distribution of LR
anova(m1, m2, test = "Chisq")


#AIC#
##lower is better

# AIC: correct model 
AIC(m1)

# AIC: incorrect model
AIC(m2)
