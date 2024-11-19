# set up ------------------------------------------------------------------

rm(list = ls())

# load library ------------------------------------------------------------

source(here::here("Code/set_library.R"))

# poisson example - likelihood --------------------------------------------
# dpois()
# the first argument is "k"
# the second argument is "lambda" - 
#if the data follows a Poisson dist. with a mean 3.5
dpois(3, lambda = 3.5)

#written version of the equation
3.5^3 * exp(-3.5) / factorial(3)

# change lambda from 0 to 10 by 0.1
lambda <- seq(0, 10, by = 0.1)

# probability
pr <- dpois(3, lambda = lambda)

# create a data frame
df_pr <- tibble(y = 3,
                  lambda = lambda,
                  pr = pr)

print(df_pr)


#plotting
g1 <-df_pr %>% 
  ggplot(aes(x = lambda,
             y = pr)) +
  geom_line() +
  geom_point()
  labs(x = "lambda",
       y = "Pr(k = 3)")
 
#arrange by probability 
df_pr %>% 
  arrange(desc(pr))

#data y= {3, 2, 5}, lambda = 3

pr <- dpois(c(3, 2, 5), lambda = 3)
print(pr)

#manually
pr[1]*pr[2]*pr[3]
#automatically
prod(pr)

# lambda = 0 - 10 by 0.01
y <- c(3, 2, 5)
lambda <- seq(0, 10, by = 0.01)

# sapply repeats the task in FUN
# each element in "X" will be sequencially substituted in "z"
pr <- sapply(X = lambda,
             FUN = function(z) 
               prod(dpois(y, lambda = z)))

# make a data frame and arrange by pr (likelihood)
df_pois <- tibble(lambda = lambda,
                  pr = pr)

df_pois %>% 
  arrange(desc(pr)) %>% 
  print()

# plotting
g2 <- df_pois %>% 
  ggplot(aes(x = lambda,
             y = pr)) +
  geom_line() +
  geom_point() +
  labs(y = "Likelihood")


# maximum likelihood ------------------------------------------------------

# load garden plant data
df_count <- read_csv(here::here("data_raw/data_garden_count.csv"))

m_pois <- glm(count ~ nitrate,
              data = df_count,
              family = "poisson")

logLik(m_pois)
