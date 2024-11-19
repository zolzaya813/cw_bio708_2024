# set up ------------------------------------------------------------------

rm(list = ls())

# load library ------------------------------------------------------------

source(here::here("Code/set_library.R"))


# ex: 9.3.1 ---------------------------------------------------------------
#binomial distribution

y <- c(2,2,0,0,3,1,3,3,4,3)
p <- seq(0, 1, by = 0.1)
N <- 10

#dbinom(x = y , 
#       size = N,
#       prob = p) 
#sturcture is correct for dbinom(), but this is wrong because I need to calculate probability y as a vector. 
#In this case it is calculating probability for each value of y.

pr <- sapply(X = p,
             FUN = function(z) 
               prod(dbinom(x = y,
                           size = N,
                           p = z)))
print(pr)

df_p <- tibble(prob = p,
               lh = pr) %>% 
  arrange(desc(lh))
print(df_p)
