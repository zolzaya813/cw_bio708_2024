
# setup -------------------------------------------------------------------

rm(list = ls())

# load library ------------------------------------------------------------

source(here::here("Code/set_library.R"))


# 3.4.1 -------------------------------------------------------------------

mu <- 10
sigma <- 5
df_1 <- rnorm(n = 50, mean = mu, sd = sigma)


x_min <- floor(min(df_1))
x_max <- ceiling(max(df_1))
bin <- seq(x_min, x_max, by = 1) 

p <- NULL
for (i in 1:(length(bin) - 1)) {
  p[i] <- pnorm(bin[i+1], mean = mu, sd = sigma) - pnorm(bin[i], mean = mu, sd = sigma)
}

df_prob <- tibble(p, bin = bin[-length(bin)] + 0.5) %>% 
  mutate(freq = p * length(df_1))

df_11 <- tibble(df_1)
df_11 %>%  
  ggplot(aes(x = df_1)) + 
  geom_histogram(binwidth = 1, 
                 center = 0.5) +
  geom_point(data = df_prob,
             aes(y = freq,
                 x = bin),
             color = "salmon") +
  geom_line(data = df_prob,
            aes(y = freq,
                x = bin),
            color = "salmon")

# 3.4.2 -------------------------------------------------------------------
lambda <- 20
y_1 <- rpois(n = 1000, 
             lambda = lambda)
lambda_hat <- mean(y_1)

min_y <- min(y_1)
max_y <- max(y_1)
bin_y <- seq(min_y, max_y, by = 1) #it is must be 1 because it's discrete

lambda_hat <- mean(y_1)

df_pois <- tibble(x = bin_y,
                  pm = dpois(bin_y, lambda = lambda_hat)) %>% 
  mutate(freq = length(y_1) * pm)
print(df_pois)

tibble(y_1 = y_1) %>% 
  ggplot() +
  geom_histogram(aes(x = y_1),
                 binwidth = 0.5) +
  geom_point(data = df_pois,
             aes(x = x,
                 y = freq)) +
  geom_line(data = df_pois,
            aes(x = x,
                y = freq)) +
  scale_x_continuous(breaks = y_1)



