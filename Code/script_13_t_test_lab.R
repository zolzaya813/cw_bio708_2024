
# setup -------------------------------------------------------------------

rm(list = ls())

# load library ------------------------------------------------------------

source(here::here("Code/set_library.R"))

# ex: 4.5.1 ---------------------------------------------------------------

xs <- rnorm(n = 10, mean = 10, sd = 5)
ys <- rnorm(n = 10, mean = 12, sd = 5)

t.test(xs, ys, var.equal = TRUE)

xl <- rnorm(n = 100, mean = 10, sd = 5)
yl <- rnorm(n = 100, mean = 12, sd = 5)

t.test(xl, yl, var.equal = TRUE)


# ex: 4.5.2 ---------------------------------------------------------------

a1 <- c(13.9, 14.9 ,13.4, 14.3, 11.8, 13.9, 14.5, 15.1, 13.3, 13.9)
a2 <- c(17.4, 17.3, 20.1, 17.2, 18.4, 19.6, 16.8, 18.7, 17.8, 18.9)

b1 <- c(10.9, 20.3, 9.6, 8.3, 14.5, 12.3, 14.5, 16.7, 9.3, 22.0)
b2 <- c(26.9, 12.9, 11.1, 16.7, 20.0, 20.9, 16.6, 15.4, 16.2, 16.2)

df <- tibble(value = c(a1, a2, b1, b2),
             group = c(rep("a1", length(a1)),
                       rep("a2", length(a2)),
                       rep("b1", length(b1)),
                       rep("b2", length(b2))))
print(df)

##summarize
df_sum <- df %>% 
  group_by(group) %>% 
  summarize(mu = mean(value), 
            sigma = sd(value)) 

##%in% -> filter(group %in% c("a1", "a2")

##ploting
df %>% 
  ggplot(aes(x = group,
             y = value)) +
  geom_jitter(width = 0.1, 
              height = 0,
              alpha = 0.25) + 
  geom_segment(data = df_sum, 
               aes(x = group,
                   xend = group,
                   y = mu - sigma,
                   yend = mu + sigma)) +
  geom_point(data = df_sum, 
             aes(x = group,
                 y = mu),
             size = 4) +
  labs(x = "Group", #
       y = "Value") #

##t_test
t.test(a1, a2, var.equal = FALSE)
t.test(a1, b1)
t.test(a2, b1)
t.test(a2, b2)
