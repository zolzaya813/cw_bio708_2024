
# setup -------------------------------------------------------------------

rm(list = ls())

# load library ------------------------------------------------------------

source(here::here("Code/set_library.R"))

df_plant <- PlantGrowth


# ex:1 --------------------------------------------------------------------

df_plant %>% 
  ggplot(aes(x = group,
             y = weight)) +
  geom_violin(draw_quantiles = 0.5, # draw median horizontal line
              alpha = 0.2) + # transparency
  geom_jitter(alpha = 0.2) # transparency

#Anova in R
z <- aov(formula = weight ~ group,
         data = df_plant)
summary(z)


# ex:2 --------------------------------------------------------------------
#Conduct an ANOVA to examine whether there are differences in weight among the different group.

mu <- mean(df_plant$weight)

df_g <- df_plant %>% 
  group_by(group) %>% 
  summarize(mu_g = mean(weight), 
            dev_g = (mu_g - mu)^2,
            n = n())
print(df_g)

df_g <- df_g %>% 
  mutate(ss = dev_g * n)

print(df_g)
s_b <- sum(df_g$ss)
print(s_b)

df_i <- df_plant %>% 
  group_by(group) %>% 
  mutate(mu_g = mean(weight)) %>% 
  ungroup() %>% 
  mutate(dev_i = (weight - mu_g)^2)

df_i_g <- df_i %>% 
  group_by(group) %>% 
  summarize(ss = sum(dev_i))

print(df_i_g)

s_w <- sum(df_i_g$ss)
print(s_w)

n_g <- n_distinct(df_plant$group)
s2_b <- s_b / (n_g - 1)
print(s2_b)

s2_w <- s_w / (nrow(df_plant) - n_g)
print(s2_w)

f_value <- s2_b / s2_w
print(f_value)


#create a function to calculate F value

f_value_fun <- function(data) {
  s_b <- data %>% 
    group_by(group) %>% 
    summarise(mu_g = mean(weight),
              dev_g = (mu_g -mu)^2,
              n = n(),
              ss = dev_g * n) %>% 
    pull(ss) %>% 
    sum()
  var_b <- s_b/(n_distinct(data$group) - 1)
  s_w <- data %>% 
    group_by(group) %>% 
    mutate(mu_g = mean(weight)) %>% 
    ungroup() %>% 
    mutate(dev_i = (weight- mu_g)^2) %>% 
    group_by(group) %>% 
    summarise(ss = sum(dev_i)) %>% 
    pull(ss) %>% 
    sum()
var_w <- s_w / (nrow(data)- n_distinct(data$group))

f_value <- var_b / var_w

return(f_value)
  
}

f_value_fun(data = df_plant)
