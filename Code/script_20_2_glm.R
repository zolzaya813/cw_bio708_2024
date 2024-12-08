
# set up ------------------------------------------------------------------

rm(list = ls())

# load library ------------------------------------------------------------

source(here::here("Code/set_library.R"))


# Proportional Data -------------------------------------------------------

# Binomial 
##visualizition
# x: produce 100 numbers from -100 to 100 (assume logit scale)
# y: convert with inverse-logit transformation (ordinary scale)
df_test <- tibble(logit_x = seq(-10, 10, length = 100),
                  x = exp(logit_x) / (1 + exp(logit_x)))

df_test %>% 
  ggplot(aes(x = logit_x,
             y = x)) +
  geom_point() +
  geom_line() +
  labs(y = "x",
       x = "logit(x)")

df_mussel <- read_csv(here::here("data_raw/data_mussel.csv"))
print(df_mussel)

# calculate the proportion of fertilized eggs
df_mussel <- df_mussel %>% 
  mutate(prop_fert = n_fertilized / n_examined)

# plot
df_mussel %>% 
  ggplot(aes(x = density,
             y = prop_fert)) +
  geom_point() +
  labs(y = "Proportion of eggs fertilized",
       x = "Mussel density")

##glm with binomial error distribution
m_binom <- glm(cbind(n_fertilized, n_examined - n_fertilized) ~ density, ###cbind() combines two columns 
               data = df_mussel,
               family = "binomial") ## always have to specify the "family"

# make prediction (making data frame for the prediction)
df_pred <- df_mussel %>% 
  reframe(density = seq(min(density),
                        max(density),
                        length = 100))


# y_binom is inv.logit-transformed because predict() returns values in logit-scale
y_binom <- predict(m_binom,
                   newdata = df_pred) %>% 
  boot::inv.logit()


df_pred <- df_pred %>% 
  mutate(y_binom)

df_mussel %>% 
  ggplot(aes(x = density,
             y = prop_fert)) +
  geom_point() +
  labs(y = "Proportion of eggs fertilized",
       x = "Mussel density") +
  geom_line(data = df_pred,
            aes(y = y_binom),
            linetype = "dashed", 
            color = "salmon")

# binomial distribution with variable number of trials --------------------

##create fake data with variable number of trials
df_mussel <- df_mussel %>% 
  mutate(n_examined = rpois(nrow(.), lambda = 40))
## this model code naturally accounts for variation in n_examined
m_binom <- glm(cbind(n_fertilized, n_examined - n_fertilized) ~ density,
               data = df_mussel,
               family = "binomial")
print(m_binom)
