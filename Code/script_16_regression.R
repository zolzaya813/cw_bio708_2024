
# setup -------------------------------------------------------------------

rm(list = ls())

# load library ------------------------------------------------------------

source(here::here("Code/set_library.R"))

# data --------------------------------------------------------------------

df_algae <- read_csv(here::here("data_raw/data_algae.csv"))

#install.packages("skimr")
skimr::skim(df_algae)

#visualization
df_algae %>% 
  ggplot(aes(x = conductivity,
             y = biomass)) +
  geom_point(color = "salmon") +
  theme_bw()


# fittest line ------------------------------------------------------------

# lm() takes a formula as the first argument
# don't forget to supply your data
m <- lm(biomass ~ conductivity,
        data = df_algae)

summary(m)

# coef() extracts estimated coefficients
# e.g., coef(m)[1] is (Intercept)

alpha <- coef(m)[1]
beta <- coef(m)[2]

df_algae %>% 
  ggplot(aes(x = conductivity,
             y = biomass)) +
  geom_point() +
  geom_abline(intercept = alpha,
              slope = beta) # draw the line
#calculate residual in the regression line
eps <- round(df_algae$biomass - (alpha + beta*df_algae$conductivity), 4)
print(eps)
eps0 <- round(resid(m), 4) ## works same as the equation above
eps == eps0

eps_ss <- sum(eps^2)


# get t-value -------------------------------------------------------------
# extract coefficients
theta <- coef(m)

# extract standard errors
se <- sqrt(diag(vcov(m)))

# t-value
t_value <- theta / se
print(t_value)

# for intercept
# (1 - pt(t_value[1], df = 48)) calculates pr(t > t_value[1])
# pt(-t_value[1], df = 48) calculates pr(t < -t_value[1])
p_alpha <- (1 - pt(t_value[1], df = 48)) + pt(-t_value[1], df = 48)

# for slope
p_beta <- (1 - pt(t_value[2], df = 48)) + pt(-t_value[2], df = 48)

print(p_alpha)
print(p_beta)


# visualize error ---------------------------------------------------------

eps <- round(df_algae$biomass - (alpha + beta*df_algae$conductivity), 4)
print(eps)
eps0 <- round(resid(m), 4) ## works same as the equation above
eps == eps0

eps_ss <- sum(eps^2)

# add error column
df_algae <- df_algae %>% 
  mutate(eps = eps)

df_algae %>% 
  ggplot(aes(x = conductivity,
             y = biomass)) +
  geom_point(color = "salmon",
             size = 2) +
  geom_abline(intercept = alpha,
              slope = beta,
              color = "orange") + 
  geom_segment(aes(x = conductivity, # start-coord x
                   xend = conductivity, # end-coord x
                   y = biomass, # start-coord y
                   yend = biomass - eps), # end-coord y
               linetype = "dashed") +
  theme_bw()

#coefficient determination
summary(m)
x