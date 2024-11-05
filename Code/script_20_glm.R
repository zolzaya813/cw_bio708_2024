
# set up ------------------------------------------------------------------

rm(list = ls())

# load library ------------------------------------------------------------

source(here::here("Code/set_library.R"))

# count data --------------------------------------------------------------

df_count <- read_csv(here::here("data_raw/data_garden_count.csv"))
print(df_count)
## fit a normal model to count data
m_normal <- lm(count ~ nitrate,
               data = df_count)

summary(m_normal)
##ploting 

a <- coef(m_normal)[1]
b <- coef(m_normal)[2]

(df_pred1 <- df_count %>% 
  ggplot(aes(x = nitrate,
             y = count)) +
  geom_point() +
  geom_abline(intercept = a,
              slope = b))

###another example for ploting 
df_pred_01 <- df_count %>% 
  reframe(nitrate = seq(min(nitrate),
                        max(nitrate),
                        length = 100),
          y = b[1] + b[2] * nitrate)
(g_normal <- df_count %>% 
  ggplot(aes(x = nitrate,
             y = count)) +
  geom_point() +
  geom_line(data = df_pred_01,
            aes(y = y),
            linetype = "dashed"))  

# Poison distribution -----------------------------------------------------
##fitting to the model
m_pois <- glm(count ~ nitrate,
              data = df_count,
              family = "poisson")
summary(m_pois)

# parameter estimates and their SEs
theta <- coef(m_pois)
se <- sqrt(diag(vcov(m_pois)))
z_value <- theta / se

print(z_value)

###ploting
# make predictions
df_pred_02 <- tibble(nitrate = seq(min(df_count$nitrate),
                                max(df_count$nitrate),
                                length = 100))

# y_pois is exponentiated because predict() returns values in log-scale
y_normal <- predict(m_normal, newdata = df_pred_02)
y_pois <- predict(m_pois, newdata = df_pred_02) %>% exp()

df_pred_pois <- df_pred_02 %>% 
  mutate(y_normal,
         y_pois)

# figure
df_count %>% 
  ggplot(aes(x = nitrate,
             y = count)) +
  geom_point() +
  geom_line(data = df_pred_02,
            aes(y = y_normal),
            linetype = "dashed") +
  geom_line(data = df_pred_02,
            aes(y = y_pois),
            color = "salmon")

##another example for plotting
df_pred_pois <- df_count %>% 
  reframe(nitrate = seq(min(nitrate),
                        max(nitrate),
                        length = 100),
          y_pois = exp(theta[1] + theta[2] * nitrate)) # you need a tweak here

g_normal + # figure object from a Normal model
  geom_line(data = df_pred_pois,
            aes(y = y_pois),
            color = "salmon")

# offset term -------------------------------------------------------------

df_count_ue <- df_count %>% 
  mutate(area = rpois(nrow(.),10),
         count_unequal = count * area)
df_count_ue %>% 
  ggplot(aes(x = area,
         y = count_unequal))+
  geom_point()

#glm with offset term
m_pois_ue <- glm( count ~ nitrate + offset(log(area)),
     data = df_count_ue,
     family = "poisson")

summary(m_pois_ue)
