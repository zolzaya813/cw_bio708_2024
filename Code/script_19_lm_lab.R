
# setup -------------------------------------------------------------------

rm(list = ls())

# load library ------------------------------------------------------------

source(here::here("Code/set_library.R"))

# ex: 7.3.1 ---------------------------------------------------------------

#Discuss whether the test should be applied to (1) the response variable or (2) model residuals. 
iris <- as_tibble(iris)
print(iris)

distinct(iris, Species)

# develop iris model
m_iris <- lm(Petal.Length ~ Petal.Width + Species,
             data = iris)

res_iris <- m_iris$residuals
eps <- resid(m_iris)

#Then apply the Shapiro-Wilk test to the model m_iris. Type ?shapiro.test in the R console to learn more about its usage.
shapiro.test(res_iris)
shapiro.test(eps)

# ex: 7.3.2 ---------------------------------------------------------------
#The model depicted in Figure 7.1 can be interpreted as follows: “each species has a distinct intercept value.”
#Extract the intercept values for each species from the m_iris object.
b<- coef(m_iris)
a<- NULL

a[1]<- b[1] #intercept for setosa
a[2]<- b[1]+ b[3] #intercept for versicolor
a[3]<- b[1] +b[4] #intersept for virginica

##check with plot
n_rep <- 100
df_pred <- tibble(Petal.Width = rep(seq(min(iris$Petal.Width),
                                        max(iris$Petal.Width),
                                        length = n_rep),
                                    n_distinct(iris$Species)),
                  Species = rep(unique(iris$Species),
                                each = n_rep))
y_pred <- predict(m_iris,
                  newdata = df_pred)

df_pred <- df_pred %>% 
  mutate(y_pred = y_pred)

print(df_pred)

g_org <- iris %>% 
  ggplot(aes(x = Petal.Width,
             y = Petal.Length,
             color = Species)) +
  geom_point(alpha = 0.5) +
  geom_line(data = df_pred,
            aes(y = y_pred))


# ex:7.3.3 ----------------------------------------------------------------
#Develop a model excluding the Species variable and create a new figure resembling Figure 7.1, but with a single regression line.

m_iris0 <- lm(Petal.Length ~ Petal.Width,
              data = iris)

df_new <- tibble(Petal.Width = with(iris,
                                    seq(min(Petal.Width),
                                        max(Petal.Width),
                                        length = 100)))
y_pred <- predict(m_iris0,
                  newdata = df_new)

df_new <- df_new %>%
  mutate(y_pred = y_pred)
g_org + 
  geom_line(data = df_new,
            aes(y = y_pred),
            color = grey(0, 0.5))
