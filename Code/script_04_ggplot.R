
# setup -------------------------------------------------------------------

rm(list = ls())


# load library ------------------------------------------------------------

library(tidyverse)


# ggplot ------------------------------------------------------------------

## scatter plot
iris %>% 
  ggplot(aes(x = Sepal.Length,
             y = Sepal.Width)) +
  geom_point()

## change color by "species"
iris %>% 
  ggplot(aes(x = Sepal.Length,
             y = Sepal.Width,
             color = Species)) +
  geom_point()
#change point color by specific color
iris %>% 
  ggplot(aes(x = Sepal.Length,
             y = Sepal.Width,)) +
  geom_point( color = "red")

# line --------------------------------------------------------------------
##geom_line()

df0 <- tibble(x = rep(1:50, 3),
              y = x * 2)

df0 %>% 
  ggplot(aes(x = x,
             y = y)) +
  geom_line()+
  geom_point(color = "orange")


# histogram ---------------------------------------------------------------
##geom_histogram() : add a histogram layer

# basic plot; bins = 30 by default
iris %>% 
  ggplot(aes(x = Sepal.Length)) +
  geom_histogram()

##change bin width
iris %>% 
  ggplot(aes(x = Sepal.Length)) +
  geom_histogram(binwidth = 0.5)

##change bin number
iris %>% 
  ggplot(aes(x = Sepal.Length)) +
  geom_histogram(bins = 50, +
                   color = "blue")
##change edge color
iris %>% 
  ggplot(aes(x = Sepal.Length)) +
  geom_histogram(bins = 50,color = "blue")

##change bar color
iris %>% 
  ggplot(aes(x = Sepal.Length)) +
  geom_histogram(bins = 50,fill = "orange",
                 color = "white")

# boxplot -----------------------------------------------------------------

iris %>% 
  ggplot(aes(x = Species,
             y = Sepal.Length)) +
  geom_boxplot()

##change color of the boxes by species
iris %>% 
  ggplot(aes(x = Species,
             y = Sepal.Length,
             color = Species)) +
  geom_boxplot()

# change fill by "Species"
iris %>% 
  ggplot(aes(x = Species,
             y = Sepal.Length,
             fill = Species)) +
  geom_boxplot()

#
iris %>% 
  ggplot(aes(x = Species,
             y = Sepal.Length,
             fill = Species)) +
  geom_boxplot(alpha = 0.2)

##
iris %>% 
  ggplot(aes(x = Sepal.Width,
             y = Sepal.Length,
             color = Petal.Length,
             alpha = Petal.Length)) +
  geom_point()
