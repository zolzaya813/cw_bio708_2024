
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
             y = Sepal.Width)) +
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
  geom_histogram(bins = 50)

##change edge color
iris %>% 
  ggplot(aes(x = Sepal.Length)) +
  geom_histogram(bins = 15,color = "blue")

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


# exercise ----------------------------------------------------------------

##1

# Library
library(networkD3)
library(dplyr)

# A connection data frame is a list of flows with intensity for each flow
links <- data.frame(
  source=c("group_A","group_A", "group_B", "group_C", "group_C", "group_E"), 
  target=c("group_C","group_D", "group_E", "group_F", "group_G", "group_H"), 
  value=c(2,3, 2, 3, 1, 3)
)

# From these flows we need to create a node data frame: it lists every entities involved in the flow
nodes <- data.frame(
  name=c(as.character(links$source), 
         as.character(links$target)) %>% unique()
)

# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
links$IDsource <- match(links$source, nodes$name)-1 
links$IDtarget <- match(links$target, nodes$name)-1

# Make the Network
p <- sankeyNetwork(Links = links, Nodes = nodes,
                   Source = "IDsource", Target = "IDtarget",
                   Value = "value", NodeID = "name", 
                   sinksRight=FALSE)
p

##2

# load library
library(ggplot2)

# Create test data.
data <- data.frame(
  category=c("A", "B", "C"),
  count=c(10, 60, 30)
)

# Compute percentages
data$fraction <- data$count / sum(data$count)

# Compute the cumulative percentages (top of each rectangle)
data$ymax <- cumsum(data$fraction)

# Compute the bottom of each rectangle
data$ymin <- c(0, head(data$ymax, n=-1))

# Compute label position
data$labelPosition <- (data$ymax + data$ymin) / 2

# Compute a good label
data$label <- paste0(data$category, "\n value: ", data$count)

# Make the plot
##note1: palette changes the color, 
##note2: theta` must be one of "x" or "y"

ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=category)) +
  geom_rect() +
  geom_label( x=3.5, aes(y=labelPosition, label=label), size=6) +
  scale_fill_brewer(palette=2) +
  coord_polar(theta="y") +
  xlim(c(2, 4)) +
  theme_void() +
  theme(legend.position = "none")
