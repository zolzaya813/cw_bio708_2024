
# setup -------------------------------------------------------------------

rm(list = ls())

##install.packages("tidyverse")

##lad library
library(tidyverse)

iris <- as_tibble(iris)

# Row manupilation --------------------------------------------------------

##filter(): select/remove rows

# single match "=="
filter(iris, Species == "virginica")

# multiple match "%in%"
filter(iris, Species %in% c("virginica", "versicolor"))

# except "!="
filter(iris, Species != "virginica")

# except multiple "!(x %in% c("a", "b"))
df_1 <- filter(iris, !(Species %in% c("virginica", "versicolor")))
df_1
# greater than ">"
filter(iris, Sepal.Length > 5)

# equal & greater than ">="
filter(iris, Sepal.Length >= 5)

# less than "<"
filter(iris, Sepal.Length < 5)

# equal & less than "<="
filter(iris, Sepal.Length <= 5)

##arrange() : arrange the order of rows

# arrange in an ascending order
arrange(iris, Sepal.Length)

# arrange in an descending order
arrange(iris, desc(Sepal.Length))

# column manupilation -----------------------------------------------------

##select(): select/remove column(s)

# select one column
select(iris, Sepal.Length)

# select multiple columns
select(iris, c(Sepal.Length, Sepal.Width))

# remove one column
select(iris, -Sepal.Length)

# remove multiple columns
select(iris, -c(Sepal.Length, Sepal.Width))

# select/remove multiple columns with a start rule
# starts_with("x")
select(iris, starts_with("Sepal"))
select(iris, -starts_with("Sepal"))

# select/remove multiple columns with an end rule
# ends_with("x")
select(iris, ends_with("Width"))
select(iris, -ends_with("Width"))

# add a new column
x <- 1:150
mutate(iris, id = x) # id is the column name


# pipping -----------------------------------------------------------------
#ctrl+shift+M is the hotkey for pipping

df_2 <- filter(iris, Species == "virginica")
df_21 <-iris %>%
  filter(Species == "virginica")

df_23 <- iris %>% 
  filter(Species == "virginica") %>% 
  select(Sepal.Length)


# reshape -----------------------------------------------------------------

##pivot_wider() : reshape a data frame to a wide format

iris_w <- iris %>% 
  mutate(id = rep(1:50, 3)) %>% # add an ID column
  select(id, Sepal.Length, Species) %>% 
  pivot_wider(id_cols = "id", # unique row ID based on
              values_from = "Sepal.Length", # values in each cell from
              names_from = "Species") # new column names from

print(iris_w, n = 50)

##pivot_longer(): reshape a data frame to a long format
iris_l <- iris_w %>% 
  pivot_longer(cols = c("setosa",
                        "versicolor",
                        "virginica"), # columns with values to be reshaped
               names_to = "Species", # column IDs move to "Species"
               values_to = "Sepal.Length") # column values move to "Sepal.Length"

print(iris_l)


# group operation ---------------------------------------------------------

##group_by() & summarize(): group-by-group operation. summarize() does not retain individual rows.

# grouping by "Species", then take means "Speal.Length" for each species
dv_3 <- iris %>% 
  group_by(Species) %>% 
  summarize(mu_sl = mean(Sepal.Length),
            signam_sl = sd(Sepal.Length))

# grouping by "Species", then take means "Speal.Length" for each species
dv_4 <- iris %>% 
  group_by(Species) %>% 
  mutate(mu_sl = mean(Sepal.Length)) %>% 
  ungroup()
