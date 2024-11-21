# set up ------------------------------------------------------------------

rm(list = ls())

# load library ------------------------------------------------------------

source(here::here("Code/set_library.R"))

# Perform only once
#install.packages("palmerpenguins")
library(palmerpenguins)

df <- penguins_raw

#(1) convert all characters to lowercase
#(2) replace white spaces with underscores _
#(3) remove unit notations
cnm_clean <- colnames(df) %>% 
  str_to_lower() %>% 
  str_replace_all(" ", "_") %>% 
  str_remove("_\\(mm\\)") %>% 
  str_remove("_\\(g\\)") %>% 
  str_remove("_\\(o/oo\\)")

colnames(df) <- cnm_clean
#convert this information to binary values: 1 for Yes and 0 for No
df_pen <- df %>% 
 mutate(success = ifelse(clutch_completion == "Yes", 
                                   yes = 1,
                                   no = 0), #ifelse works for only two 
        specie = case_when(species == "Adelie Penguin (Pygoscelis adeliae)" ~ "adelie",
                           species == "Gentoo penguin (Pygoscelis papua)" ~ "gentoo",
                           species == "Chinstrap penguin (Pygoscelis antarctica)" ~ "chinstrap"))
#remove any rows that contain missing values (NA)
df_pen <- df_pen %>% 
  drop_na(culmen_length, culmen_depth, flipper_length, body_mass)


# analyze -----------------------------------------------------------------

#Develop a statistical model that explains Clutch Completion using the variables Species, Culmen Length (mm), Culmen Depth (mm), Flipper Length (mm), Body Mass (g), and Sex

m1 <- glm (success ~ species + culmen_depth + culmen_depth + flipper_length + body_mass,
       family = "binomial",
       data = df_pen)
summary(m1)
# Perform an AIC-based model selection using the MuMIn::dredge() function
library(MuMIn)
options(na.action = "na.fail")
m_set <- dredge(m1, rank = "AIC")
subset(m_set, delta < 4)
