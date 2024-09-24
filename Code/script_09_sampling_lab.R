
# setup -------------------------------------------------------------------

rm(list = ls())

# load library ------------------------------------------------------------

source(here::here("Code/set_library.R"))


# laboratory --------------------------------------------------------------

##exercise 1

(df_h0 <- read_csv(here::here("data_raw/data_plant_height.csv")))

mu <- sigma <- NULL

for (i in 1:100){
  df_i <- df_h0 %>% 
    sample_n(50)
  
  mu[i] <- mean(df_i$height)
  sigma[i] <- var(df_i$height)
}

df50 <- tibble(mu = mu,
               sigma = sigma,
               n = 50)


for (i in 1:100){
  df_i <- df_h0 %>% 
    sample_n(100)
  
  mu[i] <- mean(df_i$height)
  sigma[i] <- var(df_i$height)
}

df100 <- tibble(mu = mu,
               sigma = sigma,
               n = 100)
##more advanced

df_m <- lapply(X = c(50,100),
               function(z) {
                 mu <- sigma <- NULL
                 
                 for(i in 10:100) {
                   df_i <- df_h0 %>% 
                     sample_n(z)
                   mu[i] <- mean(df_i$height)
                   sigma[i] <- var(df_i$height)
                 }
                 cout <- tibble(n= z,
                                mu = mu,
                                sigma = sigma)
                 return(cout)
               })%>% 
  bind_rows()
## histograms
(g_mu50 <- df50 %>% 
  ggplot(aes(x = mu)) +
  geom_histogram())

(g_mu100 <- df100 %>% 
    ggplot(aes(x = mu)) +
    geom_histogram())

(g_var50 <- df50 %>% 
    ggplot(aes(x = sigma)) +
    geom_histogram())

(g_var100 <- df100 %>% 
    ggplot(aes(x = sigma)) +
    geom_histogram())

g_mu50 / g_mu100 / g_var50 / g_var100

##more advanced - plot mu and sigma together using facet_wrap
g_r <- df_m %>% 
  pivot_longer(cols = c(mu, sigma),
               names_to = "parm", 
               values_to = "value") %>% 
  ggplot(aes(x = value,
             color = factor(n))) +
  geom_density() + 
  facet_wrap(facets =~ parm,
             scales = "free")

##exercise 2
df_m_nr <- lapply(X = c(50, 100),
                  function(z) {
                    
                    mu <- sigma <- NULL
                    
                    for(i in 1:100) {
                      df_i <- df_h0 %>%
                        filter(height >= 10) %>% #the following code excludes those less than 10 cm in height
                        sample_n(z)
                      
                      mu[i] <- mean(df_i$height)
                      sigma[i] <- var(df_i$height)
                    }
                    
                    cout <- tibble(n = z,
                                   mu = mu,
                                   sigma = sigma)
                    
                    return(cout)
                  }) %>% 
  bind_rows()

## plotting
g_nr <- df_m_nr %>% 
  pivot_longer(cols = c(mu, sigma),
               names_to = "parm", 
               values_to = "value") %>% 
  ggplot(aes(x = value,
             color = factor(n))) +
  geom_density() + 
  facet_wrap(facets =~ parm,
             scales = "free",
             labeller = label_parsed) + # label_parsed `parse` input text
  labs(x = "Plant height (cm)",
       y = "Density",
       color = "Sample size") +
  theme_bw() +
  theme(strip.background = element_blank()) ##helps visualizing 

## compare random vs non-random
g_r / g_nr

