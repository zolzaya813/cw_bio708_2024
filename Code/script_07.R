
# setup ------------------------------------------------------------------

rm(list = ls())


# load library ------------------------------------------------------------

library(tidyverse)
library(ggplot2)


# laboratory 1.3.1 --------------------------------------------------------

##Create a new vector z with length 1000 as exp(rnorm(n = 1000, mean = 0, sd = 0.1)), and calculate the arithmetic mean, geometric mean, and median.

z <- exp(rnorm(n = 1000, 
               mean = 0, 
               sd = 1))

arith_mean <-mean(z) #arithmetic mean

geom_mean <-prod(z)^(1/length(z)) #geometric mean

med_z <-median(z)

##Draw a histogram of z using functions tibble(), ggplot(), and geom_histogram().

z_1 <- tibble(z)

g_hist <- ggplot(bin = 20, z_1, aes(x = z)) +
  geom_histogram(fill = "orange",
                 color = "white")
##raw vertical lines of arithmetic mean, geometric mean, and median on the histogram with different colors using a function geom_vline(). 
z_1 <- tibble(z)

g_hist2 <- g_hist +
  geom_vline(xintercept = arith_mean, color = "RED")+
  geom_vline(xintercept = geom_mean, color = "Green", size = 2) +
  geom_vline(xintercept = med_z, color = "blue")+
  theme_bw()

##Compare the values of the central tendency measures.
z_rev <- -z+max(z) + 0.1

arith_mean <-mean(z_rev) #arithmetic mean
(geom_mean <-exp(mean(log(z_rev)))) #geometric mean
med_z <-median(z_rev)

df_z_rev <- tibble(z_rev)

(g_hist_rev <-ggplot(bin = 20, df_z_rev, aes(x = z_rev)) +
  geom_histogram(fill = "seagreen",
                 color = "white"))
df_mu <- tibble(mu = c(arith_mean, geom_mean, med_z),
                type = c("Arithmetric", "Geometric", "Median"))
(g_hist_rev2 <- g_hist_rev +
  geom_vline(data = df_mu,
             aes(xintercept = mu,
             color = type)) +
  theme_bw())

# laboratory 1.3.2 --------------------------------------------------------

w <- rnorm(100, mean = 10, sd = 1)
head(w) # show first 10 elements in w

m <- w*1000

##SD
  
var_w <-sum((w-mean(w))^2)/length(w)
sd_w <- sqrt(var_w)

var_m <-sum((m-mean(m))^2)/length(m)
sd_m <- sqrt(var_m)

##MAD

mad_w <- median(abs(w - median(w)))
mad_m <- median(abs(m - median(m)))

##CV
cv_w <- sd_w / mean(w)
cv_m <- sd_m / mean(m)

##MAD/median
mm_w <- mad_w/ median(w)
mm_m <- mad_m/ median(m)

###Standardize the unit before analyzing 