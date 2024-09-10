
# setup -------------------------------------------------------------------

rm(list = ls())


# central tendency --------------------------------------------------------

# construct vectors x and y
x <- c(15.9, 15.1, 21.9, 13.3, 24.4)
y <- c(15.9, 15.1, 21.9, 53.3, 24.4)

#calculate arithmetic means for x and y
##method 1
mean(x)
mean(y)

##method 2
sum(x)/ length(x)
sum(y)/ length(y)

#calculate geometric mean
##prod() multily all elements in a vector
prod(x)^(1/length(x))
prod(y)^(1/length(y))

##or mean in a log scale then transform back to an ordinary scale
log_y <- log(y)
exp(mean(log_y))

#calculate median for x and y
##use median()
median(x)
median(y)

##manual way
x <- sort(x) # sort x from small to large
index <- (length(x) + 1) / 2 # (N + 1)/2 th index as length(x) is an odd number
med_x <- x[index]
print(med_x)


y <- sort(y) # sort y from small to large
med_y <- y[(length(y) + 1) / 2] 
print(med_y)


# variation ---------------------------------------------------------------

##calculate variance and sd for x and y
###manual way
var_x <-sum((x-mean(x))^2)/length(x)
sd_x <- sqrt(var_x)

var_y <-sum((y-mean(y))^2)/length(y)
sd_y <- sqrt(var_y)

##calculate Inter-Quantile Range = quantile(): return quantile values
x_25 <- quantile(x, 0.25) 
x_75 <- quantile(x, 0.75)
# abs(): absolute value
iqr_x <- abs(x_25 - x_75) 
print(iqr_x)

yq <- quantile(y, c(0.25, 0.75))
(iqr_y <- abs(yq[1]-yq[2]))

# relative varience -------------------------------------------------------

##coefficient variation
(cv_x <- sd_x/ mean(x))

(cv_y <- sd_y/ mean(y))

##IQR / median

iqr_x / med_x
abs(diff(quantile(y, c(0.25, 0.75)))/ med_y)
