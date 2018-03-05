library(tidyverse)
library(knitr)
install.packages("knitr")

set.seed(87)
dat <- tibble(x = c(rnorm(100), rnorm(100)+5)-3,
              y = sin(x^2/5)/x + rnorm(200)/10 + exp(1))
kable(head(dat))


dat$distance<-abs(dat$x)
dat

arranged_data<-arrange(dat,distance)
k_subset<-arranged_data[1:5,]

Avg<- sum(k_subset$y)/nrow(k_subset)
Avg

neardist<-filter(dat,dat$distance<0.5)
neardist
Avg<- sum(neardist$y)/nrow(neardist)
Avg

###if r=0.01

neardist2<-filter(dat,dat$distance<0.01)
neardist2 # Noobservation exists

#Choosing the smaller value of r, we might overfit it. As there will be very less numbers
#in the window. But if we take a larger value, the chances of error will increase.
