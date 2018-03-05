library(tidyverse)
xgrid <- seq(-5, 4, length.out=1000)
kNN_estimates <- map_dbl(xgrid, function(x.input){
  dat$d <- abs(x.input - dat$x)
  arrange(dat, d)
  
  # Subsetting the first 10 rows
  dat.subset.kNN <- dat[1:5, ]
  yhat <- mean(dat.subset$y)
  return(yhat)
})

loess_estimates <- map_dbl(xgrid, function(x.input){
  dat$d <- abs(x.input - dat$x)
  arrange(dat, d)
  
  # Subsetting the first 10 rows
  dat.subset.loess <- filter(dat, dat$d<2)
  yhat <- mean(dat.subset.loess$y)
  return(yhat)
})

est <- tibble(x=xgrid, kNN=kNN_estimates, loess=loess_estimates) %>% 
  gather(key="method", value="estimate", kNN, loess)
ggplot() +
  geom_point(data=dat, mapping=aes(x,y), colour="red") +
  geom_line(data=est, 
            mapping=aes(x,estimate, group=method, colour=method)) +
  theme_bw()

