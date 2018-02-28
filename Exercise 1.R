install.packages("tidyverse")
install.packages("ISLR")
install.packages("knitr")

library(tidyverse)

genreg <- function(n){
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  eps <- rnorm(n)
  y <- 5-x1+2*x2+eps
  tibble(x1=x1, x2=x2, y=y) #tibble is a fancy dataframe
}
dat<-genreg(1000)

#Mutate adds the predicted values(as column) to the original data set 
dat<-mutate(dat,
  yhat=5,
  yhat1 = 5-x1,
  yaht2 = 5+2*x2,
  yhat3 = 5-x1+2*x2)
dat
#We took yhat =5, as the mean is 0 and std devation is 6, so it can deviate between 0 and 6
#Since we did not use the mean which is zero , reducible errors can be introduced but we took this bcz MSE was very high

(mse <- mean((dat$yhat-dat$y)^2))
(mse1<- mean((dat$yhat1-dat$y)^2))
(mse2<- mean((dat$yaht2-dat$y)^2))
(mse3<- mean((dat$yhat3-dat$y)^2))

#######
#When X=1
x=1
(pB <- 0.8/(1+exp(-x)))
(pA <- 0.2)
(pC<-1-(0.2+pB))

x=-2
(pB1 <- 0.8/(1+exp(-x)))
(pA1 <- 0.2)
(pC1<-1-(0.2+pB))

#Explanation of plot, in above probablities, B is the highest when x=1 so, we will predict class B
# In plot when x>0 since is highest when x=-2 , we will predict class B so, the graph area below the curve, when x>0 is for class B
#When x<0, We will predict class C, so the graph area above the curve, when x<0, corresponds to probabilty of class C
#We will not predict class A as it has the lowest probablity.

gencla <- function(n) {
  x <- rnorm(n) 
  pB <- 0.8/(1+exp(-x))
  y <- map_chr(pB, function(t) 
    sample(LETTERS[1:3], size=1, replace=TRUE,
           prob=c(0.2, t, 1-t-0.2)))
  tibble(x=x, y=y)
}
#We are using alphabet t as a replacement of pB
dat2<- gencla(1000)
  
dat2<-mutate(dat2,yhat=sapply(x,function(x_)
            if(x_<0)"C" else "B"))
dat2

MSE2<-1-(mean(dat2$yhat==dat2$y))
MSE2  

