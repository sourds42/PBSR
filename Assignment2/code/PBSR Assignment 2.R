
?faithful
attach(faithful)
head(faithful)
dim(faithful)

hist(eruptions)
hist(waiting,probability = T)
plot(faithful,pch=20)
## Bimodal data
## f(x)=p*f1(x)+(1-p)f2(x) , 0<p<1
## f(x)=p*N(52,6)+(1-p)*N(80,8)
##  suppose p = 0.5

waiting=sort(waiting)
hist(waiting,probability = T)
p=0.5
d = p*dnorm(waiting,mean = 52,sd=6)+(1-p)*dnorm(waiting,mean=80,sd=8)
lines(waiting,d,lwd=2)
d = p*dnorm(waiting,mean = 52,sd=6)+(1-p)*dnorm(waiting,mean=80,sd=6)
lines(waiting,d,lwd=2,col='blue')



p_hat = length(waiting[waiting<65])/length(waiting)
d2 = p_hat*dnorm(waiting,mean = 52,sd=6)+(1-p_hat)*dnorm(waiting,mean=80,sd=8)
lines(waiting,d2,lwd=2,col='red')

## maximum likelihood estimation (MLE) of bimodal mixture models


NegLogLikeMix <- function(theta,data){
  alpha= theta[1]
  sigma1 = theta[2]
  mu = theta[3]
  sigma2 = exp(theta[4])
  p = exp(theta[5])/(1+exp(theta[5]))
  n = length(data)
  l=0
  for(i in 1:n){
    l = l + log(p*dgamma(data[i],shape=alpha,scale=sigma1)
                +(1-p)*dnorm(data[i],mean=mu,sd=sigma2))
  }
  return(-l)
}
theta_initial=c(75,0.7,80,6,0.35)
NegLogLikeMix(theta_initial,waiting)

fit = optim(theta_initial
            ,NegLogLikeMix
            ,data=waiting
            ,control = list(maxit=1000))

theta_hat = fit$par
alpha_hat= theta_hat[1]
alpha_hat
sigma1_hat = theta_hat[2]
sigma1_hat
mu_hat = theta_hat[3]
mu_hat
sigma2_hat = exp(theta_hat[4])
sigma2_hat
p_hat = exp(theta_hat[5])/(1+exp(theta_hat[5]))
p_hat

d_mle = p_hat*dgamma(waiting,shape=alpha_hat,scale=sigma1_hat)+(1-p_hat)*dnorm(waiting,mean=mu_hat,sd=sigma2_hat)




Model 2


NegLogLikeMix1 <- function(theta,data){
  alpha_1= theta[1]
  sigma_1 =theta[2]
  alpha_2 =theta[3]
  sigma_2 =theta[4]
  p = theta[5]
  n = length(data)
  l=0
  for(i in 1:n){
    l = l + log(p*dgamma(data[i],shape=alpha_1,scale=sigma_1)
                +(1-p)*dgamma(data[i],shape=alpha_2,scale=sigma_2))
  }
  return(-l)
}

theta_initial1=c(75,0.7,87,0.9,0.5)
NegLogLikeMix1(theta_initial1,waiting)



fit1 = optim(theta_initial1
            ,NegLogLikeMix1
            ,data=waiting,
            control = list(maxit=1500))
theta_hat = fit1$par
alpha1_hat= theta_hat[1]
alpha1_hat
sigma1_hat = theta_hat[2]
sigma1_hat
alpha2_hat = theta_hat[3]
alpha2_hat
sigma2_hat = theta_hat[4]
sigma2_hat
p_hat = theta_hat[5]
p_hat

d_mle1= p_hat*dgamma(waiting,shape=alpha1_hat,scale=sigma1_hat)+(1-p_hat)*dgamma(waiting,shape=alpha2_hat,scale=sigma2_hat)


## model 3
NegLogLikeMix2=function(data,theta)
{
  mu1=(theta[1]) 
  sigma1=exp(theta[2])  
  mu2=(theta[3])
  sigma2=exp(theta[4])
  p=exp(theta[5])/(1+exp(theta[5]))
  n=length(data)
  l=0
  for(i in 1:n){
    l=l+log(p*dlnorm(data[i],mu1,sigma1)+(1-p)*dlnorm(data[i],mu2,sigma2))
  }  
  return(-l)
}

theta_initial2=c(3.9,0.015,4.4,0.012,0.5)
NegLogLikeMix2(waiting,theta_initial2)

fit2=optim(theta_initial2,NegLogLikeMix2,control=list(maxit=10000),data =waiting)
fit2

theta_hat=fit2$par
mu1_hat=theta_hat[1]
mu1_hat
sigma1_hat=exp(theta_hat[2])
sigma1_hat
mu2_hat=theta_hat[3]
mu2_hat
sigma2_hat=exp(theta_hat[4])
sigma2_hat

p_hat=exp(theta_hat[5])/(1+exp(theta_hat[5]))

d_mle2=p_hat*dlnorm(waiting,mu1_hat,sigma1_hat)+(1-p_hat)*dlnorm(waiting,mu2_hat,sigma2_hat)

data=faithful
wt_dens=density(data$waiting,n=272)
wt_dens$y

library(tidyverse)
data=data %>% arrange(waiting)

data$density=wt_dens$y
data$d1=d_mle
data$d2=d_mle1
data$d3=d_mle2
View(data)
m1 <- lm(eruptions ~ d1, data = data)
m2 <- lm(eruptions ~ d2, data = data)
m3 <- lm(eruptions ~ d3, data = data)
models=list(m1,m2,m3)

library(AICcmodavg)

#calculate AIC of each model
aictab(models,mod.names)

mod.names <- c('gamma,normal', 'gamma,gamma', 'lognormal+lognormal')

library(AICcmodavg)
aictab(models,mod.names)


## P(70<waiting<100)=integrat(70,100,f(x))

dMix<-function(x,theta){
  mu1 = theta[1]
  sigma1 = theta[2]
  mu2 = theta[3]
  sigma2 = theta[4]
  p = theta[5]
  f = p*dnorm(x,mean = mu1,sd=sigma1)+(1-p)*dnorm(x,mean=mu2,sd=sigma2)
  return(f)
}

P=integrate(dmix,60,70,c(theta_predict2))  ##p(70<waiting<100) 
P