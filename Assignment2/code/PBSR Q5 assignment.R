title: "Untitled"
author: "Souradeep Das"
date: "2022-11-16"



## Problem 5: Computational Finance - Modelling Stock prices

Following piece of code download the prices of TCS since 2007

```{r}
library(quantmod)
getSymbols('TCS.NS')

tail(TCS.NS)
```
Plot the adjusted close prices of TCS
```{r}
plot(TCS.NS$TCS.NS.Adjusted)
```

**Download the data of market index Nifty50**. The Nifty 50 index indicates how the over all market has done over the similar period.
```{r}
getSymbols('^NSEI')
tail(NSEI)
```
Plot the adjusted close value of Nifty50
```{r}
plot(NSEI$NSEI.Adjusted)
```


### Log-Return 
We calculate the daily log-return, where log-return is defined as
$$
  r_t=\log(P_t)-\log(P_{t-1})=\Delta \log(P_t),
$$
  where $P_t$ is the closing price of the stock on $t^{th}$ day.

```{r}
TCS_rt = diff(log(TCS.NS$TCS.NS.Adjusted))
Nifty_rt = diff(log(NSEI$NSEI.Adjusted))
retrn = cbind.xts(TCS_rt,Nifty_rt) 
retrn = na.omit(data.frame(retrn))

plot(retrn$NSEI.Adjusted,retrn$TCS.NS.Adjusted
     ,pch=20
     ,xlab='Market Return'
     ,ylab='TCS Return'
     ,xlim=c(-0.18,0.18)
     ,ylim=c(-0.18,0.18))
grid(col='grey',lty=1)
```

+ Consider the following model:
  
  $$
  r_{t}^{TCS}=\alpha + \beta r_{t}^{Nifty} + \varepsilon,
$$
  where $\mathbb{E}(\varepsilon)=0$ and $\mathbb{V}ar(\varepsilon)=\sigma^2$.

1. Estimate the parameters of the models $\theta=(\alpha,\beta,\sigma)$ using the method of moments type plug-in estimator discussed in the class.
```{r}
library(tinytex)
mt=TCS_mean=mean(retrn$TCS.NS.Adjusted)
varT=TCS_var=var(retrn$TCS.NS.Adjusted)
a=TCS_sd=sd(retrn$TCS.NS.Adjusted)
mn=NSEI_mean=mean(retrn$NSEI.Adjusted)
NSEI_var=var(retrn$NSEI.Adjusted)
b=NSEI_sd=sd(retrn$NSEI.Adjusted)
x=retrn$NSEI.Adjusted
y=retrn$TCS.NS.Adjusted
Cov=cov(x,y)
r=Cov/(a*b)
alpha_hat=mt-r*(a/b)*mn
beta_hat=r*(a/b)
sigma=sqrt(varT-Cov*beta_hat)
paste('The estimated value of alpha =',alpha_hat)
paste('The estimated value of beta =',beta_hat)
paste('The estimated value of sigma =',sigma)
```
```

2. Estimate the parameters using the `lm` built-in function of `R`. Note that `lm` using the OLS method.

```{r}
a=model <- lm(y~x, data=retrn)
summary(model)
alphaols = model$coefficients[[1]]
betaols = model$coefficients[[2]]
retrn$r_tcs_predicted = model$fitted.values
retrn$error = retrn$r_tcs_predicted - retrn$TCS.NS.Adjusted
sigmaols = sd(retrn$error)
paste('The estimated value of alpha =',alphaols)
paste('The estimated value of beta =',betaols)
paste('The estimated value of sigma =',sigmaols)
```


3. Fill-up the following table

Parameters | Method of Moments        | OLS
-----------|--------------------------|-----
  $\alpha$ |   0.00046165264727328    | 0.000461652647273278
$\beta$    |   0.743661766737062      | 0.743661766737066
$\sigma$   |   0.0161826159860443     | 0.0161826159860443

4. If the current value of Nifty is 18000 and it goes up to 18200. The current value of TCS is Rs. 3200/-. How much you can expect TCS price to go up?
  
  ```{r}
nif1 = 18000
nif2 = 18200
tcs1 = 3200
nifr = (log(nif2) - log(nif1))
predicttcs = predict(model, data.frame(x=c(nifr)))
tcsvalue = round(exp(predicttcs) * tcs1)
paste('The value of TCS is expected to go up at:',tcsvalue)
