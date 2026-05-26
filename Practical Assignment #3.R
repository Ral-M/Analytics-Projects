# Ralchukwu Mogbogu
# ANLY 565 Time Series and Forecasting 
# Practical Assingment #3
# Value: 150 points
# Deadline: 

#1  Check your working directory

getwd()


#2  Set your working directory to "ANLY 580/RScript". 

library(dplyr)
library(readxl)

library(nlme)


setwd("C:/Users/somto/OneDrive/Documents/ANLY 565/RScript")
print(getwd())


#3  Download "Inflation.xls" data file and set the "observation_date" 
#   variable to the date format and the "CPI" variable to the numeric format.
#   The "CPI" variable represents the consumer price index,
#   which indicates the relative prices of a consumer basket. 

inflationxl <- readxl::read_xls("Inflation.xls")
View(inflationxl)

observation_date <- as.Date(inflationxl$observation_date)
CPI <- as.numeric(inflationxl$CPI)

summary(inflationxl)
str(inflationxl)


#4  Create two stand alone variables: "date" and "cpi". 
#   "date" variable should represent the values of the "observation_date" 
#   variable from the "Inflation" data set, while, "cpi" variable should represent 
#   values of the "cpi" variable from the "Inflation" data set.

date <- inflationxl$observation_date
cpi <- inflationxl$CPI


#5 Transform "cpi" variable from numeric format to the time series format 
#   by using ts() function. Label the new variable as "cpits".  

range(date) # "1913-01-01 UTC" "2019-01-01 UTC"

cpits <- ts(cpi, start = c(1913,01), end = c(2019,01), freq = 12)
head(cpits, 12)


#6 Please construct the following three graphs:
#  1)time series plot, 2) autocorrelation
#  and 3) partial autocorrelation functions for the "cpits" variable. 
#  Based on the signature of these graphs, does the variable appear
#  stationary? Explain

plot(cpits, xlab = "Month", ylab = "CPI", 
     main = "CPI Values from 1913-01 to 2019-01")

# ts plot shows a increase/rising trend starting from around 1978, 
# indicating that the CPI variable is non-stationary

acf(cpits)
pacf(cpits)

# ACF plot shows a slight negative/decaying trend, 
# indicating that the CPI variable is non-stationary thereby showing
# significant changes in variable over time.


#7  Use "cpits" variable and window() function to create 2 new variables 
#   called "cpi.pre", "cpi.post". 
#   The "cpi.pre" should include all observations for the period starting from 
#   January of 1990 and up until October 2018.
#   The "cpi.post" should include all observations starting from November 2018.
#   and up until the last month in the dataset.

cpi.pre <- window(cpits, start = c(1990,01), end = c(2018,10), freq = 12)
cpi.post <- window(cpits, start = c(2018,11), end = c(2019,01), freq = 12)


#8  Use time() function and "cpi.pre" variable to create a variable called "Time".
#   Moreover, use "cpi.pre" variable and cycle() function to create 
#   a factor variable titled "Seas".

Time <- time(cpi.pre)
Seas <- factor(cycle(cpi.pre))


#9  Use lm() function to estimate parameter values of a linear regression model 
#   by regressing "Time", and "Seas" on "cpi.pre". 
#   Save these estimates as "cpi.lm".
#   Set the value of the intercept to 0, in order to interpret the 
#   coefficients of the seasonal dummy variables as seasonal intercepts. 
#   (Setting intercept to 0 ensures that for each season there is a unique intercept) 
#   Save these estimates as cpi.lm

cpi.lm <- lm(cpi.pre ~ 0 + Time + Seas)
summary(cpi.lm)


#10 Create the following new items: 
#   "new.Time"- sequence of 12 values starting from 2018.75+1/12
#    and each number going up by 1/12
#   "new.Seas"- a vector with the following values c(11,12,1,2,3,4,5,6,7,8,9,10)
#   "new.data"- a data frame that combines the "new.Time" and "new.Seas" variables.

new.Time <- seq(2018.75+1/12, len = 12, by = 1/12)
new.Seas <- factor(c(11,12,1,2,3,4,5,6,7,8,9,10))
new.data <- data.frame(Time = new.Time, Seas = new.Seas)


#11 Use predict() function and cpi.lm model to create a 12 month ahead forecast 
#   of the consumer price index. Save this forecast as "predict.lm"

new.data

predict.lm <- predict(cpi.lm, new.data)
predict.lm


#12 Collect residuals from the "cpi.lm" model and save them as "cpi.lm.resid".
#   Moreover, construct acf and pacf for the "cpi.lm.resid" series. 
#   Is the series stationary?
#   Is there autocorrelation in the residual series?

cpi.lm.resid <- cpi.lm$residuals

acf(cpi.lm.resid)
pacf(cpi.lm.resid)

# Based on the plots, there is autocorrelation prensent in the residual series.
# Also, the series is non-stationary.


#13  Based on the AIC, identify the best order of ARMA model 
#   (without the seasonal component) for the cpi.lm.resid time series 
#   and estimate the value of the parameter coefficients. 
#   Please, consider any ARMA model with up to 3 AR and/or MA terms.
#   Save these estimates as resid.best.arma.
#   What is the order of resid.best.arma?

best.order <- c(0, 0, 0)

best.aic <- Inf
for (i in 0:3) for (j in 0:3) {
  fit.aic <- AIC(arima(cpi.lm.resid, order = c(i, 0, j)))
  
  if (fit.aic < best.aic) {
    best.order <- c(i, 0, j)
    resid.best.arma <- arima(cpi.lm.resid, order = best.order)
    best.aic <- fit.aic
  }}

best.order # 1 0 2 : order of resid.best.arma
resid.best.arma


#14 Use predict() function and resid.best.arma to 
#   create a 12 period ahead forecast of cpi.lm.resid series.
#   Save the forecasted values as resid.best.arma.pred

resid.best.arma.pred <- predict(resid.best.arma, n.ahead = 12)

resid.best.arma.pred$pred

# -0.7179941 -0.6170488 -0.5843277 -0.5534238 -0.5242362 -0.4966696 -0.4706340 
# 2-0.4460444 -0.4228204 -0.4008862 -0.3801702 -0.3606047


#15 Use ts() function to combine the cpi values forecaseted by cpi.lm model
#   and the residual values forecasted by resid.best.arma.
#   Lable this time series as cpi.pred

# cpi.lm model uses new.data, which contains new.time as time and new.seas as seas
# cpi.lm forecast value in "predict.lm <- predict(cpi.lm, new.data)"; start = 2018.833
# end = 2019.750

cpi.pred <- ts((predict.lm + resid.best.arma.pred$pred), start = 2018.833, freq = 12)
cpi.pred


#16 Use ts.plot() function to plot cpi.pre and cpi.pred together on one graph.
#   What do you expect will happen to the CPI during the next 12 month?

ts.plot(cbind(cpi.pre, cpi.pred), lty = 1:2)

# based on cpi.pre and cpi.pred values combined, the CPI will continue in an 
# upward trend (increase) during the next 12 months.


#17 Please calculate mean absolute percentage error for the cpi.pred
#   forecast for the first three month (November 2018, December 2018, January 2019)
#   How accurate is the model? 

cpi.pred[1:3]
cpi.post[1:3]

cpforecast <- cpi.post[1:3] - cpi.pred[1:3] #forecast error
abspcterr <- abs(cpforecast / cpi.post[1:3]) * 100 #absolute percent error
 
mean(abspcterr)
# mean abs pct is 0.5375443
# accuracy is on point, with only a ~0.5% prediction error rate


#18 What is the forecasted rate of inflation between December 2018 and January 2019?
#   Hint: Inflation = % change in CPI

diff(log(cpi.pred[2:3])) * 100 
# an approximate value of 28% inflation rate between December 2018 and January 2019

diff(log(cpi.post[2:3])) * 100 
# if we view via cpi.post, the values reduce to a 19% inflation rate


#19 Policy makers often care more about inflation rather than cpi.
#   Create a new stand alone variable that would represent 
#   the first log difference of the the cpits variable. 
#   Label this variable  "pi", which represents monthly inflation rate in the US.
#   If percentage change is positive there is inflation (prices go up), 
#   and if the percentage change is negative there is deflation (prices fall). 
#   What was the lowest monthly rate of inflation(deflation) recorded in US
#   during the time sample? What about was the highest?

lcpits <- log(cpits)

pi <- (diff(lcpits))*100
pi

str(pi)
summary(pi) # min and max
# lowest monthly rate of deflation recorded during the time sample is -3.2%, while 
# the highest monthly rate of deflation is being recorded at 5.71%


#20 Please construct the time series plot, the autocorrelation
#  and partial autocorrelation functions for the "pi" variable. 
#  Based on the signature of these graphs, does the variable appear
#  stationary? Explain

plot(pi, xlab = "Inflation (%)", ylab = "Time (Monthly)", 
     main = "Inflation (1913-01 to 2019-01)")

#stationary?
acf(pi)
pacf(pi)

# based on the signature of the graphs, it shows a change in trend, with fluctuations 
# therefore, since the pattern of these graphs seems to change over time, it does not 
# indicate a stationary time series. Non-stationary.


#21  Use "pi" variable and window() function to create 2 new variables 
#   called "pi.pre", "pi.post". 
#   The "pi.pre" should include all observations for the period starting from 
#   January of 1990 and up until October 2018.
#   The "pi.post" should include all observations starting from November 2018.
#   and up until the last month in the dataset.

pi.pre <- window(pi, start = c(1990,01), end = c(2018,10), freq = 12)
pi.post <- window(pi, start = c(2018,11), end = c(2019,01), freq = 12)


#22 Please create a function that takes a time series as input, 
#   and then uses AIC to identify the best SARIMA model. 
#   The function should return the following:
#   - the order of the best SARIMA, 
#   - its AIC
#   - and the estimates of its coefficient values
#   Lable this formula get.best.sarima

get.best.sarima <- function(x.ts, maxord = c(2,2,2,2,2,2))
{
  best.aic <- Inf
  n <- length(x.ts)
  for (p in 0:maxord[2]) for(d in 0:maxord[2]) for(q in 0:maxord[2])
    for (P in 0:maxord[2]) for(D in 0:maxord[2]) for(Q in 0:maxord[2])
    {
      fit <- arima(x.ts, order = c(p,d,q),
                   seas = list(order = c(P,D,Q),
                               frequency(x.ts)), method = "CSS")
      fit.aic <- -2 * fit$loglik + (log(n) + 1) * length(fit$coef)
      if (fit.aic < best.aic)
      {
        best.aic <- fit.aic
        best.fit <- fit
        best.model <- c(p,d,q,P,D,Q)
      }
    }
  list(best.aic, best.fit, best.model)
}


#23 By using get.best.sarima() function please identify the best SARIMA model
#   for pi.pre time series. 
#   Please cosider SARIMA(2,2,2,2,2,2) as the maximum order of the model. 
#   Save the results of the get.best.sarima() function as "pi.best.sarima"
#   What is the order of the best SARIMA model?


pi.best.sarima <- get.best.sarima(pi.pre, maxord = c(2,2,2,2,2,2))
pi.best.sarima

# best order will be  0 0 1 2 1 1 for the SARIMA model; non-seasonal and seasonal


# 24 Please use predict() function and the best.sarima.pi model to forecast
#    monthly rate of inflation in the US during November 2018, December 2018
#    and January 2019.
#    Save these predictions as pi.sarima.pred

pi.sarima.pred <- predict(pi.best.sarima[[2]], n.ahead=3)
pi.sarima.pred$pred

#             Jan   Feb Mar Apr May Jun Jul Aug Sep Oct        Nov        Dec
# 2018                                                    -0.1475132    -0.2388366
# 2019      0.2646618                                                          


#25 Please calculate mean absolute percentage error of the best.sarima model.
#   How accurate is the model? 

pi.post[1:3]

# this could go two ways (because a negative pct is not standard for MAPE calculations...unless)

# First way

piforecast <- pi.post[1:3] - pi.sarima.pred$pred #forecast error
absopercterror <- abs(piforecast / pi.post[1:3]) * 100 #absolute percent error

mean(absopercterror) 

#MAPE is 40.11%; however, this means that the model has a low model accuracy; 
# although, it is a reasonable MAPE, it just is quite low im accuracy, and reading further 
# on MAPE, it seems that this value also indicates a high level of error.

# Second way

abso_err <- abs(pi.post[1:3] - pi.sarima.pred$pred)
pct_err <- (abso_err / pi.post[1:3]) * 100

mean(pct_err) # -14.14246

# sometimes in the real-world finance sector, MAPE percentage can turn out negative due to 
# observed value being in the negative itself; 
# as is the case of observed values in "pi.sarima.pred$pred" showing negative actual values
# therefore, this shows that the MAPE indicates that we have 14.14% rate lower
# than the predicted inflation rate. However, this deviates greatly for the inflation model.


# to be safe and stay in standard, I would say the first one gives a likely MAPE; 
# however, even with both MAPE values, we can say the model has a low accuracy.


#26 Extract the residual series from the pi.best.sarima model,
#   and save them as sarima.resid.

sarima.resid <- pi.best.sarima[[2]]$residuals


#27 Plot the acf of the sarima.resid series and acf of the sarima.resid^2 series 
#   What can you conclude based on these graphs?

acf(sarima.resid)
acf(sarima.resid^2)

# The first graph does not show autocorrelation, but the second graph for "sarima.resid^2"
# showed autocorrelation. We can also see some heteroscedasticity (conditional) and volatility.

# There are periods with high volatility clustering together, and same for the low volatile periods.

# However, the graphs do not show a complete account for changes in variance or anything;
# That means we would need an additional modelling to get a complete view and understanding of the
# volatility in inflation rate. 

# Garch is a good model to start with. 


#28 Download fGarch package and upload it to the library

install.packages(fGarch)
library(fGarch)


#29 Use garchFit() function from the fGarch package 
#   to estimate garch(1,1) model of the sarima.resid time series. 
#   By doing so you will be able to analyze the volatility of the 
#   inflation, or, in other words,  how stable it is.
#   Save the estimated coefficients as resid.garch

resid.garch <- garchFit(formula = ~ garch(1, 1), data = sarima.resid, 
                        include.mean = F, trace = F)
summary(resid.garch)

coef(resid.garch)
#        omega         alpha1         beta1 
#     0.0006571871   0.3346612260   0.7281965472 


#30 The main priority of the monetary authority (Federal Reserve)
#   in the United States is to ensure stable value of currency. 
#   Simply put, Fed wants to keep inflation stable (no volatility). 
#   To maintain stability the Fed depends on a number of tools, 
#   and its effectiveness is judged based on the forecasting model of volatility.
#   Please use resid.garch variable and predict function 
#   to forecast two period ahead inflation volatility, which is measured by 
#   a square of the forecasted standard deviation.
#   How stable will be the currency in February and March of 2019?


pi.resid.garch <- garchFit(formula = ~ garch(1, 1), data = pi, 
                        include.mean = F, trace = F)
summary(pi.resid.garch)

pi.inf <- predict(pi.resid.garch, n.ahead = 2)

volt.inf <- (pi.inf$standardDeviation)^2
volt.inf

# Forecasted values of volatility, showing variance, are 0.09645932 and 0.10229105.
# With inflation rates, a high volatility shows that there may be an instability in the currency.

# This may affect local and globalized business, especially with foreign exchange or even stock market,
# as inflation rates prone to volatility will create uncertainty in the economy.

# Based on this forecast result, the currency stability is low, and may progress into the next period
# due to increase in volatility.





