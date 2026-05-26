# ANLY 580 Time Series and Forecasting 
# Practical Assignment #2
# Value: 150 points
# Deadline:

#1  Check you working directory

getwd()


#2  Set your working directory to "ANLY 565/RScript". 
#   Upload "nlme" library

library(dplyr)
library(readxl)

library(nlme)

setwd("C:/Users/somto/OneDrive/Documents/ANLY 565/RScript")
print(getwd())


#3  Download "trade.xls" data file and set the "date" 
#   variable to the date format and the "trade" variable to
#   the numeric format. The "trade" variable represents 
#   the Ratio of Exports to Imports for China expressed in percentages.

tradexl <- readxl::read_xls("trade.xls")
View(tradexl)


date <- as.Date(tradexl$date)
trade <- as.numeric(tradexl$trade)

summary(tradexl)


#4  Create two stand alone variables: "datev" and "tradev". 
#   "datev" variable should represent values of the "date" variable 
#   from the "trade" data set, while, "tradev" variable should represent 
#   values of the "trade" variable from the "trade" data set.

datev <- tradexl$date
tradev <- tradexl$trade


#5  Use the "datev" variable and the range() function to check the time sample
#   covered by the "trade" data set. What time period is covered?

range(datev)

# What time period is covered?
# Time period covered is from "1992-01-01 UTC" to "2019-04-01 UTC"

#   What is the frequency of the data?

head(datev, 10)

# based on the data, the frequency is monthly (01 - 12) for the time period


#6  Transform "tradev" variable from numeric format to the time series format 
#   by using ts() function. Label the new variable as "tradets".  

tradets <- ts(tradev, start = c(1992,01), end = c(2019,04), freq = 12)
tradets


#7  Plot the time series graph of the "tradets"variable.
#   Please label all axis correctly, and make sure to label the graph. 
#   Based on this graph does the Ratio of Exports to Imports for China exhibit a trend? 
#   What about a regular seasonal fluctuation? 

plot(tradets, xlab = "Year", ylab = "Ratio of Exports to Imports (%)", 
     main = "Trade Ratio By Time Period")

#Trend? Yes.
#Regular Seasonal Fluctuation? Yes.

plot(decompose(tradets, type = "additive"))
plot(decompose(tradets, type = "multiplicative")) 

# The plots indicates a trend, even with the fluctuations. The plots also show
# a regular seasonal fluctuation, and it happens annually with a consistent pattern 
# of fluctuation throughout the time period. This is shown in the additive decomposition
# due to consistent nature of seasonal fluctuations (i.e, no large difference).


#8  Use "tradets" variable and window() function to create 2 new variables 
#   called "tradepre", "tradepost". 

#   The "tradepre" should include all observations for the period 
#   up until December 2018.(Last observation should be December 2018)

tradepre <- window(tradets, start = c(1992, 01), end = c(2018, 12), freq = 12)

#   The "tradepost" should include all observations starting from January 2019.
#   and up until the last month in the dataset.

tradepost <- window(tradets, start = c(2019, 01), end = c(2019, 04), freq = 12)


#9  Estimate autocorrelation function and partial autocorrelation function for 
#   the "tradepre" variable. Does the trade ratio for China exhibit autocorrelation?  
#   What process can explain this time series (white noise, random walk, AR, etc..)?

acf(tradepre)
pacf(tradepre)

# autocorrelation? yes autocorrelation is exhibited in trade ratio for china; 
# Process explained in TS is AR.


#10 Estimate AR(q) model for the "tradepre" time series. 
#   Use ar() function (set aic=FALSE) and rely on the corellologram 
#   to determine q, the order of the model. Moreover, use maximum likelihood method.
#   After that, set aic=TRUE and estimate ar() again to see if you have identified 
#   the order correctly.
#   Save the estimates as "trade.ar".

tradepref.ar <- ar(tradepre, aic = FALSE, method = "mle")
summary(tradepref.ar) ## 12

trade.ar <- ar(tradepre, aic = TRUE, method = "mle")
summary(trade.ar) # 3

tradepref.ar$order
trade.ar$order # Since TRUE = 3, then it is a better because it has a 
# better chance at model simplicity
# best fit


#11 For each of the AR coefficients estimate 95% confidence interval
#   To find 95% confidence intervals you need to add and subtract 2
#   standard deviations of the coefficient estimates. 
#   Hint you can obtain these standard deviations by applying sqrt()
#   function to the diagonal elements of the asymptotic-theory variance 
#   matrix of the coefficient estimates

trade.ar$ar # 0.3159134 0.3970605 0.1479268
arcoeff <- trade.ar$ar

arcoeff[1] + c(-2, 2) * sqrt(trade.ar$asy.var[1,1]) # 0.2061866 0.4256402
arcoeff[2] + c(-2, 2) * sqrt(trade.ar$asy.var[2,2]) # 0.2904095 0.5037114
arcoeff[3] + c(-2, 2) * sqrt(trade.ar$asy.var[3,3]) # 0.0382000 0.2576536


#12 Extract the residuals from the trade.ar model and estimate 
#   the autocorrelation function. Based on this correlogram would you say 
#   trade.ar model does a good job of explaining the trade ratio in China?

trade.ar$resid
traderesid <- trade.ar$resid

acf(traderesid[-(1:trade.ar$order)])
pacf(traderesid[-(1:trade.ar$order)])

# I would say that it does a good job, but not a great job. Both an acf and pacf
# visualizations were done. There is no autocorrelation on the positive end; but 
# shown at Lag 12 is a negative slight autocorrelation, that does not add a great 
# deal in explaining the trade ratio in china. this visualization rages from a 
# fairly good to good explanation, but surely not a great one.


#13 Use trade.ar model and predict() function to create a 4 period ahead forecast
#   of the trade ratio in China. Save these predicted values as "trade.ar.forc"

trade.ar.forc <- predict(trade.ar, n.ahead = 4)
print(trade.ar.forc)


#14 Use ts.plot() function to plot side-by-side actual values of the trade ratio
#   from January 2019-April 2019 period and their forecasted counterparts. 
#   (tradepost and trade.ar.forc)
#   Please designate red color to represent the actual observed values, 
#   and blue doted lines to represent forecasted values. 

ts.plot(tradepost, trade.ar.forc$pred, lty = c(1, 5), col=c("red","blue"))

# for the red line observed value, an increase is observed. it peaks and then
# decreases drastically. for the blue doted forecasted values, a slight increase
#, then sligtly decreases. The decrease seems like it goes back closer to the 
# original starting point. 

#   How does the ability to predict future trade ratio depends on the 
#   time horizon of the forecast?

# due to the increases, peak and then decreases. I would say the ability is limited
# and fair. Overall a poor representation of predictive feature. Which means, as times 
# goes, the prediction forecast accuracy declines.


#15 Please calculate forecast's mean absolute percentage error 
#   for the trade.ar.forc forecasting model. Why is it important to calculate 
#   mean absolute percentage error rather than mean percentage error?

tradeforcast <- tradepost - trade.ar.forc$pred #forecast error
abspercerror <- abs(tradeforcast / tradepost) * 100 #absolute percent error

mean(abspercerror) #mean absolute percent error
# 5.596669


#16 Use time() function and tradepre variable to create a variable called "Time".

Time <- time(tradepre)


#17 Estimate linear regression model by regressing "Time" on "tradepre" variable.
#   USE OLS. Save this regression model as "trade.lmt". 
#   By using confint() function calculate 95% confidence intervals for the estimated 
#   model coeficients.
#   What can you conclude based on the estimates of the model coeficients?
#   What is the direction of the time trend?

trade.lmt <- lm(tradepre ~ Time)
summary(trade.lmt) # Coefficient: (Intercept) -1.113e+03, Time  6.130e-01

confint(trade.lmt, level = 0.95) # CI 95%:
# intercept (-1457.3906131 to -769.020633), Time (~0.4414 to 0.784636)

# this concludes that as time passes (as value of time increases), then tradepre ratio 
# increases by 0.6130 or 61.3%, explaining for a positive linear relationship
# our 95% confident interval shows a range (0.4414 to 0.7846) in which has 
# no zero and is positive.

# the direction of trend will reflect the model's positive linear relationship, 
# which is positive (basically up)


#18 By visually inspecting a time series plot of the "tradepre" variable, 
#   and given the seasonal nature of the trade relationships it is reasonable to assume 
#   that there are regular seasonal fluctuations in the trade ratio for China. 
#   Use "tradepre" variable and cycle() function to create a factor variable titled "Seas".

plot.ts(tradepre)

Seas <- factor(cycle(tradepre))
#shows " Factor w/ 12 levels "1","2","3","4",..: 1 2 3 4 5 6 7 8 9 10 ..."


#19 Use lm() function to estimate linear regression model by regressing 
#   "Time" and "Seas" on "tradepre". Save this regression model as "trade.lmts".
#   Set the value of the intercept to 0, in order to interpret the 
#   coefficients of the seasonal dummy variables as seasonal intercepts. 
#   (Setting intercept to 0 ensures that for each season there is a unique intercept)

trade.lmts <- lm(tradepre ~ 0 + Time + Seas)
summary(trade.lmts) # Coefficient: (Intercept) , Time  

confint(trade.lmts, level = 0.95) # CI 95%:

#   What can you conclude based on the estimates of the model coefficients?

#Even with accounting for seasonal trends or seasonality, time still shows 
# a positive linear relationship with trade. For each increase in Time, the trade
# ratio increases by ~0.615 units.

# seas 1-12 indicates the months; all coefficient estimates are in the negatives
# ranging from -1114 to -1118.

#   What is the direction of the time trend? Positive and upwards
#   Is there a seasonal component? Yes

#   During which month should you expect the trade ratio to be the largest?
 # assuming largest would equals least negative:
 # the month closer to a positive or least negative, so that would be
 # February, then January, then November; but February is the least negative month.


#20 Extract the residual series from the "trade.lmts" model and save them as 
#   "trade.lmts.resid". Then, estimate autocorrelation function to check the 
#   goodness of the fit. What is the value of autocorrelation at lag 1?
#   What can you conclude based on the correlogram of the residual series?

trade.lmts.resid <- trade.lmts$residuals

acf(trade.lmts.resid)
#  What is the value of autocorrelation at lag 1? 
  # Close to 0.7 acf

# to be specific
    acf(trade.lmts.resid)[1] #lag 1 acf @ 0.664
    
#   What can you conclude based on the correlogram of the residual series?
  # Positive autocorrelation is observed from lags 0 - 15, then slightly from 16-18


#21 Fit linear model by regressing "Time" and "Seas" on "tradepre"
#   by utilizing generalized least squares (gls() function).
#   Set the value of the intercept to 0, in order to interpret the 
#   coefficients of the seasonal dummy variables as seasonal intercepts.
#   Save this model's estimates as "trade.gls".

trade.gls <- gls(tradepre ~ 0 + Time + Seas)
summary(trade.gls)

#   AIC      BIC    logLik
# 2525.645 2578.002 -1248.823


#22 Compute Akaike's An Information Criterion for "trade.lmts" and "trade.gls".

 #From #21 gla model, AIC for trade.gls is 2525.645

AIC (trade.lmts) # 2565.278

#   Which model performs better? Trade.gls model because of its smaller AIC,
# indicating a little to no complexity in the model; best fit


#23 Create the following new variables: 
#   "new.Time"- sequence of 4 values starting from 2019 and each number going up by 1/12
#   "alpha" - assumes value of the Time coefficient from the trade.gls model
#   "beta" - takes on values of the first, second, third, and fourth seasonal coefficients 
#            from the trade.gls model.

new.Time <- seq(2019, len = 4, by = 1/12)
alpha <- coef(trade.gls)[1] #Time (0.6148695 or 0.6149 or 0.615)
beta <- coef(trade.gls)[2:5] #Seas 1-4 (-1114.763 -1114.335 -1117.967 -1117.785) 


#24 By using the forecasting equation of x_(t+1)<-0+alpha*Time_(t+1)+beta
#   create a 4 period ahead forecast of the trade ratio for China. 
#   Label this forecast as "trade.gls.forc"

trade.gls.forc <- (0 + alpha * new.Time + beta)
trade.gls.forc

#    Seas1    Seas2    Seas3    Seas4 
#  126.6586 127.1375 123.5570 123.7897 


#25 Use ts.plot() function to plot side-by-side actual values of the trade ratio
#   from January 2019-April 2019 period and their forecasted counterparts. 
#   (tradepost and trade.gls.forecast)
#   Please designate red color to represent the actual observed values, 
#   and blue doted lines to represent forecasted values.

ts.plot(tradepost, trade.gls.forc, lty = c(1,5), col = c("red", "blue"))


#26 Please calculate forecast mean absolute percentage error 
#   for the "trade.gls.forc" forecasting model. Based on the 
#   forecast's mean absolute percentage error, which of the two models, 
#   "trade.ar.forc" and trade.gls.forc" performs better?

tglsforc <- tradepost - trade.gls.forc # forecast error
absopcterr <- abs(tglsforc / tradepost) * 100 # absolute percent error

mean(absopcterr) # mean absolute percent error 7.342898


#27 Create a variable called tradepreL, that represents the first lagged value
#   of the "tradepre" variable. For example tradepreL_t=tradepre_(t-1).
#   Moreover, transform "tradepreL" variable into a time series object by using ts().
#   It should cover the same time period as "tradepre".

tradepreL <- tradepre
for (t in c(2:length(tradepre))) {
  tradepreL[t] <- tradepre[t-1]
}
tradepreL <- ts(tradepreL)


#28 Use lm() function to estimate linear regression model by regressing 
#   "tradepreL", "Time" and "Seas" on "tradepre". 
#   Set the value of the intercept to 0, in order to interpret the 
#   coefficients of the seasonal dummy variables as seasonal intercepts.
#   Save this regression model as "trade.ar.lmts".

trade.ar.lmts <- lm(tradepre ~ 0 + tradepreL + Time + Seas)
summary(trade.ar.lmts)


#29  By using new.Time variable, and the following forecasting equation 
#    x_(t+1)<-0+alpha1*x_t+alpha2*Time_(t+1)+beta 
#    create the following new variables:
#   "alpha1" - assumes value of the tradepreL coefficient from the trade.ar.lmts model
#   "alpha2" - assumes value of the Time coefficient from the trade.ar.lmts model
#   "beta1" - takes on values of the first seasonal coefficient from the trade.ar.lmts.
#   "beta2" - takes on values of the second seasonal coefficient from the trade.ar.lmts.
#   "beta3" - takes on values of the third seasonal coefficient from the trade.ar.lmts.
#   "beta4" - takes on values of the fourth seasonal coefficient from the trade.ar.lmts.
#   "forc20191" - takes on the forecasted value of the trade ratio for January 2019
#   "forc20192" - takes on the forecasted value of the trade ratio for February 2019
#   "forc20193" - takes on the forecasted value of the trade ratio for March 2019
#   "forc20194" - takes on the forecasted value of the trade ratio for April 2019
#   "trade.ar.lmts.forc" a vector of four predicted trade ratios.


alpha1 <- coef(trade.ar.lmts)[1]
alpha2 <- coef(trade.ar.lmts)[2]

beta1 <- coef(trade.ar.lmts)[3]
beta2 <- coef(trade.ar.lmts)[4]
beta3 <- coef(trade.ar.lmts)[5]
beta4 <- coef(trade.ar.lmts)[6]

forc20191 <- 0 + alpha1*tradepre[length(tradepre)] + alpha2*new.Time[1] + beta1
forc20192 <- 0 +alpha1*forc20191 + alpha2*new.Time[2] +beta2
forc20193 <- 0 +alpha1*forc20192 + alpha2*new.Time[3] +beta3
forc20194 <- 0 +alpha1*forc20193 + alpha2*new.Time[4] +beta4

trade.ar.lmts.forc <- c(forc20191,forc20192,forc20193,forc20194)


#30 Please calculate forecast mean absolute percentage error 
#   for the trade.ar.lmts.forc forecasting model.
#   Which of the following models would you chose to based on this criteria?
#   Models: trade.ar.forc, trade.gls.forc, and trade.ar.lmts.forc)

tarforc <- tradepost - trade.ar.lmts.forc # forecast error
apcte <- abs(tarforc / tradepost) * 100
mean(apcte) # mean is  6.919727
