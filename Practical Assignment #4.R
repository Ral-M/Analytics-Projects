# Ralchukwu Mogbogu
# ANLY 565 Time Series and Forecasting 

#1  Check your working directory

getwd()


#2  Set your working directory to "ANLY 565/RScript". 

library(dplyr)
library(readxl)

library(nlme)

setwd("xyz/xyz")
print(getwd())



#3  Download "ffrategdp.xls" data file and set the "observation_date" 
#   variable to the date format and the "FEDFUNDS" and "GDPC1" variables to the numeric format.
#   The "FEDFUNDS" variable represents the effective federal funds rate,
#   which indicates the interest rate at which depository institutions trade federal funds 
#   (balances held at Federal Reserve Banks) with each other overnight. 
#   The "GDPC1" variable represents real gross domestic product.

ffrategdpxl <- readxl::read_xls("ffrategdp.xls")
View(ffrategdpxl)

observation_date <- as.Date(ffrategdpxl$observation_date)
FEDFUNDS <- as.numeric(ffrategdpxl$FEDFUNDS)
GDPC1 <- as.numeric(ffrategdpxl$GDPC1)

summary(ffrategdpxl) # min/start 1954-07-01, max/end 2019-04-01
str(ffrategdpxl)


#4  By using ts() function create a time series object that contains two variables: "FEDFUNDS" and "GDPC1".
#   Label it as "ffrategdpts".

# start date is in July (07) which is in the third quarter of the year,
# and the end date is in April (04) which is in the second quarter of the year.
# especially since we are dealing with a ts data with a quaterly frequency.

# since we are only using two variables, we remove "[-1]" the first element and return only two

ffrategdpts <- ts(ffrategdpxl[-1], start = c(1954,03), end = c(2019,02), freq = 4)
head(ffrategdpts, 4)

str(ffrategdpts)


#5  Create two stand alone variables "fedrate" and "gdp" that take on values of the "FEDFUNDS" and "GDPC1"
#   variables from the "ffrategdpts" data set

# first and second column of ffrategdpts

fedrate <- ffrategdpts[,1]
gdp <- ffrategdpts[,2]


#6 When federal funds rate goes down, the commercial loan interest rates go down too.
#  This means that people can borrow cheaply and invest in their businesses, 
#  which will result in higher gross domestic output. 
#  Therefore, you suspect that the federal funds rate has a negative correlation with GDP.
#  To test this hypothesis you decide to use lm() function to estimate the coefficients of 
#  a linear regression model in which "gdp" is a dependent variable and "fedrate" 
#  is as an independent variable.  
#  Save the estimated model as gdpfr.lm
#  Based on the results of this model can you make any conclusions about the nature of the 
#  relationship between the gdp and the federal funds rate?

gdpfr.lm <- lm(gdp ~ fedrate)
summary(gdpfr.lm)

# gdpfr.lm summary shows that fedrate is a significant predictor of gdp.
# the model estimates (-538.30) that as gdp has an overall negative relation with fedrate.
# also adjusted r-squared in this model is 0.15 or 15% which is low because it means only a small part of
# of the relationship is explained; concluding that the model is unfit or inaccurate. 


#7 You have suspected that the "gdp" variable may contain a unit root.
#  By using Augmented Dickey Fuller method test "gdp" variable for the 
#  the presence of unit root. 
#  Does "gdp" variable contain a unit root?
#  Is "gdp" variable stationary?

install.packages("tseries")
library(tseries)
adf.test(gdp)
# data:  gdp
# Dickey-Fuller = -1.5023, Lag order = 6, p-value = 0.7855

acf(gdp)

# the gdp cariable does contain a unit root, due to pvalue  greater than 0.05.
# failing to reject null-hypothesis.
# acf plot and unit-root presence shows the gdp variable is a non-stationary. 


#8  By using Augmented Dickey Fuller method test "fedrate" variable for the 
#  the presence of unit root.
#  Does "fedrate" variable contain a unit root?
#  Is "fedrate" variable stationary?

adf.test(fedrate)
# data:  fedrate
# Dickey-Fuller = -2.783, Lag order = 6, p-value = 0.2462


acf(fedrate)

# the federate cariable does contain a unit root, due to pvalue  greater than 0.05.
# failing to reject null-hypothesis.
# acf plot and unit-root presence shows the fedrate variable is a non-stationary. 


#9  The Phillips-Ouliaris test shows whether there is evidence that the series are
#   cointegrated, which justifies the use of a regression model. 
#   Are "gdp" and "fedrate" variables cointegrated?
#   Is "gdpfr.lm" a suitable model to explore the relationship between "gdp" and "fedrate"?

po.test(cbind(gdp, fedrate))
# Phillips-Ouliaris Cointegration Test
# data:  cbind(gdp, fedrate)
# Phillips-Ouliaris demeaned = -2.2286, Truncation lag parameter = 2, p-value = 0.15

# gdp and fedrate are not cointegrated due to p-value being greater than o.05 significance level.
# the gdpfr.lm is not a suitable model for gdp and fedrate due to their lack of cointegration.


#10 Create the following 2 new variables:
#   "gdpgrowth" - that represents quarterly percentage change in GDP
#   "fedratediff" - that represents quarterly difference in the federal funds rate (simple difference)
#   To each of the variables add "NA" as the first observation .
#   This will ensure that the new variables are of the same length as the existing variables.

gdpgrowth <- c(NA, diff(gdp) / stats::lag(gdp,-1) * 100)
head(gdpgrowth, 4)
# NA 1.956683 2.856249 1.627179

# stats::lag() cause "gdp" is under the ts class, not a vector

fedratediff <- c(NA, diff(fedrate)) #difference
head(fedratediff, 4)
#  NA -0.0400000  0.3566667  0.1566667


#11  By using ts() and cbind() functions add "gdpgrowth" and "fedratediff" variables 
#    to the "ffrategdpts" data set. 

ffrategdpts <- ts(cbind(gdpgrowth, fedratediff), start = c(1954,03), end = c(2019,02), freq = 4)
head(ffrategdpts, 4)
str(ffrategdpts)


#12 Use na.omit() function to get rid of the missing values in the "ffrategdpts" data set. 
#   Save the new data set as "ffrategdptscc". 

ffrategdptscc <- na.omit(ffrategdpts)
head(ffrategdptscc)

#13  Create 2 new variables: 
#    "ggdp" - takes on values of the "gdpgrowth" from the "ffrategdptscc"
#    "dfrate" - takes on values of the "fedratediff" from the "ffrategdptscc"

ggdp <- ffrategdptscc[,1]
dfrate <- ffrategdptscc[,2]


#14 Use to Augmented Dickey-Fuller test to determine whether "ggdp" and "dfrate"
#   are stationary or not. 
#   Does "ggdp" contain a unit root? Is "ggdp" stationary? 
#   Does "dfrate" contain a unit root? Is "dfrate" stationary?

adf.test(ggdp)
# data:  ggdp
# Dickey-Fuller = -6.0671, Lag order = 6, p-value = 0.01
# alternative hypothesis: stationary

# p value is less than 0.05, therefore we can reject null hypothesis and accept the alternate.
# ggdp is stationary and contains no unit root.

adf.test(dfrate)
# data:  dfrate
# Dickey-Fuller = -7.2109, Lag order = 6, p-value = 0.01
# alternative hypothesis: stationary

# p value is less than 0.05, therefore we can reject null hypothesis and accept the alternate.
# dfrate is stationary and contains no unit root.


#15 Use lm() function to estimate the coefficients of a linear regression model 
#   in which "ggdp" is a dependent variable and "dfrate" is as an independent variable.
#   Lable these estimates as "ggdp.dfrate.lm".
#   Based on the findings of the linear regression model what is the nature of the relationship 
#   between the growth rate of real gdp and difference in federal funds rate?

ggdp.dfrate.lm <- lm(ggdp ~ dfrate)
summary(ggdp.dfrate.lm)

# based on the linear regression model findings , there is a positive nature to the 
# relationship between the growth rate of real gdp and difference in federal funds rate. 
# Federal funds rate is a significant variable that explains and predicts the growth in GDP.


#16 Create a variable called "ggdp.dfrate.lm.resid" that represents the residual series obtained 
#   from the "ggdp.dfrate.lm" regression

ggdp.dfrate.lm.resid <- ggdp.dfrate.lm$residuals


#17 Construct acf and pacf functions for "ggdp.dfrate.lm.resid".
#   What can you say about the goodness of the fit of the model?

acf(ggdp.dfrate.lm.resid)
pacf(ggdp.dfrate.lm.resid)

# Model fit not great, autocorrelation present in acf and pacf plots for residuals.


#18 Maybe vector autoregression model would prove a better fit. 
#   Upload "vars" library that contains VAR() function

library(vars)


#19 Estimate a VAR model for the "ggdp" and "dfrate" variables.
#   In this model include 3 lags of each variable. 
#   Save the estimates of the var model as "ggdp.dfrate.var"

plot(ggdp) # significant trend
plot(dfrate) 

ggdp.dfrate.var <- VAR(cbind(ggdp, dfrate), p = 3, type = "trend")
coef(ggdp.dfrate.var)


#20 Use plot() and irf() functions to obtain and plot impulse response functions for each variable.
#   IRF illustrates the behavior of a variable in response to one standard deviation shock 
#   in its own value and in the value of the other variable.
#   Based on the these graph what conclusions can you draw about the nature of the relationship between 
#   the growth rate of gdp and the difference in federal funds rate? 
#   Any potential explanations?

ggdpirf <- irf(ggdp.dfrate.var, response = "ggdp", boot = T, nsteps =4)
dfrateirf <- irf(ggdp.dfrate.var, response = "dfrate", boot = T, nsteps =4)

plot(ggdpirf)
plot(dfrateirf)

# 4 graphs were outputed via the plot codes. to conclude from all the graphs:

# they show that difference in federal funds rate (ffr) 
# does indeed have an influence on the gdp growth, and vice versa. 
# these two factors are copdependent on each other; increasing and decreasing.
# Lag 3 is important here: graph 2 shows how ffr may have a negative impact on the growth of gdp,
# maybe after 3 quarters the difference in ffr will cause the gdp to decrease too; 
# Graph 3 shows a positive impact, but after the peak, it softly settles showing that even with the increase, 
# the ffr may still have a negative impact on gdp growth. FFR mostly negatively impacts gdp growth.

# a potential explanation could be that economic expansion or even policies that get adjusted.


#21 Use resid() function to obtain the residuals from the ggdp equation of the "ggdp.dfrate.var" model.
#   Save this residual series as "var.ggdp.resid".

var.ggdp.resid <- (resid(ggdp.dfrate.var)[,1])


#22 #17 Use resid() function to obtain the residuals from the dfrate equation of the "ggdp.dfrate.var" model.
#   Save this residual series as "var.dfrate.resid".

var.dfrate.resid <- (resid(ggdp.dfrate.var)[,2])


#24 Plot acf and pacf functions for the 'var.ggdp.resid". 
#   Does "ggdp.dfrate.var" model provide a good fit to explain growth rate of gdp?

acf(var.ggdp.resid)
pacf(var.ggdp.resid)

# acf and pacf plots indicate model doe indeed provides a good fit to explain growth rate of gdp,
# due to lack of autocorrelation present in residuals


#25 Plot acf and pacf functions for the 'var.dfrate.resid". 
#   Does "ggdp.dfrate.var" model provide a good fit to explain the difference in federal funds rate?

acf(var.dfrate.resid)
pacf(var.dfrate.resid)

# acf and pacf plots indicate model does not provides a good fit to explain ffr difference,
# due to presence of autocorrelation in residuals


#26 Use "ggdp.dfrate.var" model and predict() function to forecast growth rate of gdp and 
#   change in federal funds rate over the upcoming year. 
#   Save the predicted values as "VAR.pred"

VAR.pred <- predict(ggdp.dfrate.var, n.ahead = 4)
VAR.pred


#27 Use ts() function and VAR.pred forecast to create a new variable "ggdp.pred".
#   It should contain the forcasted values of the growth rate of gdp over the next 4 quarters.

ggdp.pred <- ts(VAR.pred$fcst$ggdp[,1], start = c(2019, 03), fr = 4)
ggdp.pred

#           Qtr1      Qtr2      Qtr3      Qtr4
#  2019                     0.6309280 0.7101314
#  2020   0.7821502 0.8437790     


#28 Use ts() function and VAR.pred forecast to create a new variable "dfrate.pred".
#   It should contain the prediction of the change in the federal funds rate over the next 4 quarters.

dfrate.pred <- ts(VAR.pred$fcst$dfrate[,1], start = c(2019, 03), fr = 4)
dfrate.pred

#         Qtr1        Qtr2        Qtr3        Qtr4
# 2019                         -0.10401903 -0.11413444
# 2020 -0.09381149 -0.09254682    


#29 Plot the times series graph of the past growth rates of gdp alongside 
#   its future forecasted values. Do you expect the gdp to grow over the next 4 quarters?

ts.plot(cbind(ggdp, ggdp.pred), lty = 1:2, col = c("blue3", "magenta2"))

# The magenta dotted line tipping upwards indicates that gdp is expected to grow 
# over the next 4 quarters.


#30 Plot the times series graph of the past changes of the federal funds rate alongside 
#   its future forecasted values. 
#   Do you expect the federal funds rate to increase over the next 4 quarters?
#   Should one take out a loan now?

ts.plot(cbind(dfrate, dfrate.pred), lty = 1:2, col = c("blue3", "magenta2"))

# FFR is not expected to witness an increase, and that is backed up by the ts plot which shows a 
# decrease over the next 4 quaters. YES, it is adviced o take a loan now cause as FFR is decreasing,
# the interest rates on loans may decrease too (lower borrowing costs); this will not affect loans with
# a fixed rate though.

