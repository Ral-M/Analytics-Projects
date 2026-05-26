# Ralchukwu Mogbogu 
# ANLY 565 Time Series and Forecasting 
# Practical Assignment #1 
# Value: 150 points
# Deadline: 

#library

library(dplyr)

#1  Check you working directory

getwd()

#2  Set your working directory to "ANLY 565/RScript"

setwd("C:/Users/somto/OneDrive/Documents/ANLY 565/RScript")
print(getwd())


#3  Download goy data set posted on Canvas  and lable it 
#   goy.

library(readxl)
goy <- readxl::read_xls("goy.xls")


#This dataset reperesnets daily prices of gold,
#   oil, and the price of 1 US dollar in terms of Japanese yen.
#   Set the first column in each data set to the date format 
#   and the remaining columns in numerical format.

goy$observation_date <- as.Date(goy$observation_date)
goy$gold <- as.numeric(goy$gold)
goy$oil <- as.numeric(goy$oil)
goy$yen <- as.numeric(goy$yen)


str(goy)

#4  Create a new data set called "goycc" that contains all complete cases of goy data.
#   Utilize complete.cases function.

goycc <- goy[complete.cases(goy), ] #removes NA and leaves only complete cases
str(goycc)

View(goycc)


#5 Create a stand alone variable "date" that takes on values of "observation_date"
# variable from the goycc data set. Set the mode of the variable to character

date <- as.character(goycc$observation_date)


#6 Find the range of dates covered in goycc data set by applying range function
#  to "date" variable.  

range(date)


#7 Create a time series object called "goyccts" by utilizing goycc dataset and 
#  ts() function. In this time series object please exclude the first column 
#  of the goycc dataset. 

##dates 1971,1st month to 2019,2nd month; -1 to exclude first column; frequency 12 as per 12 months 
goyccts <- ts(data = goycc[,-1], start = c(1971,1), end = c(2019,2), frequency = 12)
print(goyccts)


#8 Reassign the value of the yen variable from the goyccts data set
#  by converting the exchange rate of yen that represents 
#  the price of 1 US Dollar in terms of Japanese yen to represent 
#  the price of 1 Yen in terms of US Dollar. 
#  This way if the number increases it represents appreciation of Yen. 
#  Hint: Reassign the value of yen variable by taking a reciprocal 

#reciprocal 1 / original yen amount; reassign dollar value to yen variable
goyccts[,"yen"] <- 1/goyccts[,"yen"]
head(goyccts[,"yen"],10)


#9 Plot the time series plot of the three assets. Do you see any trend?
# Do you see any seasonal component?

plot(goyccts)

#10 Utilize the aggregate function to plot annual average prices of the three assets.
#   How does this graph differ from the monthly time series plot?

annualavg <- aggregate(goyccts) 
plot(annualavg) 


#11 Find the average summer price of oil for the entire sample.

# looking for average IN summer JJA (June, July and August), months 06-08 of the year;
# 2-digit date format tags %m (01-12 range)
summer <- goycc[format.Date(goycc$observation_date, "%m") %in% c("06", "07", "08"), ]
summeravg <- mean(summer$oil)
print(summeravg)


# 2 ways to look for average: Will be showing that using %in% format and combining 
# or grouping the summer months TOGETHER for ease of analysis and SEPERATING the winter 
# months, factoring them separately

# 1 way shown for Summer Avg, 2nd way shown for Winter Avg

#12 Find the average winter price of oil for the entire sample.

#Winter months two-digits, and DFJ (December, January, and February); 12, 01,02
winter <- goycc[format.Date(goycc$observation_date, "%m") == "12" | 
                  format.Date(goycc$observation_date, "%m") == "01" |
                  format.Date(goycc$observation_date, "%m")== "02",]
winteravg <- mean(winter$oil)
print(winteravg)


#13 Find how the summer price of oil compares to the winter price of oil.
#   Please provide your answer in percentages. 

# economic comparison showing us the percent change would be a formula of
# ((new value - Old Value)) divided by the Old value and multiplied by 100 for 
# a percent reflection, based on the previous answers we compare averages; therefore:
pricecompare <- (summeravg - winteravg)/winteravg #0.07238298
pricecompare * 100 #percent 7.24% difference in summer price of oil as compared to winter


#14 Use window() function to create three stand alone variables 
#   "gold", "oil", and "yen" that take on values of the "gold", "oil", and "yen" 
#   variables from the goyccts dataset starting from January of 2005

#window() extracts subsets from time series
gold <- window(goyccts[,"gold"], start = c(2005,1))
oil <- window(goyccts[,"oil"], start = c(2005,1))
yen <- window(goyccts[,"yen"], start = c(2005,1))


#15 Use plot and decompose functions to generate three graphs that would depict
#   the observed values, trends, seasonal, and random components for "gold"
#   "oil" and "yen" variables. Would you choose multiplicative or 
#   additive decomposition model for each of the variables?

#Gold
plot(gold)

GDAdd <- decompose(gold, type = "additive")
plot(GDAdd)

GDMul <- decompose(gold, type = "multiplicative")
plot(GDMul)

#additive or multiplicative?:


#Oil
plot(oil)

ODAdd <- decompose(oil, type = "additive")
plot(ODAdd)

ODMul <- decompose(oil, type = "multiplicative")
plot(ODMul)

#additive or multiplicative?:


#Yen
plot(yen)

YDAdd <- decompose(yen, type = "additive")
plot(YDAdd)

YDMul <- decompose(yen, type = "multiplicative")
plot(YDMul)


# additive or multiplicative?: For each of the variable, I would lean towards an
# and overall choice of additive decomposition model. This is because when observed
# the seasonal and random components are not proportional to the trend components due to
# fluctuations and trend not really having an observed effect on the seasonal changes.
# the fluctuations are consistent. Therefore, it is safe to say the better choice from
# these decomposition model plots would be the Additive model due to no variation in
# seasonal fluctuations around the trend components in this time series level. 


#16 For each of the variables extract the random component and save 
#   them as "goldrand", "oilrand", and "yenrand". Moreover, use na.omit()
#   function to deal with the missing values.

# using choice of decomposition model: additive
goldrand <- na.omit(GDAdd$random)
oilrand <- na.omit(ODAdd$random)
yenrand <- na.omit(YDAdd$random)


#17 For the random component of each of the assets, please estimate 
#   autocorrelation function.Does any of the assets exhibit autocorrelation?
#   If yes, to what degree?
#   Keep in mind there are missing values. 

acf(goldrand)
# exhibits autocorrelation at a moderate (0.6 acf at the second lag after 0.0) degree based on the spike.

acf(oilrand)
# also exhibits autocorrelation at a degree that is relatively strong, but not  
# perfect in the sense that the second lag is more close to acf1.0 (around acf0.7 to acf0.8 to be precise)
# than it is with gold.

acf(yenrand)
# exhibits autocorrelation, but it also has a pattern in which some of the yen time series are
# slightly negatively correlated. However, it has a moderate degree of correlation 
# (around 0.6 acf at the second lag). It also has some weak correlations at later lag points
# but not really note worthy.

# overall, they all exhibit autocorrelation at moderate to strong degrees, for the most part,
# as Lag (second lag, after lag 0.0; lag 1) for gold, oil and yen has a large spike, but not at a perfect acf of 1. 
# The spikes for the second lag and the one after, surpasses the blue significant threshold/bound-line.


#18 For all possible pairs of assets please estimate cross-correlation function 
#   Do any of the variable lead or precede each other?
#   Could you use any of the varibales to predict values of other variables?
#   Make sure to use detrended and seasonally adjusted variables. 
#   ("goldrand", "oilrand", and "yenrand")

ts.plot(goldrand, oilrand, lty = c(1,3), col=c("red","blue"))

ccf(goldrand, oilrand)


ts.plot(oilrand, yenrand, lty = c(1,3), col=c("red","blue"))

ccf(oilrand, yenrand)


ts.plot(yenrand, goldrand, lty = c(1,3), col=c("red","blue"))

ccf(yenrand, goldrand)

# Goldrand could be used to predict values of oil, as it looks like Gold may be leading Oil
# Oilrand could also predict value in yen, because it shows a lead over yen
# Yen also leads the value of Gold


#19 Based on the time series plot of gold, oil, and yen prices, 
#   there appears to be no systematic trends or seasonal effects. 
#   Therefore, it is reasonable to use exponential smoothing for these time series.
#   Estimate alpha, the smoothing parameter for gold, oil and yen. 
#   What is the estimated value of the mean for each asset?

# no seasonal effects or systematic trend, beta and gamma = F or 0 for maximum smoothing
gold.hw <- HoltWinters(gold, beta = F, gamma = F)
gold.hw # alpha = 0.9999271, mean = 1319.753

oil.hw <- HoltWinters(oil, beta = F, gamma = F)
oil.hw # alpha = 0.9999263, mean = 54.94974

yen.hw <- HoltWinters(yen, beta = F, gamma = F)
yen.hw # alpha = 0.9999431, mean = 0.009054697


#   What does the value of alpha tell you tell you about the behavior of the mean? 
# to give a general answer, the alpha tells us about the behavior of the mean by informing 
# that 99.9% of the mean (0.9999 is extremely close to 1; which is very high weight) is determined by the most recent value of 
# x (gold, oil, yen) at that time (i.e., mean changes immediately according to current value).


#20 Use plot() function to generate three graphs that depict observed 
#   and HoltWinter fitted values for each asset.

plot(gold.hw)

plot(oil.hw)

plot(yen.hw)


#21 Use window() function to create 3 new variables called 
#   "goldpre", "oilpre", and "yenpre" that covers the period from January 2005, 
#   until August 2018. 

goldpre <- window(gold, start = c(2005,1), end = c(2018, 8))
oilpre <- window(oil, start = c(2005,1), end = c(2018, 8))
yenpre <- window(yen, start = c(2005,1), end = c(2018, 8))


#22 Use window() function to create 3 new variables called 
#   goldpost, oilpost, and yenpost that cover the period from September 2018, 
#   until February 2019.

goldpost <- window(gold, start = c(2018,9), end = c(2019,2))
oilpost <- window(oil, start = c(2018,9), end = c(2019,2))
yenpost <- window(yen,  start = c(2018,9), end = c(2019,2))


#23 Estimate HoltWinters filter model for each asset, while using only only pre data.
#   Save each of these estimates as "gold.hw", "oil.hw", and "yen.hw".

gold.hw <- HoltWinters(goldpre, seasonal = "additive")

oil.hw <- HoltWinters(oilpre, seasonal = "additive")

yen.hw <- HoltWinters(yenpre,  seasonal = "additive")



#24 Use HoltWinters filter estimates generated in#23 and predict() function 
#   to create a 6 month ahead forecast of the gold, oil, and yen prices. 
#   Save these forcasted values as "goldforc", "oilforc", and "yenforc".

goldforc <- predict(gold.hw, n.ahead = 6)
oilforc <- predict(oil.hw, n.ahead = 6)
yenforc <- predict(yen.hw, n.ahead = 6)


#25 Use ts.plot() function to plot side-by-side post sample prices 
#   ("goldpost", "oilpost","yenpost") and their forecasted counterparts.
#   Please designate red color to represent the actual prices, 
#   and blue doted lines to represent forecasted values. 

GPlot <- ts.plot(goldpost, goldforc, lty = 1:2, col=c("red","blue"), ylim = c(1000,1350)) 

OPlot <- ts.plot(oilpost, oilforc, lty = 1:2, col=c("red","blue"))

YPlot <- ts.plot(yenpost, yenforc, lty = 1:2, col=c("red","blue"))

#gold is first plot, Oil is second, Yen is third


#26 Please calculate forecast mean percentage error for each assets forecasting model. 
#   Which asset's forecasting model has the lowest mean percentage error?

pctgold <- mean((goldpost - goldforc)/goldpost)
pctgold *100

pctoil <- mean((oilpost - oilforc)/oilpost)
pctoil *100

pctyen <- mean((yenpost - yenforc)/yenpost)
pctyen *100


#27 Use gold, oil, and yen variables to estimate Holt-Winters model
#   for each asset. Save these estimates as "goldc.hw", "oilc.hw", and "yenc.hw".

goldc.hw <- HoltWinters(gold, seasonal = "additive")

oilc.hw <- HoltWinters(oil, seasonal = "additive")

yenc.hw <- HoltWinters(yen,  seasonal = "additive")


#28 Use "goldc.hw", "oilc.hw", and "yenc.hw" models to create an out-of-sample
#   forecasts to predict the prices of each of the assets for the rest of the 2019.
#   Save these forecasts as "goldforcos", "oilforcos", "yenforcos".
#   What is the forecasted price of Gold for November 2019? 

goldforcos <- predict(goldc.hw, n.ahead = 10)
oilforcos <- predict(oilc.hw, n.ahead = 10)
yenforcos <- predict(yenc.hw, n.ahead = 10)

# forecasted price for gold Nov 2019: two ways
goldforcosNov <- goldforcos[10]
goldforcosNov #1269.187 forecasted price for gold in Nov 2019

# OR

gfnov <- window(goldforcos, start = c(2019, 11), end = c(2019, 11))
gfnov #1276.936 forecasted price for gold in Nov 19'

# pointing this out because I noticed that using the window() function led to 
# different value; however still within the range.


# 29 Create time series plots for each asset, that combines the actual price data
#    of each asset and their out-of-sample forecasted values.
#    Please designate red color to represent the actual prices, 
#    and blue doted lines to represent forecasted values.
#    What do you think will happen to the price of each asset by the end of the year?

ts.plot(gold, goldforcos, lty = 1:4, col = c("red","blue"))

ts.plot(oil, oilforcos, lty = 1:4, col = c("red","blue"))

ts.plot(yen, yenforcos, lty = 1:4, col = c("red","blue"))

# Gold decreases by about 150 - 100 at least, by year end.

# Oil would have a mild fluctuation, whereby it increases to just about 20 pts- 
# in value, then subsequently returns to previous value (a decrease).

# Yen suffers the same fate as Oil, whereas it increases a bit, then decreases back to- 
# previous value.


# 30 Please calculate percentage change between the price of each asset in 
#    February 2019 and their forecasted December 2019 prices. 
#    Which asset promises the highest rate of return? 

# Gold
goldfeb <- window(gold, start = c(2019,2))[[1]]
golddec <- window(goldforcos, start = c(2019,12))[[1]]
gold_RR <- ((golddec - goldfeb) / goldfeb) * 100
gold_RR #-3.831629 decrease

# Oil
oilfeb <- window(oil, start = c(2019,2))[[1]]
oildec <- window(oilforcos, start = c(2019,12))[[1]]
oil_RR <- ((oildec - oilfeb) / oilfeb) * 100
oil_RR #5.550046 percent increase

# Yen
yenfeb <- window(yen, start = c(2019,2))[[1]]
yendec <- window(yenforcos, start = c(2019,12))[[1]]
yen_RR <- ((yendec - yenfeb) / goldfeb) * 100
yen_RR #0.0000005222354 percent increase

