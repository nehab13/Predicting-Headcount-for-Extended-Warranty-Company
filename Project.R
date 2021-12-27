
## USE FORECAST LIBRARY.

library(forecast)

## CREATE DATA FRAME. 

# Set working directory for locating files.
setwd("/Users/neha/Documents/Capstone Project")

# Create data frame.
Call_HT.data <- read.csv("Call_HT_data_new.csv")

# See the first 6 records of the file.
head(Call_HT.data)


#Dropping unwanted columns
keeps <- c("Call_month","No.of.Calls")
Call_HT.data = Call_HT.data[keeps]
head(Call_HT.data)

## VISUALIZING TIME SERIES---------------------------------------------------------------------

## USE ts() FUNCTION TO CREATE TIME SERIES DATA SET
## AND PARTITION DATA SET.

# Function ts() takes three arguments: start, end, and freq.
# With monthly data, frequency (freq) of periods in a season (year) is 12. 
# With quarterly data, frequency in a season (year) is equal to 4.
# Arguments start and end are pairs: (season number, period number).
Call_HT.ts <- ts(Call_HT.data$No.of.Calls, 
            start = c(2017, 1), end = c(2021, 9), freq = 12)

## Use plot() to plot time series data  
options(scipen = 100)
plot(Call_HT.ts, 
     xlab = "Time", ylab = "Number of Calls", ylim = c(80000, 150000), main = "Square Trade Insurance Compnay",xaxt = "n", xlim = c(2017, 2021), col = "blue")
axis(1, at = seq(2017, 2021, 1), labels = format(seq(2017, 2021, 1)))

## TIME SERIES COMPONENTS-----------------------------------------------------------------------

# Use stl() function to plot times series components of the original data. 
# The plot includes original data, trend, seasonal, and reminder 
# (level and noise component).
Call_HT.stl <- stl(Call_HT.ts, s.window = "periodic")
autoplot(Call_HT.stl, main = "Square Trade Insurance Time Series Components")

#--------------------------------------------------------------------------------------------------------------------------------
# Define the numbers of months in the training and validation sets,
# nTrain and nValid, respectively.
# Total number of period length = 57.
# nvalid = 6 months.
# nTrain = 51 months.
nValid <- 6
nTrain <- length(Call_HT.ts) - nValid
train.ts <- window(Call_HT.ts, start = c(2017, 1), end = c(2017, nTrain))
valid.ts <- window(Call_HT.ts, start = c(2017, nTrain + 1), 
                   end = c(2017, nTrain + nValid))

#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#Applying Forecasting Method.

#MODEL 1
## FIT REGRESSION MODEL WITH LINEAR TREND. 

# Use tslm() function (time series linear model) to create regression model with 
# linear trend.
train.lin <- tslm(train.ts ~ trend)

# See summary of linear trend model and associated parameters.
summary(train.lin)

# Apply forecast() function to make forecast for validation period.
train.lin.pred <- forecast(train.lin, h = nValid, level = 0)

# Use accuracy() function to identify common accuracy measures with rounded
# values to 3 decimals.
round(accuracy(train.lin.pred$mean, valid.ts), 3)

#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#MODEL 2
## FIT REGRESSION MODEL WITH QUADRATIC (POLYNOMIAL) TREND

# Use tslm() function to create quadratic (polynomial) trend model.
train.quad <- tslm(train.ts ~ trend + I(trend^2))

# See summary of quadratic trend model and associated parameters.
summary(train.quad)

# Apply forecast() function to make predictions for ts data in
# validation set.  
train.quad.pred <- forecast(train.quad, h = nValid, level = 0)

# Use accuracy() function to identify common accuracy measures
# for regression models with  quadratic (polynomial) trend.
round(accuracy(train.quad.pred$mean, valid.ts), 3)

#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#Comparing the accuracy of Regression Model.
#(1)linear trend, (2) quadratic  
round(accuracy(train.lin.pred$mean, valid.ts), 3)
round(accuracy(train.quad.pred$mean, valid.ts), 3)


# Regression Model with quadratic trend model has Adjusted R Square - 72% and lower RMSE.
#This model can be used to predict data for the entire data set.
# Use tslm() function to create quadratic trend
trend <- tslm(Call_HT.ts ~ trend + I(trend^2))

# See summary of quadratic trend and seasonality equation and associated parameters.
summary(trend)

# Apply forecast() function to make predictions for ts with 
# trend and seasonality data in 6 future periods.
trend.pred <- forecast(trend, h = 6, level = 0)

# Use accuracy() function to identify common accuracy measures
#regression model with quadratic trend and seasonality.
round(accuracy(trend.pred$fitted, Call_HT.ts),3)

#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#MODEL 3
## HOLT-WINTER'S (HW) EXPONENTIAL SMOOTHING WITH PARTITIONED DATA, AUTOMATED
## ERROR, TREND and SEASONALITY (ZZZ) OPTIONS, AND OPTIMAL PARAMETERS
## ALPHA, BETA, AND GAMMA.

hw.ZZZ <- ets(train.ts, model = "ZZZ")
hw.ZZZ

# Use forecast() function to make predictions using this HW model with 
# validation period (nValid). 
# Show predictions in tabular format.
hw.ZZZ.pred <- forecast(hw.ZZZ, h = nValid, level = 0)
hw.ZZZ.pred

# MEASURE FORECAST ACCURACY FOR VALIDATION DATA
round(accuracy(hw.ZZZ.pred$mean, valid.ts), 3)

## FORECAST WITH HOLT-WINTER'S MODEL USING ENTIRE DATA SET INTO
## THE FUTURE FOR 6 PERIODS.

# Create Holt-Winter's (HW) exponential smoothing for full Amtrak data set. 
# Use ets() function with model = "ZZZ", to identify the best HW option
# and optimal alpha, beta, & gamma to fit HW for the entire data period.
HW.ZZZ <- ets(Call_HT.ts, model = "ZZZ")
HW.ZZZ # Model appears to be (M, N, N), with alpha = 0.8435.

# Use forecast() function to make predictions using this HW model for
# 6 month into the future.
HW.ZZZ.pred <- forecast(HW.ZZZ, h = 6 , level = 95)
HW.ZZZ.pred

# Identify performance measures for HW forecast.
round(accuracy(HW.ZZZ.pred$fitted, Call_HT.ts), 3)

#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#MODEL 4
#AUTO ARIMA
## FIT AUTO ARIMA MODEL.
train.auto.arima <- auto.arima(train.ts)
summary(train.auto.arima)

# Apply forecast() function to make predictions for ts with 
# auto ARIMA model in validation set.  
train.auto.arima.pred <- forecast(train.auto.arima, h = nValid, level = 0)
train.auto.arima.pred

# Using Acf() function, create autocorrelation chart of auto ARIMA 
# model residuals.
Acf(train.auto.arima$residuals, lag.max = 12, 
    main = "Autocorrelations of Auto ARIMA Model Residuals")

# Use accuracy() function to identify common accuracy measures 
# for validation period forecast:
round(accuracy(train.auto.arima.pred, valid.ts), 3)

## FIT AUTO ARIMA MODELS FOR ENTIRE DATA SET. 
auto.arima <- auto.arima(Call_HT.ts)
summary(auto.arima)

# Apply forecast() function to make predictions for ts with 
# auto ARIMA model in validation set.  
auto.arima.pred <- forecast(auto.arima, h = 6, level = 0)
auto.arima.pred

# Use Acf() function to create autocorrelation chart of auto ARIMA 
# model residuals.
Acf(auto.arima$residuals, lag.max = 12, 
    main = "Autocorrelations of Auto ARIMA Model Residuals")


# MEASURE FORECAST ACCURACY FOR ENTIRE DATA SET.
round(accuracy(auto.arima.pred$fitted, Call_HT.ts), 3)

#-------------------------------------------------------------------------------------------------------------------------------------
#Comparing the Accuracy of all models
#regression model with quadratic trend and seasonality.
round(accuracy(trend.pred$fitted, Call_HT.ts),3)

#Holt-Winter
round(accuracy(HW.ZZZ.pred$fitted, Call_HT.ts), 3)

#Auto Arima
round(accuracy(auto.arima.pred$fitted, Call_HT.ts), 3)