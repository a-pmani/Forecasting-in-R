##Case Study - 1 
## Author name: Alarmelu Pichu Mani 
## NetId - TJ6723
##dataset - 673_case1.csv - worldwide monthly sales from a grocery store chain

#preliminary settings 
# Set working directory
setwd("~/Documents/MSBA/sem_2/BAN 673/r_code")
library(forecast)
library(zoo)
library(formattable)
# Create data frame.
grocery.data <- read.csv("673_case1.csv")
head(grocery.data)

########################1.Identify time series components and plot the data.####################################
#1a - Create time series data set in R using the ts() function.
grocery.ts <- ts(grocery.data$Sales, 
                 start = c(2015,1), end = c(2019, 12), freq = 12)
head(grocery.ts)
tail(grocery.ts)
#1b -Employ the plot() function to create a data plot with the historical data, provide it in your
#report, and explain what data patterns can be visualized in this plot.
#plot the time series data  
plot(grocery.ts, 
     xlab = "Time", ylab = "Sales (MM)", 
     ylim = c(100, 500), main = "Monthly Sales from 2015 to 2019", col = "blue")
#to get an idea of the trend
grocery.lin <- tslm(grocery.ts ~ trend)
plot(grocery.ts, 
     xlab = "Time", ylab = "Sales (MM)", 
     ylim = c(100, 500), main = "Monthly Sales from 2015 to 2019", col = "blue")
lines(grocery.lin$fitted, lwd = 2)
grocery.stl <- stl(grocery.ts, s.window = "periodic")
autoplot(grocery.stl, main = "Grocery store sales Series Components")

#1c -Apply the Acf() function to identify possible time series components. 
#Provide in the report the autocorrelation chart and explain the time series components 
#existing in the historical data.
autocor <- Acf(grocery.ts, lag.max = 12, main = "Autocorrelation for Monthly sales of the grocery chain")

# Display autocorrelation coefficients for various lags.
Lag <- round(autocor$lag, 0)
ACF <- round(autocor$acf, 3)
data.frame(Lag, ACF)

########################2. Use trailing MA for forecasting time series.####################################

#2a -Use the rollmean() function to develop three trailing MAs (apply the entire data set with no partitioning) 
#for the window width of 2, 6, and 12, respectively.Present the R code for these MAs in your report.
ma.trailing_2 <- rollmean(grocery.ts, k = 2, align = "right")
ma.trailing_6 <- rollmean(grocery.ts, k = 6, align = "right")
ma.trailing_12 <- rollmean(grocery.ts, k = 12, align = "right")

#2b-Use the forecast() function to create a trailing MA forecast for each window width in 12 months of 2020 
#and present these forecasts in your report.
## Create trailing MA forecast for 12 periods into the future.
ma.trailing_2.pred <- forecast(ma.trailing_2, h=12, level = 0)
formattable(data.frame(ma.trailing_2.pred))
ma.trailing_6.pred <- forecast(ma.trailing_6, h=12, level = 0)
formattable(data.frame(ma.trailing_6.pred))
ma.trailing_12.pred <- forecast(ma.trailing_12, h=12, level = 0)
formattable(data.frame(ma.trailing_12.pred))

#2c -Develop a seasonal naïve forecast for the entire historical data set, and apply the accuracy() function to compare accuracy of the four models: 
#seasonal naïve forecast and trailing MAs with window width of 2, 6, and 12, respectively. Present the accuracy measures in your report, 
#compare MAPE and RMSE of these forecasts, and identify the best forecasting model.

#seasonal naive forecast for the entire dataset
grocery.snaive.pred <- snaive(grocery.ts)
formattable(data.frame(grocery.snaive.pred))

# Use accuracy() function to identify common accuracy measures.
# Use round() function to round accuracy measures to three decimal digits.
round(accuracy(grocery.snaive.pred$fitted,grocery.ts), 3)

#accuracy
round(accuracy(ma.trailing_2,grocery.ts),3)
round(accuracy(ma.trailing_6,grocery.ts),3)
round(accuracy(ma.trailing_12,grocery.ts),3)     


########################3-Apply the two-level forecast with regression and trailing MA for residuals##################
#3a - To de-trend and de-seasonalize the historical data for the entire data set, 
#develop using the tslm() function a regression model with linear trend and seasonality and 
#forecast sales in 2020 with the forecast() function. 
#Present and briefly explain the model in your report.

reg.trend.seas <- tslm(grocery.ts ~ trend + season)
summary(reg.trend.seas)
# forecast sales in 2020 with the forecast() function. 
reg.trend.seas.pred <- forecast(reg.trend.seas, h = 12, level = 0)
formattable(data.frame(reg.trend.seas.pred))

# Plot original Grocery sales time series data and regression model.
plot(grocery.ts, 
     xlab = "Time", ylab = "Sales(in MM)", ylim = c(100, 500), bty = "l",
     xaxt = "n", xlim = c(2015, 2020), lwd =2,
     main = "Grocery sales Series and Regression with Trend and Seasonality") 
axis(1, at = seq(2015,2020, 1), labels = format(seq(2015,2020, 1)))
lines(reg.trend.seas$fitted, col = "brown", lwd = 2)
lines(reg.trend.seas.pred$mean, col = "brown", lty =5, lwd = 2)
legend(1992,2300, legend = c("Sales", "Regression",
                             "Regression Forecast for 12 Periods into Future"), 
       col = c("black", "brown" , "brown"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

#3b-Identify regression residuals, apply a trailing MA (window width of 2) for these 
#residuals using the rollmean() function, and forecast worldwide monthly sales in 
#12 months of 2020 (use the forecast() function). 
#Combine the regression and trailing MA residuals’ forecast for 2020, 
#and present in your report a table that contains regression forecast, 
#trailing MA forecast for residuals, and total (combined) forecast in 2020.

#Identify regression residuals
reg.trend.seas.res <- reg.trend.seas$residuals
reg.trend.seas.res

#MA using rollmean()
ma.trailing.res_2 <- rollmean(reg.trend.seas.res, k = 2, align = "right")
ma.trailing.res_2
#create forecast for residuals for the 12 periods into the future.
ma.trailing.res_2.pred <- forecast(ma.trailing.res_2, h = 12, level = 0)
ma.trailing.res_2.pred
#combine regression forecast and trailing MA forecast for residuals.
ts.forecast.12 <- reg.trend.seas.pred$mean + ma.trailing.res_2.pred$mean
ts.forecast.12
#making a single data frame with regression,MA and forecast data
total.reg.ma.pred <- data.frame(reg.trend.seas.pred$mean, ma.trailing.res_2.pred$mean, 
                                ts.forecast.12)
formattable(data.frame(total.reg.ma.pred))

#3c-Apply the accuracy() function to compare accuracy of the three forecasting models: 
#seasonal naïve forecast (applied in question 2c), regression model with trend and seasonality, 
#and two-level (combined) model with regression and trailing MA for residuals. 
#Present the accuracy measures in your report, 
#compare MAPE and RMSE of these forecasts, and identify the best forecasting model.

#accuracy for seasonal naive forecast from 2c
round(accuracy(grocery.snaive.pred$mean,grocery.ts), 3)

#accuracy for regression model with trend and seasonality
round(accuracy(reg.trend.seas.pred$fitted, grocery.ts), 3)

#accuracy for combined model
round(accuracy(reg.trend.seas.pred$fitted+ma.trailing.res_2, grocery.ts), 3)

###############4.Use advanced exponential smoothing methods.#################################
#4a-Develop data partition with the validation partition of 12 historical periods and 
#training partition of the rest of the historical periods. 
#Present in your report the data associated with each partition.
nValid <- 12
nTrain <- length(grocery.ts) - nValid
train.ts <- window(grocery.ts, start = c(2015, 1), end = c(2015, nTrain))
valid.ts <- window(grocery.ts, start = c(2015, nTrain + 1), 
                   end = c(2015, nTrain + nValid))
data.frame(train.ts)
valid.ts
#4b-For the training partition, use the ets() function to develop a Holt-Winter’s model with multiplicative error,
#multiplicative trend, and multiplicative seasonality options, and automated selection of smoothing parameters for the training partition.
#Present and explain the model in your report. Use the model to forecast worldwide sales for the validation period using the forecast() function, 
#and present the forecast in your report.
ses.orig <- ets(train.ts, model = "MMM")
ses.orig
#forecasting
ses.orig.pred <- forecast(ses.orig, h = nValid, level = 0)
formattable(data.frame(ses.orig.pred))

#4c-To make a forecast for the 12 months of 2020, use the entire data set (no partitioning) 
#to develop the Holt-Winter’s model using the ets() function with the automated selection of error, 
#trend, and seasonality options, and automated selection of smoothing parameters. 
#Present and explain the model in your report.Use the model to forecast worldwide sales 
#for the 12 months of 2020 using the forecast() function, and present the forecast in your report.
hw.ZZZ <- ets(grocery.ts, model = "ZZZ")

#forecasting
hw.ZZZ.pred <- forecast(hw.ZZZ, h = 12, level = 0)
formattable(data.frame(hw.ZZZ.pred))

#4d-Apply the accuracy() function to compare the two forecasting models: 
#seasonal naïve forecast (applied in question 2c) and 
#Holt-Winter’s model developed in question 4c. 
#Present the accuracy measures in your report, 

#compare MAPE and RMSE of these forecasts, and identify the best forecasting model.
round(accuracy(hw.ZZZ.pred$fitted,grocery.ts), 3)

#4e-Compare the best forecasts identified in questions 3c and 4c. 
#Explain what your final choice of the forecasting model in this case will be

#accuracy for combined model- best from 3c
round(accuracy(reg.trend.seas.pred$fitted+ma.trailing.res_2, grocery.ts), 3)

#accuracy for HW "ZZZ" model - best from 4c
round(accuracy(hw.ZZZ.pred$fitted,grocery.ts), 3)
