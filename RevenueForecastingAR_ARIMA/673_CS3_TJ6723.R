##Case Study - 3 FORECASTING WALMART REVENUE WITH AR & ARIMA MODELS
## Author name: Alarmelu Pichu Mani 
## NetId - TJ6723
##dataset - 673_case2.csv - represents quarterly revenues (in $million) in Walmart 
##from the first quarter of 2005 through the second quarter of 2020 

#preliminary settings 
# Set working directory
setwd("~/Documents/MSBA/sem_2/BAN 673/r_code")
library(forecast)
library(zoo)
library(formattable)
options(scipen=999)
# Create data frame.
revenue.data <- read.csv("673_case2.csv")
head(revenue.data)

#Create time series data set in R using the ts() function.
revenue.ts <- ts(revenue.data$Revenue, 
                 start = c(2005,1), end = c(2020, 2), freq = 4)
head(revenue.ts)

#Develop data partition with the validation partition of 16 periods 
#and the rest for the training partition.
nValid <- 16 
nTrain <- length(revenue.ts) - nValid
train.ts <- window(revenue.ts, start = c(2005, 1), end = c(2005, nTrain))
valid.ts <- window(revenue.ts, start = c(2005, nTrain + 1), 
                   end = c(2005, nTrain + nValid))
train.ts
valid.ts

#1.Identify time series predictability.
#1a - Using the AR(1) model for the historical data, 
#Provide and explain the AR(1) model summary in your report. 
#Explain if the Walmart revenue is predictable.

# Use Arima() function to fit AR(1) model for Walmart's quaterly revenues(MM)
# The ARIMA model of order = c(1,0,0) gives an AR(1) model.
revenue.ar1<- Arima(revenue.ts, order = c(1,0,0))
summary(revenue.ar1)

#1b -Using the first differencing (lag-1) of the historical data 
#and Acf() function Provide in the report the autocorrelation 
#plot of the first differencing (lag-1) with the maximum of 8 lags 
#and explain if Walmart revenue is predictable.

# Create differenced revenue data using (lag-1).
diff.revenue.ts <- diff(revenue.ts, lag = 1)
diff.revenue.ts

revenue.ts_lag1 <- c(0, revenue.ts[1:62])
revenue.ts_lag1
diff.revenue_lag1 <- revenue.ts[2:62] - revenue.ts[1:61]
diff.revenue_lag1

# Use plot() function to create plot For first differencing, lag-1. 
plot(diff.revenue_lag1, 
     xlab = "Time", ylab = "$ in MM",
     ylim = c (-50000, 50000), main = "First Differencing of Walmart quarterly revenue", 
     xaxt = "n", bty = "l", lty = 5, lwd = 2, col="orange")
axis(1, at = seq(1, 252), labels = format(seq(1, 252)))

#plotting the lags and differenced data
Acf(diff.revenue.ts, lag.max = 8, 
    main = "Autocorrelation for Differenced Walmart Quarterly revenue data")

#2.Apply the two-level forecast with regression model and AR model for residuals.

#2a-For the training data set, use the tslm() function to develop a regression model with quadratic trend and seasonality. 
#Forecast Walmart’s revenue with the forecast() function (use the associated R code from case #2). 
                                                      
#case#2-MODEL v. Regression model with quadratic trend and seasonality.
train.Quadtrend.season <- tslm(train.ts ~ trend + I(trend^2) + season)

# See summary of quadratic trend and seasonality model and associated parameters.
summary(train.Quadtrend.season)

# Apply forecast() function to make predictions for ts with 
# trend and seasonality data in validation set.  
train.Quadtrend.season.pred <- forecast(train.Quadtrend.season, h = nValid, level = 0)
formattable(data.frame(train.Quadtrend.season.pred))

# Plot ts data, quadratic trend and seasonality ,forecast for validation period.
plot(train.Quadtrend.season.pred, 
     xlab = "Time", ylab = "Revenue ($ MM)", ylim = c(70000, 160000), bty = "l",
     xlim = c(2005, 2021), main = "Quadratic trend & Seasonality for Training and Validation Data", flty = 2) 
axis(1, at = seq(2005, 2021, 1), labels = format(seq(2005, 2021, 1)) )
lines(train.Quadtrend.season.pred$fitted, col = "blue", lwd = 2)
lines(valid.ts, col = "black", lty = 1)
legend(2008,90000, legend = c("Revenue Time Series", 
                              "Quadratic trend & Seasonality for Training and Validation Data", 
                              "Forecast for Training and Validation Data"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 3), lwd =c(2, 2, 2), bty = "n")
lines(c(2021 - 4.8, 2021 - 4.8), c(0, 160000))
lines(c(2020.25, 2020.25), c(0, 160000))
text(2010, 150000, "Training")
text(2018, 150000, "Validation")
text(2021, 150000, "Future")
arrows(2021 - 4.8, 140000, 2005, 140000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2021 - 4.8, 140000, 2020.25, 140000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.25, 140000, 2021, 140000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
#Regression model with quadratic trend and seasonality - full data set.
full.Quadtrend.season <- tslm(revenue.ts ~ trend + I(trend^2) + season)
# See summary of quadratic trend and seasonality model and associated parameters.
summary(full.Quadtrend.season)
# Apply forecast() function to make predictions for ts with 
# trend and seasonality data in validation set.  
full.Quadtrend.season.pred <- forecast(full.Quadtrend.season, h = 6, level = 0)
formattable(data.frame(full.Quadtrend.season.pred))

#2b-Identify the regression model’s residuals for the training period and use the Acf() function with 
# the maximum of 8 lags to identify autocorrelation for these residuals. 
# Provide the autocorrelation plot in your report and explain if it would be a good idea to add to 
# your forecast an AR model for residuals.

#get the residuals
train.Quadtrend.season.res <- train.Quadtrend.season$residuals
formattable(data.frame(train.Quadtrend.season.res))

# Use Acf() function to identify autocorrelation for the model residuals 
# (training and validation sets), and plot autocorrelation for different 
# lags (up to maximum of 8).
Acf(train.Quadtrend.season.pred$residuals, lag.max = 8, 
    main = "Autocorrelation for Walmart revenue Training Residuals")
Acf(valid.ts - train.trend.season.pred$mean, lag.max = 8, 
    main = "Autocorrelation for Walmart revenue Validation Residuals")

#2c-Develop an AR(1) model for the regression residuals, present and explain the 
# model and its equation in your report. Use the Acf() function for the residuals of 
# the AR(1) model (residuals of residuals), present the autocorrelation chart, and explain it in your report.

# Use Arima() function to fit AR(1) model for training residuals. The Arima model of 
# order = c(1,0,0) gives an AR(1) model.
# Use summary() to identify parameters of AR(1) model. 
res.ar1 <- Arima(train.Quadtrend.season$residuals, order = c(1,0,0))
summary(res.ar1)
res.ar1$fitted

# Use forecast() function to make prediction of residuals in validation set.
res.ar1.pred <- forecast(res.ar1, h = nValid, level = 0)
res.ar1.pred

Acf(res.ar1.pred$residuals, lag.max = 8, 
    main = "Autocorrelation for Walmart revenue residuals of Residuals")

# Develop a data frame to demonstrate the training AR model results 
# vs. original training series, training regression model, 
# and its residuals.  
train.df <- data.frame(train.ts, train.Quadtrend.season$fitted, 
                       train.Quadtrend.season$residuals, res.ar1$fitted, res.ar1$residuals)
names(train.df) <- c("Walmart Revenue", "Regression", "Reg_Residuals",
                     "AR.Model", "AR.Model.Residuals")
train.df

# Plot residuals of the predictions for training data before AR(1).
plot(train.Quadtrend.season$residuals, 
     xlab = "Time", ylab = "Residuals", ylim = c(-4000, 6000), bty = "l",
     xaxt = "n", xlim = c(2005, 2020), 
     main = "Regresssion Residuals for Training Data before AR(1)", 
     col = "brown", lwd = 3) 
axis(1, at = seq(2005, 2020, 1), labels = format(seq(2005, 2020, 1)))


# Plot residuals of the residuals for training data after AR(1).
plot(res.ar1$residuals, 
     xlab = "Time", ylab = "Residuals", ylim = c(-4000, 6000), bty = "l",
     xaxt = "n", xlim = c(2005, 2020), 
     main = "Residuals of Residuals for Training Data after AR(1)", 
     col = "brown", lwd = 3) 
axis(1, at = seq(2005, 2020, 1), labels = format(seq(2005, 2020, 1)))

#2d-Create a two-level forecasting model (regression model with quadratic trend and seasonality + AR(1) model for residuals) 
# for the validation period. Show in your report a table with the validation data, 
# regression forecast for the validation data, AR() forecast for the validation data, 
# and combined forecast for the validation period.

valid.two.level.pred <- train.Quadtrend.season.pred$mean + res.ar1.pred$mean

valid.df <- data.frame(valid.ts, train.Quadtrend.season.pred$mean, 
                       res.ar1.pred$mean, valid.two.level.pred)
names(valid.df) <- c("Walmart revenue", "Reg.Forecast", 
                     "AR(1)Forecast", "Combined.Forecast")
formattable(valid.df)

# #2e-Develop a two-level forecast (regression model with quadratic trend and seasonality and AR(1) model 
# for residuals) for the entire data set. Provide in your report the autocorrelation chart for the AR(1) model’s residuals 
# and explain it. Also, provide a data table with the models’ forecasts 
# for Walmart revenue in 2020-2021 (regression model, AR(1) for residuals, and two-level combined forecast).

# Use tslm() function to create quadratic trend and seasonality model.
trend.season <- tslm(revenue.ts ~ trend + I(trend^2) + season)

# See summary of linear trend equation and associated parameters.
summary(trend.season)

# Apply forecast() function to make predictions with quadratic trend and seasonal 
# model into the future - 2020-2021  
trend.season.pred <- forecast(trend.season, h = 6, level = 0)
formattable(data.frame(trend.season.pred))

# Plot residuals of the predictions for full data before AR(1).
plot(trend.season.pred$residuals, 
     xlab = "Time", ylab = "Residuals", ylim = c(-6000, 9000), bty = "l",
     xaxt = "n", xlim = c(2005, 2020), 
     main = "Regresssion Residuals for full Data before AR(1)", 
     col = "brown", lwd = 3) 
axis(1, at = seq(2005, 2020, 1), labels = format(seq(2005, 2020, 1)))

# Use Acf() function to identify autocorrelation for the regression residuals 
# and plot autocorrelation for different lags (up to maximum of 8).
Acf(trend.season.pred$residuals, lag.max = 8, 
    main = "Autocorrelation for Walmart revenue regression Residuals for Entire Data Set")


# Use Arima() function to fit AR(1) model for regression residuals.
# The ARIMA model order of order = c(1,0,0) gives an AR(1) model.
# Use forecast() function to make prediction of residuals into the future
residual.ar1 <- Arima(trend.season$residuals, order = c(1,0,0))
residual.ar1.pred <- forecast(residual.ar1, h = 6, level = 0)

# Use summary() to identify parameters of AR(1) model.
summary(residual.ar1)

# Plot residuals of residuals of the predictions for full data after AR(1).
plot(residual.ar1$residuals, 
     xlab = "Time", ylab = "Residuals", ylim = c(-6000, 9000), bty = "l",
     xaxt = "n", xlim = c(2005, 2020), 
     main = "Residual of Residuals for full Data after AR(1)", 
     col = "brown", lwd = 3) 
axis(1, at = seq(2005, 2020, 1), labels = format(seq(2005, 2020, 1)))

# Use Acf() function to identify autocorrealtion for the residual of residuals 
# and plot autocorrelation for different lags (up to maximum of 8).
Acf(residual.ar1$residuals, lag.max = 8, 
    main = "Autocorrelation for Walmart revenue Residuals of Residuals for Entire Data Set")


# Identify forecast for the future 6 periods as sum of quadratic trend and seasonal model
# and AR(1) model for residuals.
trend.season.ar1.pred <- trend.season.pred$mean + residual.ar1.pred$mean
trend.season.ar1.pred


# Create a data table with quadratic trend and seasonal forecast for 6 future periods,
# AR(1) model for residuals for 6 future periods, and combined two-level forecast for
# 6 future periods. 
table.df <- data.frame(trend.season.pred$mean, 
                       residual.ar1.pred$mean, trend.season.ar1.pred)
names(table.df) <- c("Reg.Forecast", "AR(1)Forecast","Combined.Forecast")
formattable(table.df)

# plot historical data, predictions for historical data, and forecast for 6 future periods.
plot(revenue.ts, 
     xlab = "Time", ylab = "Walmart quarterly revenue (in MM)", ylim = c(50000, 160000), bty = "l",
     xaxt = "n", xlim = c(2005, 2021.50), lwd = 2,
     main = "Two-Level Forecast: Regression with Trend and Seasonlity + AR(1)
     for Residuals") 
axis(1, at = seq(2005, 2021.50, 1), labels = format(seq(2005, 2021.50, 1)))
lines(trend.season$fitted+residual.ar1$fitted, col = "blue", lwd = 2)
lines(trend.season.ar1.pred, col = "blue", lty = 5, lwd = 2)
legend(2007,80000, legend = c("Walmart quarterly revenue (in MM)", 
                             "Two-Level Forecast for Training Data", "Two-Level Forecast into the future"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

# plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
# lines(c(2004.25 - 3, 2004.25 - 3), c(0, 2600))
lines(c(2005, 2005), c(0, 140000))
lines(c(2020.5, 2020.5), c(0, 140000))
text(2008, 150000, "Training")
text(2021, 150000, "Future")
arrows(2005, 140000, 2020.5, 140000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.5, 140000, 2022, 140000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

#3.Use ARIMA Model and Compare Various Methods.
#3a-Use Arima() function to fit ARIMA(1,1,1)(1,1,1) model for the training data set. 
#Insert in your report the summary of this ARIMA model, present and briefly explain the ARIMA model and 
#its equation in your report.Using this model, forecast revenue for the validation period and present it in your report.

#using Arima() function to fit ARIMA(1,1,1)(1,1,1) model for the training data set.
train.arima.seas <- Arima(train.ts, order = c(1,1,1),seasonal = c(1,1,1))
summary(train.arima.seas)

# Apply forecast() function to make predictions for ts with 
# ARIMA model in validation set.    
train.arima.seas.pred <- forecast(train.arima.seas, h = nValid, level = 0)
formattable(data.frame(train.arima.seas.pred))

# Plot the residuals of the predictions for full data.
plot(train.arima.seas$residuals, 
     xlab = "Time", ylab = "Residuals", ylim = c(-6000, 9000), bty = "l",
     xaxt = "n", xlim = c(2005, 2020), 
     main = "ARIMA(1,1,1)(1,1,1) Model Residuals for full Data", 
     col = "brown", lwd = 3) 
axis(1, at = seq(2005, 2020, 1), labels = format(seq(2005, 2020, 1)))

# Use Acf() function to create autocorrelation chart of ARIMA(1,1,1)(1,1,1) 
# model residuals.
Acf(train.arima.seas$residuals, lag.max = 12, 
    main = "Autocorrelations of ARIMA(1,1,1)(1,1,1) Model Residuals")

# Plot ts data, ARIMA model, and predictions for validation period.
plot(train.arima.seas.pred, 
     xlab = "Time", ylab = "Revenue (in MM)", ylim = c(0, 160000), bty = "l",
     xaxt = "n", xlim = c(2005, 2020.50), 
     main = "ARIMA(1,1,1)(1,1,1)[12] Model", lwd = 2, flty = 5) 
axis(1, at = seq(2005, 2020.50, 1), labels = format(seq(2005, 2020.50, 1)))
lines(train.arima.seas.pred$fitted, col = "orange", lwd = 2)
lines(valid.ts, col = "black", lwd = 2, lty = 1)
legend(x="bottomleft", legend = c("Walmart Revenue Time Series", 
                             "Seasonal ARIMA Forecast for Training Period",
                             "Seasonal ARIMA Forecast for Validation Period"), 
       col = c("black", "orange" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2005,2005), c(0, 150000))
lines(c(2016.50,2016.50), c(0, 150000))
lines(c(2020.50,2020.50), c(0, 150000))
text(2009, 160000, "Training")
text(2018, 90000, "Validation")
arrows(2005, 150000, 2016.50, 150000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2016.50, 100000, 2020.50, 100000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

#3b-Use the auto.arima() function to develop an ARIMA model using the training data set. 
# Insert in your report the summary of this ARIMA model, present and explain the ARIMA model 
# and its equation in your report. Use this model to forecast revenue in the validation period 
# and present this forecast in your report.

# Use auto.arima() function to fit ARIMA model.
# Use summary() to show auto ARIMA model and its parameters.
train.auto.arima <- auto.arima(train.ts)
summary(train.auto.arima)

# Apply forecast() function to make predictions for ts with 
# auto ARIMA model in validation set.  
train.auto.arima.pred <- forecast(train.auto.arima, h = nValid, level = 0)
formattable(data.frame(train.auto.arima.pred))

# Plot the residuals of the predictions for full data.
plot(train.auto.arima.pred$residuals, 
     xlab = "Time", ylab = "Residuals", ylim = c(-6000, 9000), bty = "l",
     xaxt = "n", xlim = c(2005, 2020), 
     main = "Auto Arima Model Residuals for full Data", 
     col = "brown", lwd = 3) 
axis(1, at = seq(2005, 2020, 1), labels = format(seq(2005, 2020, 1)))

# Using Acf() function, create autocorrelation chart of auto ARIMA 
# model residuals.
Acf(train.auto.arima$residuals, lag.max = 12,main = "Autocorrelations of Auto ARIMA Model Residuals")

# Plot ts data, ARIMA model, and predictions for validation period.
plot(train.auto.arima.pred, 
     xlab = "Time", ylab = "Revenue (in MM)", ylim = c(0, 160000), bty = "l",
     xaxt = "n", xlim = c(2005, 2020.50), 
     main = "Auto ARIMA Model", lwd = 2, flty = 5) 
axis(1, at = seq(2005, 2020.50, 1), labels = format(seq(2005, 2020.50, 1)))
lines(train.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(valid.ts, col = "black", lwd = 2, lty = 1)
legend(x="bottomleft", legend = c("Walmart Revenue Time Series", 
                                  "Auto ARIMA Forecast for Training Period",
                                  "Auto ARIMA Forecast for Validation Period"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2005,2005), c(0, 150000))
lines(c(2016.50,2016.50), c(0, 150000))
lines(c(2020.50,2020.50), c(0, 150000))
text(2009, 160000, "Training")
text(2018, 90000, "Validation")
arrows(2005, 150000, 2016.50, 150000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2016.50, 100000, 2020.50, 100000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

#3c-Apply the accuracy() function to compare performance measures of the two ARIMA models 
#in 3a and 3b. Present the accuracy measures in your report, compare them and identify, 
#using MAPE and RMSE, the best ARIMA model to apply.

#accuracy for 3a - ARIMA(1,1,1)(1,1,1)
round(accuracy(train.arima.seas.pred, valid.ts), 3)
#accuracy for 3b - Auto ARIMA
round(accuracy(train.auto.arima.pred, valid.ts), 3)

#3d-Use two ARIMA models from 3a and 3b for the entire data set. 
# Present models’ summaries in your report. Use these ARIMA models to forecast 
# Walmart revenue in 2020- 2021 and present these forecasts in your report.

#ARIMA(1,1,1)(1,1,1) - full dataset forecast for 2021
#using Arima() function to fit ARIMA(1,1,1)(1,1,1) model for the training data set.
full.arima.seas <- Arima(revenue.ts, order = c(1,1,1),seasonal = c(1,1,1))
summary(full.arima.seas)

# Apply forecast() function to make predictions for ts with 
# ARIMA model in validation set.    
full.arima.seas.pred <- forecast(full.arima.seas, h = 6, level = 0)
formattable(data.frame(full.arima.seas.pred))

# Plot the residuals of the predictions for full data.
plot(full.arima.seas.pred$residuals, 
     xlab = "Time", ylab = "Residuals", ylim = c(-6000, 9000), bty = "l",
     xaxt = "n", xlim = c(2005, 2020), 
     main = "ARIMA(1,1,1)(1,1,1) Model Residuals for full Data", 
     col = "brown", lwd = 3) 
axis(1, at = seq(2005, 2020, 1), labels = format(seq(2005, 2020, 1)))

# Use Acf() function to create autocorrelation chart of ARIMA(1,1,1)(1,1,1) 
# model residuals.
Acf(full.arima.seas.pred$residuals, lag.max = 12, 
    main = "Autocorrelations of ARIMA(1,1,1)(1,1,1) Model Residuals")

# Plot ts data, ARIMA model, and predictions for validation period.
plot(full.arima.seas.pred, 
     xlab = "Time", ylab = "Revenue (in MM)", ylim = c(0, 160000), bty = "l",
     xaxt = "n", xlim = c(2005, 2022), 
     main = "ARIMA(1,1,1)(1,1,1)[12] Model", lwd = 2, flty = 5) 
axis(1, at = seq(2005, 2022, 1), labels = format(seq(2005, 2022, 1)))
lines(full.arima.seas.pred$fitted, col = "blue", lwd = 2)
lines(full.arima.seas.pred$mean, col = "green", lwd = 2)
legend(2007,40000, legend = c("Walmart Revenue Time Series", 
                                  "Seasonal ARIMA Forecast for Training Period",
                                  "Seasonal ARIMA Forecast into the future"), 
       col = c("black", "blue" , "green"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2005,2005), c(0, 150000))
lines(c(2020.50,2020.50), c(0, 150000))
text(2009, 160000, "Training")
text(2020.90, 90000, "Future")
arrows(2005, 150000, 2020.50, 150000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.50, 100000, 2022, 100000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

#Auto ARIMA - full dataset forecast for 2021
# auto ARIMA model in validation set.  
# Use auto.arima() function to fit ARIMA model.
# Use summary() to show auto ARIMA model and its parameters.
full.auto.arima <- auto.arima(revenue.ts)
summary(full.auto.arima)

# Apply forecast() function to make predictions for ts with 
# auto ARIMA model in validation set.  
full.auto.arima.pred <- forecast(full.auto.arima, h = 6, level = 0)
formattable(data.frame(full.auto.arima.pred))

# Plot the residuals of the predictions for full data.
plot(full.auto.arima.pred$residuals, 
     xlab = "Time", ylab = "Residuals", ylim = c(-6000, 9000), bty = "l",
     xaxt = "n", xlim = c(2005, 2020), 
     main = "Auto Arima Model Residuals for full Data", 
     col = "brown", lwd = 3) 
axis(1, at = seq(2005, 2020, 1), labels = format(seq(2005, 2020, 1)))

# Using Acf() function, create autocorrelation chart of auto ARIMA 
# model residuals.
Acf(full.auto.arima.pred$residuals, lag.max = 12,main = "Autocorrelations of Auto ARIMA Model Residuals")

# Plot ts data, ARIMA model, and predictions for validation period.
plot(full.auto.arima.pred, 
     xlab = "Time", ylab = "Revenue (in MM)", ylim = c(0, 160000), bty = "l",
     xaxt = "n", xlim = c(2005, 2022), 
     main = "Auto ARIMA Model forecast", lwd = 2, flty = 5) 
axis(1, at = seq(2005, 2022, 1), labels = format(seq(2005, 2022, 1)))
lines(full.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(full.auto.arima.pred$mean, col = "green", lwd = 2)
legend(2007,40000, legend = c("Walmart Revenue Time Series", 
                              "Auto ARIMA Forecast for Training Period",
                              "Auto ARIMA Forecast into the future"), 
       col = c("black", "blue" , "green"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2005,2005), c(0, 150000))
lines(c(2020.50,2020.50), c(0, 150000))
text(2009, 160000, "Training")
text(2021.50, 90000, "Future")
arrows(2005, 150000, 2020.50, 150000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.50, 100000, 2022, 100000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

#3e-Apply the accuracy() function to 
#compare performance measures of the following forecasting models for the entire data set: 
#(1) regression model with quadratic trend and seasonality; 
#(2) two-level model (with AR(1) model for residuals); 
#(3) ARIMA(1,1,1)(1,1,1) model; 
#(4) auto ARIMA model; and 
#(5) seasonal naïve forecast for the entire data set. 
#Present the accuracy measures in your report, compare them, and identify, 
#using MAPE and RMSE, the best model to use for forecasting Walmart’s revenue in quarters 3 and 4 of 2020 and quarters 1 and 2 of 2021.

#(1) regression model with quadratic trend and seasonality;
round(accuracy(full.Quadtrend.season.pred$fitted, revenue.ts), 3)

#(2) two-level model (with AR(1) model for residuals); 
round(accuracy(trend.season.pred$fitted + residual.ar1.pred$fitted, revenue.ts), 3)

#(3) ARIMA(1,1,1)(1,1,1) model;
round(accuracy(full.arima.seas.pred$fitted, revenue.ts), 3)

#(4) auto ARIMA model
round(accuracy(full.auto.arima.pred$fitted, revenue.ts), 3)

#(5) seasonal naïve forecast for the entire data set
round(accuracy((snaive(revenue.ts))$fitted, revenue.ts), 3)


