##Case Study - 2 
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
# Create data frame.
revenue.data <- read.csv("673_case2.csv")
head(revenue.data)

################### Q1-1. Plot the data and visualize time series components.################### 
#1a. Create time series data set in R using the ts() function.
revenue.ts <- ts(revenue.data$Revenue, 
                 start = c(2005,1), end = c(2020, 2), freq = 4)
head(revenue.ts)

#1b. Apply the plot() function to create a data plot with the historical data, 
#provide it in your report, and explain what time series components can be visualized in this plot.

plot(revenue.ts, 
     xlab = "Time", ylab = "Revenue ($ MM)", 
     ylim = c(70000, 140000), main = "Quarterly revenue from 2005 Q1 to 2020 Q2", col = "blue")
#lines(revenue.lin$fitted, lwd = 2)
revenue.stl <- stl(revenue.ts, s.window = "periodic")
autoplot(revenue.stl, main = "Walmart quarterly revenue Series Components")

################### Q2-Apply five regression models using data partition.################### 
#Consider the following 5 regression-based models:
# i.Regression model with linear trend
# ii. Regression mode with quadratic trend
# iii. Regression model with seasonality
# iv. Regression model with linear trend and seasonality
# v. Regression model with quadratic trend and seasonality.

#2a-Develop data partition with the validation partition of 16 periods 
#and the rest for the training partition.
nValid <- 16 
nTrain <- length(revenue.ts) - nValid
train.ts <- window(revenue.ts, start = c(2005, 1), end = c(2005, nTrain))
valid.ts <- window(revenue.ts, start = c(2005, nTrain + 1), 
                   end = c(2005, nTrain + nValid))
train.ts
valid.ts

#2b-Use the tslm() function for the training partition to develop each of the 5 regression
# models from the above list. Apply the summary() function to identify the model structure and
# parameters for each regression model, show them in your report, and also present the
# respective model equation. Use each model to forecast revenues for the validation
# period using the forecast() function.

#i.Regression model with linear trend 
train.lin <- tslm(train.ts ~ trend)
#summary
summary(train.lin)
#model equation Yt = 81740.51 + 1024.62*t
#R_squared = coff of determination 0<=R_squared<=1 

# Apply forecast() function to make forecast for validation period.
train.lin.pred <- forecast(train.lin, h = nValid, level = 0)
formattable(data.frame(train.lin.pred))

# Plot ts data, linear trend and forecast for validation period.
plot(train.lin.pred, 
     xlab = "Time", ylab = "Revenue ($ MM)", ylim = c(70000, 160000), bty = "l",
     xlim = c(2005, 2021), main = "Linear Trend for Training and Validation Data", flty = 2) 
axis(1, at = seq(2005, 2021, 1), labels = format(seq(2005, 2021, 1)) )
lines(train.lin.pred$fitted, col = "blue", lwd = 2)
lines(valid.ts, col = "black", lty = 1)
legend(2008,90000, legend = c("Revenue Time Series", 
                              "Linear Trend for Training and Validation Data", 
                              "Forecast trend for Training and Validation Data"), 
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

# ii. Regression mode with quadratic trend
train.quad <- tslm(train.ts ~ trend + I(trend^2))
#summary
summary(train.quad)
# Apply forecast() function to make forecast for validation period.
train.quad.pred <- forecast(train.quad, h = nValid, level = 0)
formattable(data.frame(train.quad.pred))

# Plot ts data, quadratic trend and forecast for validation period.
plot(train.quad.pred, 
     xlab = "Time", ylab = "Revenue ($ MM)", ylim = c(70000, 160000), bty = "l",
     xlim = c(2005, 2021), main = "Quadratic Trend for Training and Validation Data", flty = 2) 
axis(1, at = seq(2005, 2021, 1), labels = format(seq(2005, 2021, 1)) )
lines(train.quad.pred$fitted, col = "blue", lwd = 2)
lines(valid.ts, col = "black", lty = 1)
legend(2008,90000, legend = c("Revenue Time Series", 
                              "Quadratic Trend for Training and Validation Data", 
                              "Forecast trend for Training and Validation Data"), 
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

# iii. Regression model with seasonality
train.season <- tslm(train.ts ~ season)
#summary of seasonal model and associated parameters.
summary(train.season)
#identify seasons
train.season$data 
# Apply forecast() function to make predictions for ts with 
# seasonality data in validation set.  
train.season.pred <- forecast(train.season, h = nValid, level = 0)
formattable(data.frame(train.season.pred))

# Plot ts data,seasonality and forecast for validation period.
plot(train.season.pred, 
     xlab = "Time", ylab = "Revenue ($ MM)", ylim = c(70000, 160000), bty = "l",
     xlim = c(2005, 2021), main = "Seasonality for Training and Validation Data", flty = 2) 
axis(1, at = seq(2005, 2021, 1), labels = format(seq(2005, 2021, 1)) )
lines(train.season.pred$fitted, col = "blue", lwd = 2)
lines(valid.ts, col = "black", lty = 1)
legend(2008,90000, legend = c("Revenue Time Series", 
                              "Seasonality for Training and Validation Data", 
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

# Plot residuals of the model with seasonality.
plot(train.season.pred$residuals, 
     xlab = "Time", ylab = "Residuals", ylim = c(-50000, 50000), bty = "l",
     xlim = c(2005,2020), main = "Residuals for the Seasonality Model", 
     col = "brown", lwd = 2) 
axis(1, at = seq(2005,2020, 1), labels = format(seq(2005,2020, 1)))
lines(valid.ts - train.season.pred$mean, col = "brown", lty = 1, lwd=2)

#iv. Regression model with linear trend and seasonality
train.Lintrend.season <- tslm(train.ts ~ trend + season)

# See summary of linear trend and seasonality model and associated parameters.
summary(train.Lintrend.season)

# Apply forecast() function to make predictions for ts with 
# trend and seasonality data in validation set.  
train.Lintrend.season.pred <- forecast(train.Lintrend.season, h = nValid, level = 0)
formattable(data.frame(train.Lintrend.season.pred))

# Plot ts data, linear trend and seasonality ,forecast for validation period.
plot(train.Lintrend.season.pred, 
     xlab = "Time", ylab = "Revenue ($ MM)", ylim = c(70000, 160000), bty = "l",
     xlim = c(2005, 2021), main = "Linear trend & Seasonality for Training and Validation Data", flty = 2) 
axis(1, at = seq(2005, 2021, 1), labels = format(seq(2005, 2021, 1)) )
lines(train.Lintrend.season.pred$fitted, col = "blue", lwd = 2)
lines(valid.ts, col = "black", lty = 1)
legend(2008,90000, legend = c("Revenue Time Series", 
                              "Linear trend & Seasonality for Training and Validation Data", 
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

# v. Regression model with quadratic trend and seasonality.
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

##COMPARING THE REGRESSION MODELS BASED ON ACCURACY
# accuracy for Regression model with linear trend
round(accuracy(train.lin.pred, valid.ts), 3)

# accuracy for Regression mode with quadratic trend
round(accuracy(train.quad.pred, valid.ts), 3)

# accuracy for Regression model with seasonality
round(accuracy(train.season.pred, valid.ts), 3)

# accuracy for Regression model with linear trend and seasonality
round(accuracy(train.Lintrend.season.pred, valid.ts), 3)

# accuracy for Regression model with quadratic trend and seasonality.
round(accuracy(train.Quadtrend.season.pred, valid.ts), 3)

################### Q3.Employ the entire data set to make time series forecast.################### 
#3a - Apply the two most accurate regression models identified in question to make the forecast for the 
# last two quarters of 2020 and first two quarters of 2021. For that, 
# use the entire data set to develop the regression model using the tslm() function. 
# Apply the summary() function to identify the model structure and parameters, 
# show them in your report, and also present the respective model equation. 
# Use each model to forecast Walmart’s revenue in the 4 quarters of 2020 and 2021 
# using the forecast() function, and present this forecast in your report.

#iv. Regression model with linear trend and seasonality - full dataset
revenue.Lintrend.season <- tslm(revenue.ts ~ trend + season)

# See summary of linear trend and seasonality model and associated parameters.
summary(revenue.Lintrend.season)

# Apply forecast() function to make predictions for ts with 
# trend and seasonality data in full dataset.  
revenue.Lintrend.season.pred <- forecast(revenue.Lintrend.season, h = 4, level = 0)
formattable(data.frame(revenue.Lintrend.season.pred))

# Plot ts data, linear trend and seasonality ,forecast for validation period.
plot(revenue.Lintrend.season.pred, 
     xlab = "Quarters", ylab = "Revenue ($ MM)", ylim = c(60000, 155000), bty = "l",
     xlim = c(2005, 2022.50), main = "Model with Linear Trend and Monthly Seasonality", flty = 2) 
axis(1, at = seq(2005, 2022.50, 1), labels = format(seq(2005, 2022.50, 1)) )
lines(revenue.Lintrend.season.pred$fitted, col = "blue", lwd = 2)
lines(revenue.Lintrend.season.pred$mean, col = "blue", lwd = 2, lty=5)
legend(2005,70000, legend = c("Revenue Time Series", "Linear Trend and Seasonality Model for Training Data",
                               "Linear Trend and Seasonality Forecast for Validation Data"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n",
       pch=c(".",".", "."), merge=TRUE, cex=0.8, y.intersp=0.6)
lines(c(2020.25, 2020.25), c(0, 160000))
lines(c(2021.25, 2021.25), c(0, 160000))
text(2010, 150000, "Training full dataset")
text(2021, 150000, "Future")
arrows(2020.25 - 0, 140000, 2005, 140000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.25, 140000, 2021.25, 140000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

# v. Regression model with quadratic trend and seasonality.- full data set
revenue.Quadtrend.season <- tslm(revenue.ts ~ trend + I(trend^2) + season)

# See summary of quadratic trend and seasonality model and associated parameters.
summary(revenue.Quadtrend.season)

# Apply forecast() function to make predictions for ts with 
# trend and seasonality data with full data set.  
revenue.Quadtrend.season.pred <- forecast(revenue.Quadtrend.season, h = 4, level = 0)
formattable(data.frame(revenue.Quadtrend.season.pred))

# Plot ts data, quadratic trend and seasonality ,forecast for validation period.
plot(revenue.Quadtrend.season.pred, 
     xlab = "Quarters", ylab = "Revenue ($ MM)", ylim = c(60000, 155000), bty = "l",
     xlim = c(2005, 2022.50), main = "Model with Quadratic Trend and Monthly Seasonality", flty = 2) 
axis(1, at = seq(2005, 2022.50, 1), labels = format(seq(2005, 2022.50, 1)) )
lines(revenue.Quadtrend.season.pred$fitted, col = "blue", lwd = 2)
lines(revenue.Quadtrend.season.pred$mean, col = "blue", lwd = 2, lty=5)
legend(2005,70000, legend = c("Revenue Time Series", "Quadratic Trend and Seasonality Model for Training Data",
                               "Quadratic Trend and Seasonality Forecast for Validation Data"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n",
       pch=c(".",".", "."), merge=TRUE, cex=0.8, y.intersp=0.6)
lines(c(2020.25, 2020.25), c(0, 160000))
lines(c(2021.25, 2021.25), c(0, 160000))
text(2010, 150000, "Training full dataset")
text(2021, 150000, "Future")
arrows(2020.2 - 0, 140000, 2005, 140000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.25, 140000, 2021.25, 140000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
#COMPARING ACCURACIES
# accuracy for Regression model with linear trend and seasonality
round(accuracy(revenue.Lintrend.season.pred$fitted, revenue.ts), 3)

# accuracy for Regression model with quadratic trend and seasonality.
round(accuracy(revenue.Quadtrend.season.pred$fitted, revenue.ts), 3)

# #3b - Apply the accuracy() function to compare the performance measures of 
# the regression models developed in 3a with those for naïve and seasonal 
# naïve forecasts. Present the accuracy measures in your report, 
# compare them, and identify, using MAPE and RMSE, which forecast 
# is most accurate to forecast Walmart’s quarterly revenue in 2020 and 2021.

round(accuracy((naive(revenue.ts))$fitted, revenue.ts), 3)
round(accuracy((snaive(revenue.ts))$fitted, revenue.ts), 3)


