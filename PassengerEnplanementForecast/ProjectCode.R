##FORECASTING TRAVEL IN TIMES OF THE COVID PANDEMIC
##Submitted by Ankita Chadha (NetID: gb9655) and Alarmelu Pichu Mani (NetID:TJ6723 )

## USE FORECAST LIBRARY.

#install.packages
library(forecast)
library(zoo)
library(ggplot2)
options(scipen=999)

## CREATE DATA FRAME. 

# Set working directory for locating files.
setwd("~/Documents/MSBA/sem_2/BAN 673/project")
#setwd("C:\\Users\\stsc\\Desktop\\MSBA\\BAN673_TimeSeries\\TermProject")

############### DATA LOAD #######################################################
########## Reading covid daily data and creating ts dataframe ###################

covid.data <- read.csv("covid_data1.csv", header = TRUE, strip.white = TRUE)

# See the first 6 records of the file.
head(covid.data)

## Create a daily Date object - helps my work on dates
inds <- seq(as.Date("2019-12-31"), as.Date("2020-11-18"), by = "day")
inds

## Create a time series object for Covid daily, Lockdown data and test cases
set.seed(25)

covid.ts.daily <- ts(covid.data$total_cases,     
               start = c(2019, as.numeric(format(inds[1], "%j"))), end = c(2020, as.numeric(format(inds[length(inds)], "%j"))),
               frequency = 365)
covid.ts.daily

tests.ts <- ts(covid.data$total_tests,     
               start = c(2019, as.numeric(format(inds[1], "%j"))), end = c(2020, as.numeric(format(inds[length(inds)], "%j"))),
               frequency = 365)

lockdown.ts <- ts(covid.data$lockdown,     
                  start = c(2019, as.numeric(format(inds[1], "%j"))), end = c(2020, as.numeric(format(inds[length(inds)], "%j"))),
                  frequency = 365)

########## Creating monthly ts for travel - #of passengers, covid cases ###################

# Create data frame.
travel.data <- read.csv("travel.csv", header = TRUE, strip.white = TRUE)

forecast.data <- read.csv("forecastPeriod.csv", header = TRUE, strip.white = TRUE) # for covid based on ARIMA

forecast.data.dummy <- read.csv("forecastPerioddummy.csv", header = TRUE, strip.white = TRUE) # for covid based on ARIMA

travel.ts <- ts(travel.data$Total,     
                start = c(2000, 1), end = c(2020, 8),
                frequency = 12)

covid.ts <- ts(travel.data$Covid,     
               start = c(2000, 1), end = c(2020, 8),
               frequency = 12)

forecast.ts <- ts(forecast.data$forecast,     
                  start = c(2020, 9), end = c(2021, 3),
                  frequency = 12)

forecast.ts.dummy <- ts(forecast.data.dummy$forecast,     
                  start = c(2020, 9), end = c(2021, 3),
                  frequency = 12)

############### DATA VISUALIZATION #######################################################

# Plot time series without x-axis
plot(covid.ts.daily, ylab = "cases", xaxt = "n")

plot.ts(covid.ts.daily)

# Plot time series without x-axis (Monthly COvid aggregated data)
plot(covid.ts, ylab = "cases", xaxt = "n")

# Use stl() function to plot times series components of the original data. 
# The plot includes original data, trend, seasonal, and reminder 
# (level and noise component).
plot(travel.ts, ylab = "No. of Passengers", xaxt = "n")

travel.stl <- stl(travel.ts, s.window = "periodic")
autoplot(travel.stl, main = "Travel Time Series Components")

################ CHECK PREDICTABILITY OF COVID DAILY data and TRAVEL MONTHLY DATA ########

#covid.ar1<- Arima(covid.ts.daily, order = c(1,0,0))
#summary(covid.ar1)
autocor <- Acf(covid.ts.daily, lag = 365, main = "Autocorrelation for Covid daily data")
autocor <- Acf(covid.ts, lag = 12, main = "Autocorrelation for Covid monthly data")

travel.ar1<- Arima(travel.ts, order = c(1,0,0))
summary(travel.ar1) #since B1 is less than 1, travel data is not a random walk and hence is predictable

# Use Acf() function to identify autocorrelation and plot autocorrelation
# for different lags (up to maximum of 12).
autocor <- Acf(travel.ts, lag = 12, main = "Autocorrelation for Travel")
# Autocorrelation is present and hence travel data is predictable

# Display autocorrelation coefficients for various lags.
Lag <- round(autocor$lag, 0)
ACF <- round(autocor$acf, 3)
data.frame(Lag, ACF)

#############################################
##### TO FORECAST TRAVEL, WE WILL HAVE TO FORECASTING COVID FIRST BASED ON DAILY FREQUENCY ########
############### MODEL -1 FORECASTING COVID USING ARIMA  ###########################

#### ARIMA prediction-TRAINING & VALIDATION DATASET
###DATASET PARTITION
nValid <- 120
nTrain <- length(covid.ts.daily) - nValid
train.ts <- window(covid.ts.daily, start = c(2019, 365), end = c(2020, nTrain))
valid.ts <- window(covid.ts.daily, start = c(2019, nTrain + 1), 
                   end = c(2020, nTrain + nValid))
train.ts
valid.ts

# Use auto.arima() function to fit ARIMA model.
# Use summary() to show auto ARIMA model and its parameters.
train.auto.arima <- auto.arima(train.ts)
summary(train.auto.arima)

# Apply forecast() function to make predictions for ts with 
# auto ARIMA model in validation set.  
train.auto.arima.pred <- forecast(train.auto.arima, h = nValid, level = 0)
train.auto.arima.pred

# Plot ts data, trend and seasonality data, and predictions for validation period.
plot(train.auto.arima.pred, 
     xlab = "Time", ylab = "No. of COVID cases", ylim = c(0, 16000000), bty = "l",
     xaxt = "n", xlim = c(2019.9, 2021), 
     main = "Auto ARIMA Model for COVID", lwd = 2, flty = 5) 
axis(1, at = seq(2019.9, 2021, 0.1), labels = format(seq(2019.9, 2021, 0.1)))
lines(train.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(valid.ts, col = "black", lwd = 2, lty = 1)
lines(train.auto.arima$fitted, col = "blue", lwd = 2)
legend(x="topleft", legend = c("COVID Time Series", 
                             "Auto ARIMA Forecast for Training Period",
                             "Auto ARIMA Forecast for Validation Period"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n", cex=0.8, y.intersp=0.6)
lines(c(2020.6,2020.6 ), c(0, 12000000))
lines(c(2020.88, 2020.88), c(0, 12000000))
text(2020.2, 10000000, "Training")
text(2020.7, 10000000, "Validation")
arrows(2020, 9000000, 2020.6, 9000000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.6, 9000000, 2020.88, 9000000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

#### ARIMA prediction-FULL DATASET
## use auto.arima to choose ARIMA terms
arima_fit <- auto.arima(covid.ts.daily)
arima_fit
#(yt - yt -1) - (yt -1 - yt -2) = b0 + b1(yt-1 - yt-2) + b2(yt-2 - yt-3) + et  + q1 et-1 + q2 et-2

## forecast for next 120 time points
covid_arima_forecast <- forecast(arima_fit, h = 120)
## plot the arima forecast
plot(covid_arima_forecast)

# Use Acf() function to create autocorrelation chart of auto ARIMA 
# model residuals.
#Acf(arima_fit$residuals, lag.max = 30, 
#    main = "Autocorrelations of Auto ARIMA Model Residuals - Entire Data")

plot(train.auto.arima.pred, 
     xlab = "Time", ylab = "No. of COVID cases", ylim = c(0, 16000000), bty = "l",
     xaxt = "n", xlim = c(2019.9, 2021), 
     main = "Auto ARIMA Model for COVID", lwd = 2, flty = 5) 
axis(1, at = seq(2019.9, 2021, 0.1), labels = format(seq(2019.9, 2021, 0.1)))
lines(train.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(covid_arima_forecast$mean, col = "brown", lty = 5, lwd = 2)
lines(valid.ts, col = "black", lwd = 2, lty = 1)
legend(x="topleft", legend = c("COVID Time Series", 
                               "Auto ARIMA Forecast for Training",
                               "Auto ARIMA Forecast for Validation",
                                "Auto ARIMA Forecast for 120 days into the future"), 
       col = c("black", "blue" , "blue","brown"), 
       lty = c(1, 1, 5,5), lwd =c(2, 2, 2), bty = "n",  cex=0.8, y.intersp=0.6)
lines(c(2020.6,2020.6 ), c(0, 12000000))
lines(c(2020.88, 2020.88), c(0, 12000000))
text(2020.2, 11000000, "Training")
text(2020.7, 11000000, "Validation")
text(2020.95,11000000,"Future")
arrows(2020, 10000000, 2020.6, 10000000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.6, 10000000, 2020.88, 10000000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.88, 10000000, 2021, 10000000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

round(accuracy(covid_arima_forecast$fitted, covid.ts.daily), 3)

#               ME     RMSE      MAE   MPE MAPE   ACF1 Theil's U
#Test set 706.011 6330.918 3893.811 1.077 3.19 -0.014         0

############### MODEL-2- FORECASTING COVID USING Quadratic trend with external variables i.e. number of tests ###########################

# Use tslm() function to create quadratic trend with 1 external variables.
covid.quad.external_1 <- tslm(covid.ts.daily ~ trend + I(trend^2)
                              + tests.ts)

# See summary of the model with linear trend and external variables.
summary(covid.quad.external_1)

test_ma.trailing_7 <- rollmean(tests.ts, k = 7, align = "right")

## Create trailing MA forecast with window width of 7 for future 120 periods of 2020.
test_ma.trailing_7.pred <- forecast(test_ma.trailing_7, h=120, level = 0)
test_ma.trailing_7.pred$mean

# To forecast 4 quarters of 2019, develop the values of the variables for those periods.
forecast_param <- data.frame(trend = c(325:444), tests.ts = test_ma.trailing_7.pred$mean)

# Apply forecast() function to make predictions for ts with 
# trend and external variables in 4 quarters of 2019.  
covid.external.pred <- forecast(covid.quad.external_1, newdata = forecast_param, level = 0)
covid.external.pred 

plot(covid.external.pred, 
     xlab = "Time", ylab = "No. of COVID cases", bty = "l",
     xlim = c(2019.80, 2021.25),ylim = c(0, 16000000), main = "Quadratic Trend with external variables for COVID complete data", flty = 2) 
    axis(1, at = seq(2019.80, 2021.25, 0.1), labels = format(seq(2019.80, 2021.25, 0.1)) )
lines(covid.external.pred$fitted, col = "blue", lwd = 2)
lines(covid.ts.daily, col = "black", lty = 1)
legend(x="topleft", legend = c("COVID Time Series", 
                               "Quadratic Trend for complete COVID Data", 
                               "Forecast trend for complete COVID Data"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 3), lwd =c(2, 2, 2), bty = "n",  cex=0.8, y.intersp=0.6)
lines(c(2020,2020), c(0, 10000000))
lines(c(2020.88, 2020.88), c(0, 12000000))
text(2020.4, 10000000, "Training-Entire Data")
text(2021, 10000000, "Future")
arrows(2020, 9000000, 2020.88, 9000000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
#arrows(2020.56, 9000000, 2020.88, 9000000, code = 3, length = 0.1,
#lwd = 1, angle = 30)
arrows(2020.88, 9000000, 2021.15, 9000000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

#USED this in MODEL but it gives Inf mape and rest all other error measures are better in MA 7 
round(accuracy(covid.external.pred$fitted, covid.ts.daily),3)
#ME     RMSE      MAE MPE MAPE  ACF1 Theil's U
#Test set  0 216300.8 173487.6 Inf  Inf 0.988       NaN

############### MODEL-3 FORECASTING COVID USING MOVING AVERAGES ###########################
##TRAILING MA USING FULL DATASET
# Create trailing moving average with window (number of periods) k = 60, 7, and 30 days.
# In rollmean(), use argument align = "right" to calculate a trailing MA.
covid_ma.trailing_60 <- rollmean(covid.ts.daily, k = 60, align = "right")
covid_ma.trailing_7 <- rollmean(covid.ts.daily, k = 7, align = "right")
covid_ma.trailing_30 <- rollmean(covid.ts.daily, k = 30, align = "right")

# Use head() function to show the first 6 MA results 
# and tail() function to show the last 6 MA results.
head(covid_ma.trailing_30)
tail(covid_ma.trailing_30)

# Combine covid.ts and ma.trailing in one data table.

covid_ma.trail_60 <- c(rep(NA, length(covid.ts.daily) - length(covid_ma.trailing_60)), covid_ma.trailing_60)
covid_ma.trail_7 <- c(rep(NA, length(covid.ts.daily) - length(covid_ma.trailing_7)), covid_ma.trailing_7)
covid_ma.trail_30 <- c(rep(NA, length(covid.ts.daily) - length(covid_ma.trailing_30)), covid_ma.trailing_30)

covid_ma_trailing_tab <- cbind(covid.ts.daily, covid_ma.trail_7, covid_ma.trail_30, covid_ma.trail_60)
covid_ma_trailing_tab

#Combine covid.ts and ma.trailing in one plot.
plot(covid.ts.daily, 
     xlab = "Time", ylab = "COVID cases (in 000s)", bty = "l",
     xlim = c(2019.8, 2021.25), main = "COVID and moving averages with different windows", 
     col = "black", lwd =1) 
axis(1, at = seq(2019.8, 2021.25, 0.1), labels = format(seq(2019.8, 2021.25, 0.1)) )
lines(covid_ma.trailing_60, col = "green", lwd = 2)
lines(covid_ma.trailing_7, col = "orange", lwd = 2)
lines(covid_ma.trailing_30, col = "blue", lwd = 2)
legend(x= "topleft",legend = c("COVID", "Trailing MA, k=60", "Trailing MA, k=7", 
                               "Trailing MA, k=30"), 
       col = c("black", "green", "orange", "blue",  "green", "orange", "blue"), 
       lty = c(1,1,1), lwd =c(2), bty = "n",
       pch=c(".",".", ".", ".","."), merge=TRUE, cex=0.8, y.intersp=0.6)
lines(c(2020,2020), c(0, 10000000))
lines(c(2020.88, 2020.88), c(0, 12000000))
text(2020.4, 10000000, "Training-Entire Data")
text(2021, 10000000, "Future")
arrows(2020, 9000000, 2020.88, 9000000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
#arrows(2020.56, 9000000, 2020.88, 9000000, code = 3, length = 0.1,
#lwd = 1, angle = 30)
arrows(2020.88, 9000000, 2021.15, 9000000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

## Create trailing MA forecast with window width of 60 for future 120 periods of 2020.
covid_ma.trailing_60.pred <- forecast(covid_ma.trailing_60, h=120, level = 0)
covid_ma.trailing_60.pred

## Create trailing MA forecast with window width of 7 for future 120 periods of 2020.
covid_ma.trailing_7.pred <- forecast(covid_ma.trailing_7, h=120, level = 0)
covid_ma.trailing_7.pred

## Create trailing MA forecast with window width of 30 for future 120 periods of 2020.
covid_ma.trailing_30.pred <- forecast(covid_ma.trailing_30, h=120, level = 0)
covid_ma.trailing_30.pred

plot(covid.ts.daily, 
     xlab = "Years", ylab = "COVID cases", bty = "l",
     xlim = c(2019.8, 2021.25), main = "Trailing Moving Average Forecast for COVID data") 
axis(0.6, at = seq(2019.8, 2021.25, .1), labels = format(seq(2019.8, 2021.25, .1)))
#lines(ma.trailing_60, col = "green", lwd = 2, lty = 1)
lines(covid_ma.trailing_7, col = "orange", lwd = 2, lty = 1)
lines(covid_ma.trailing_30, col = "blue", lwd = 2, lty = 1)
lines(covid_ma.trailing_60, col="green", lwd=2, lty=1)
lines(covid_ma.trailing_60.pred$mean, col = "green", lwd = 2, lty = 5)
lines(covid_ma.trailing_7.pred$mean, col = "orange", lwd = 2, lty = 5)
lines(covid_ma.trailing_30.pred$mean, col = "blue", lwd = 2, lty = 5)
legend(x= "topleft",legend = c("COVID original data", "Trailing MA, k=7", "MA Forecast, k=7", "Trailing MA, k=30", 
                               "MA Forecast, k=30", "Trailing MA, k=60", "MA Forecast, k=60"), 
       col = c("black", "orange", "orange", "blue", "blue", "green",  "green"), 
       lty = c(1,1,5,1,5,1), lwd =c(2), bty = "n",
       pch=c(".",".", ".", ".","."), merge=TRUE, cex=0.8, y.intersp=0.6)
lines(c(2020,2020), c(0, 10000000))
lines(c(2020.88, 2020.88), c(0, 12000000))
text(2020.4, 10000000, "Training-Entire Data")
text(2021, 10000000, "Future")
arrows(2020, 9000000, 2020.88, 9000000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
#arrows(2020.56, 9000000, 2020.88, 9000000, code = 3, length = 0.1,
#lwd = 1, angle = 30)
arrows(2020.88, 9000000, 2021.15, 9000000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

round(accuracy(covid_ma.trail_60, covid.ts.daily), 3)
round(accuracy(covid_ma.trail_7, covid.ts.daily), 3)
round(accuracy(covid_ma.trail_30, covid.ts.daily), 3)

#> round(accuracy(covid_ma.trail_7, covid.ts), 3)
#ME     RMSE      MAE    MPE   MAPE  ACF1 Theil's U
#Test set 104709.4 140373.2 104709.4 12.315 12.315 0.971         0

############### Combined plot for all models #########################################

plot(covid.ts.daily, 
     xlab = "Years", ylab = "COVID cases", bty = "l",
     xlim = c(2019.8, 2021.25), ylim=c(0, 16000000), main = "Forecast for COVID data - All Models") 
axis(1, at = seq(2019.8, 2021.25, .1), labels = format(seq(2019.8, 2021.25, .1)))
#lines(ma.trailing_60, col = "green", lwd = 2, lty = 1)
lines(covid_arima_forecast$fitted, col = "green", lwd = 2)
lines(covid_arima_forecast$mean, col = "green", lty = 5, lwd = 2)
lines(covid.external.pred$fitted, col = "blue", lwd = 2)
lines(covid.external.pred$mean, col = "blue", lty = 1)
lines(covid_ma.trailing_7, col = "orange", lwd = 2, lty = 1)
lines(covid_ma.trailing_7.pred$mean, col = "orange", lwd = 2, lty = 5)
legend(x= "topleft",legend = c("COVID original data", "ARIMA", "ARIMA Forecast", "Two level Regression + MA Residual", 
                               "Two level Forecast", "Trailing MA, k=7", "MA Forecast, k=7"), 
       col = c("black", "green", "green", "blue", "blue", "orange",  "orange"), 
       lty = c(1,1,5,1,5,1,5), lwd =c(2), bty = "n",
       pch=c(".",".", ".", ".","."), merge=TRUE, cex=0.8, y.intersp=0.6)
lines(c(2020,2020), c(0, 10000000))
lines(c(2020.88, 2020.88), c(0, 12000000))
text(2020.4, 10000000, "Training-Entire Data")
text(2021, 10000000, "Future")
arrows(2020, 9000000, 2020.88, 9000000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
#arrows(2020.56, 9000000, 2020.88, 9000000, code = 3, length = 0.1,
#lwd = 1, angle = 30)
arrows(2020.88, 9000000, 2021.15, 9000000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

############### Combined plot for two best models (ARIMA, MA) #########################################

plot(covid.ts.daily, 
     xlab = "Years", ylab = "COVID cases", bty = "l",
     xlim = c(2019.8, 2021.25), ylim=c(0, 16000000), main = "Forecast for COVID data - Top Two Models") 
axis(1, at = seq(2019.8, 2021.25, .1), labels = format(seq(2019.8, 2021.25, .1)))
#lines(ma.trailing_60, col = "green", lwd = 2, lty = 1)
lines(covid_arima_forecast$fitted, col = "green", lwd = 2)
lines(covid_arima_forecast$mean, col = "green", lty = 5, lwd = 2)
lines(covid_ma.trailing_7, col = "orange", lwd = 2, lty = 1)
lines(covid_ma.trailing_7.pred$mean, col = "orange", lwd = 2, lty = 5)
legend(x= "topleft",legend = c("COVID original data", "ARIMA", "ARIMA Forecast",  
                               "Trailing MA, k=7", "MA Forecast, k=7"), 
       col = c("black", "green", "green", "orange",  "orange"), 
       lty = c(1,1,5,1,5), lwd =c(2), bty = "n",
       pch=c(".",".", ".", ".","."), merge=TRUE, cex=0.8, y.intersp=0.6)
lines(c(2020,2020), c(0, 10000000))
lines(c(2020.88, 2020.88), c(0, 12000000))
text(2020.4, 10000000, "Training-Entire Data")
text(2021, 10000000, "Future")
arrows(2020, 9000000, 2020.88, 9000000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
#arrows(2020.56, 9000000, 2020.88, 9000000, code = 3, length = 0.1,
#lwd = 1, angle = 30)
arrows(2020.88, 9000000, 2021.15, 9000000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

#######################################################################################
####################### USING FORECASTED COVID cases to FORECAST TRAVEL ###############
###### used Covid.ts which is monthly aggregated data for Covid #######################

# Create fixed data partitioning for data.
# Define the numbers of months in the training and validation sets,
# nTrain and nValid, respectively.
nValid <- 20
nTrain <- length(travel.ts) - nValid
train.ts <- window(travel.ts, start = c(2000, 1), end = c(2000, nTrain))
valid.ts <- window(travel.ts, start = c(2000, nTrain + 1), 
                   end = c(2000, nTrain + nValid))


############### MODEL-1 FORECASTING TRAVEL USING Moving Average  ###########################

# Create trailing moving average with window (number of periods) k = 2, 6, and 12.
# In rollmean(), use argument align = "right" to calculate a trailing MA.
travel_ma.trailing_2 <- rollmean(travel.ts, k = 2, align = "right")
travel_ma.trailing_6 <- rollmean(travel.ts, k = 6, align = "right")
travel_ma.trailing_12 <- rollmean(travel.ts, k = 12, align = "right")


# Combine travel.ts and ma.trailing in one data table.

travel_ma.trail_2 <- c(rep(NA, length(travel.ts) - length(travel_ma.trailing_2)), travel_ma.trailing_2)
travel_ma.trail_6 <- c(rep(NA, length(travel.ts) - length(travel_ma.trailing_6)), travel_ma.trailing_6)
travel_ma.trail_12 <- c(rep(NA, length(travel.ts) - length(travel_ma.trailing_12)), travel_ma.trailing_12)

ma_trailing_tab <- cbind(travel.ts, travel_ma.trail_2, travel_ma.trail_6, travel_ma.trail_12)
ma_trailing_tab

#Combine travel.ts and ma.trailing in one plot.
plot(travel.ts, 
     xlab = "Time", ylab = "Passenger Enplanements (in 000s)", bty = "l",
     xlim = c(2000, 2021.25), main = "Trailing Moving Average for Travel data", 
     col = "black", lwd =1) 
axis(1, at = seq(2000, 2021.25, 1), labels = format(seq(2000, 2021.25, 1)) )
lines(travel_ma.trailing_2, col = "green", lwd = 2)
lines(travel_ma.trailing_6, col = "orange", lwd = 2)
lines(travel_ma.trailing_12, col = "blue", lwd = 2)
legend(x= "topleft",legend = c("Passenger Enplanements", "Trailing MA, k=2", "Trailing MA, k=6", 
                               "Trailing MA, k=12"), 
       col = c("black", "green", "orange", "blue",  "green", "orange", "blue"), 
       lty = c(1,1,1), lwd =c(2), bty = "n",
       pch=c(".",".", ".", ".","."), merge=TRUE, cex=0.8, y.intersp=0.6)


## Create trailing MA forecast with window width of 2 for future 12 periods of 2020.
travel_ma.trailing_2.pred <- forecast(travel_ma.trailing_2, h=7, level = 0)
travel_ma.trailing_2.pred

## Create trailing MA forecast with window width of 6 for future 12 periods of 2020.
travel_ma.trailing_6.pred <- forecast(travel_ma.trailing_6, h=7, level = 0)
travel_ma.trailing_6.pred

## Create trailing MA forecast with window width of 12 for future 12 periods of 2020.
travel_ma.trailing_12.pred <- forecast(travel_ma.trailing_12, h=7, level = 0)
travel_ma.trailing_12.pred

plot(travel.ts, 
     xlab = "Years", ylab = "Enplanements", bty = "l",
     xlim = c(2000, 2021.25), main = "Trailing Moving Average Forecast for enplanements") 
axis(1, at = seq(2000, 2021.25, 3), labels = format(seq(2000, 2021.25, 1)))
lines(travel_ma.trailing_2, col = "green", lwd = 2, lty = 1)
lines(travel_ma.trailing_6, col = "orange", lwd = 2, lty = 1)
lines(travel_ma.trailing_12, col = "blue", lwd = 2, lty = 1)
lines(travel_ma.trailing_2.pred$mean, col = "green", lwd = 2, lty = 5)
lines(travel_ma.trailing_6.pred$mean, col = "orange", lwd = 2, lty = 5)
lines(travel_ma.trailing_12.pred$mean, col = "blue", lwd = 2, lty = 5)
legend(2002, 40000,legend = c("Passenger Enplanements", "Trailing MA, k=2", "Trailing MA, k=6", 
                              "Trailing MA, k=12", "MA Forecast, k=2", "MA Forecast, k=6", 
                              "MA Forecast, k=12"), 
       col = c("black", "green", "orange", "blue",  "green", "orange", "blue"), 
       lty = c(1,1,1,1,5,5,5), lwd =c(2), bty = "n",
       pch=c(".",".", ".", ".","."), merge=TRUE, cex=0.8, y.intersp=0.6)

#Accuracy for Moving average for k=2
round(accuracy(travel_ma.trail_2, travel.ts), 3)
#Accuracy for Moving average for k=6
round(accuracy(travel_ma.trail_6, travel.ts), 3)
#Accuracy for Moving average for k=12
round(accuracy(travel_ma.trail_12, travel.ts), 3)

#@@@@@@@@@@@@@@@ Make partition split model on this one @@@@@@@@@@@@@@@@@@###########
########## MODEL-2 FORECASTING TRAVEL USING Two-level model - Linear Trend+ Season+Covid  ###########################

## DE-TRENDING and DE-SEASONALIZING TIME SERIES USING REGRESSION
## CREATE TRAILING MA USING RESIDUALS.
## FORECAST USING REGRESSION AND TRAILING MA INTO FUTURE PERIODS.

# Fit a regression model with linear trend.
reg.trend.seas <- tslm(travel.ts ~ trend + season + covid.ts)
summary(reg.trend.seas) #Adjusted R-squared:  0.6796 

forecast_param <- data.frame(trend = c(249:255), covid.ts = forecast.ts)
class(forecast.data$forecast)

# Create forecast for the 12 periods into the future.
reg.trend.seas.pred <- forecast(reg.trend.seas, newdata = forecast_param, h = 7, level = 0)
reg.trend.seas.pred

plot(travel.ts, 
     xlab = "Years", ylab = "covid", bty = "l",
     xlim = c(2000, 2021.25), lwd=2,
     main = "Regression Model and Forecast") 
axis(1, at = seq(2000, 2021.25, 1), labels = format(seq(2000, 2021.25, 1)))
lines(reg.trend.seas$fitted, col = "brown", lwd = 2)
lines(reg.trend.seas.pred$mean, col = "brown", lty =5, lwd = 2)
legend(x= "topright", legend = c("COVID", "Regression", "Regression Forecast"), 
       col = c("black", "brown", "brown"), 
       lty = c(1,1,5), lwd =c(2), bty = "n",        
       pch=c(".",".", ".", ".","."), merge=TRUE, cex=0.8, y.intersp=0.6)

reg.trend.seas.res <- reg.trend.seas$residuals
reg.trend.seas.res

#trend exists in residuals post regression, so we should include it in the forecast
autocor <- Acf(reg.trend.seas.res, lag = 12, main = "Autocorrelation for Travel data post regression")

# Apply trailing MA with 2 periods in the window to residuals.
ma.trailing.res_2 <- rollmean(reg.trend.seas.res, k = 2, align = "right")
ma.trailing.res_2

# Create forecast for residuals for the 12 periods into the future.
ma.trailing.res_2.pred <- forecast(ma.trailing.res_2, h = 7, level = 0)
ma.trailing.res_2.pred

# To develop real forecast for 12 periods into the future, 
# combine regression forecast and trailing MA forecast for residuals.
ts.forecast.2 <- reg.trend.seas.pred$mean + ma.trailing.res_2.pred$mean
ts.forecast.2

# Create a table with regression forecast, trailing MA residual's forecast, and 
# total forecast for 12 months into the future.
total.reg.ma.pred <- data.frame(reg.trend.seas.pred$mean, ma.trailing.res_2.pred$mean, 
                                ts.forecast.2)
total.reg.ma.pred

#check y label and title
plot(travel.ts, 
     xlab = "Years", ylab = "Passenger enplanements(in 000s)",bty = "l",
     xlim = c(2000, 2021.25), lwd=2,
     main = "Residuals, Trailing MA of residuals and Residual Forecast") 
axis(1, at = seq(2000, 2021.25, 1), labels = format(seq(2000, 2021.25, 1)))
#lines(reg.trend.seas.res, col = "black", lty =1, lwd = 1)
lines(reg.trend.seas$fitted, col = "brown", lwd = 2)
lines(reg.trend.seas.pred$mean, col = "brown", lty =5, lwd = 2)
lines(ts.forecast.2, col = "blue", lty =1, lwd = 2)
#lines(ma.trailing.res_2.pred$mean, col = "blue", lty =5, lwd = 2)
legend(x= "topleft", legend = c("Regression Entire", "Regression Forecast", "Two level Forecast Regression+MA Residuals"), 
       col = c("black","blue","blue"), 
       lty = c(1,1,5), lwd =c(2), bty = "n",        
       pch=c(".",".", ".", ".","."), merge=TRUE, cex=0.8, y.intersp=0.6)

round(accuracy(reg.trend.seas.pred$fitted, travel.ts), 3)
round(accuracy(reg.trend.seas.pred$fitted+ma.trailing.res_2, travel.ts), 3)
round(accuracy(snaive(travel.ts)$fitted, travel.ts), 3)

################# MODEL-3 HW ZZZ for TRAVEL ##################################

## HOLT-WINTER'S (HW) EXPONENTIAL SMOOTHING WITH PARTITIONED DATA, AUTOMATED
## ERROR, TREND and SEASONALITY (ZZZ) OPTIONS, AND OPTIMAL PARAMETERS
## ALPHA, BETA, AND GAMMA.

# Create Holt-Winter's (HW) exponential smoothing for partitioned data.
# Use ets() function with model = "ZZZ", i.e., automated selection 
# error, trend, and seasonality options.
# Use optimal alpha, beta, & gamma to fit HW over the training period.
hw.ZZZ <- ets(train.ts, model = "ZZZ")
hw.ZZZ # Model appears to be ETS(A,N,A), with   alpha = 0.5665 and gamma = 0.0001

# Use forecast() function to make predictions using this HW model with 
# validation period (nValid). 
# Show predictions in tabular format.
hw.ZZZ.pred <- forecast(hw.ZZZ, h = nValid, level = 0)
hw.ZZZ.pred

#tested it on full data
hw.ZZZ.full <- ets(travel.ts, model = "ZZZ")
hw.ZZZ.full

# Use forecast() function to make predictions using this HW model for 
# validation period (nValid). 
# Show predictions in tabular format.
hw.ZZZ.full.pred <- forecast(hw.ZZZ.full, h = 7, level = 0)
hw.ZZZ.full.pred

#visualizing ZZZ and MMM model prediction outputs on the graph
plot(travel.ts, 
     xlab = "Year", ylab = "Passenger enplanements(in 000s)", bty = "l",
     xaxt = "n", xlim = c(2000, 2021.25), 
     main = "Holt-Winter's ZZZ models with Optimal Smoothing Parameters", flty = 2) 
axis(1, at = seq(2000, 2021.25, 1), labels = format(seq(2000, 2021.25, 1)))
lines(hw.ZZZ$fitted, col = "orange", lwd = 2)
lines(hw.ZZZ.pred$mean, col = "blue", lty =5, lwd = 2)
legend(x= "bottomleft",legend = c( "Travel-Passenger enplanements",
                                   "Holt Winter's ZZZ", "ZZZ Forecast"), 
       col = c("black","orange", "blue"), 
       lty = c(1,1,5,1,5), lwd =c(2), bty = "n",
       pch=c(".",".", ".", ".","."), merge=TRUE, cex=0.8, y.intersp=0.6)
lines(c(2019,2019 ), c(0,80000))
lines(c(2020.7, 2020.7), c(0, 80000))
text(2009, 85000, "Training")
text(2020.5, 85000, "Validation")
arrows(2000, 80000, 2019, 80000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019, 80000, 2020.7, 80000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

# Use accuracy() function to identify common accuracy measures.
# Use round() function to round accuracy measures to three decimal digits.
round(accuracy(snaive(travel.ts)$fitted, travel.ts), 3)

round(accuracy(hw.ZZZ.pred$fitted, train.ts), 3)

round(accuracy(hw.ZZZ.pred$mean, valid.ts), 3)

round(accuracy(hw.ZZZ.full.pred$fitted, travel.ts), 3)

plot(travel.ts, 
     xlab = "Time", ylab = "Passenger enplanements)", bty = "l",
     xaxt = "n", xlim = c(2000, 2021.50), 
     main = "Holt-Winter's ZZZ(A,N,A) model with Optimal Smoothing Parameters", flty = 2) 
axis(1, at = seq(2000, 2021.50, 1), labels = format(seq(2000, 2021.50, 1)))
#lines(hw.MMM.full.pred$fitted, col = "blue", lwd = 2)
#lines(hw.MMM.full.pred$mean, col = "blue", lty =5, lwd = 2)
lines(hw.ZZZ.full.pred$fitted, col = "orange", lwd = 2)
lines(hw.ZZZ.full.pred$mean, col = "blue", lty =5, lwd = 2)
legend(x= "bottomleft",legend = c("Enplanements data","Holt Winter's ZZZ", "ZZZ Forecast"), 
       col = c("black", "orange", "blue"), 
       lty = c(1,1,5,1,5), lwd =c(2), bty = "n",
       pch=c(".",".", ".", ".","."), merge=TRUE, cex=0.8, y.intersp=0.6)
lines(c(2020.6, 2020.6), c(0, 80000))
text(2009, 85000, "Training-Entire Data")
text(2021.3, 85000, "Future")
arrows(2000, 80000, 2020.6, 80000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.6, 80000, 2021.5, 80000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

################# MODEL-4 HW + MA for residuals -  TRAVEL ##################################
#HW model for the full dataset
hw_full.ZZZ <- ets(travel.ts, model = "ZZZ")
hw_full.ZZZ 

#HW forecast for full dataset
hw_full.ZZZ.pred <- forecast(hw_full.ZZZ, h = 7, level = 0)
hw_full.ZZZ.pred

#get the residuals
hw_full.ZZZ.res <- hw_full.ZZZ$residuals
hw_full.ZZZ.res

#check autocorrelation in residuals after HW model
autocor <- Acf(hw_full.ZZZ$residuals, lag = 12, main = "Autocorrelation for Travel data post regression")
#There is still trend present in residuals (refer to lag 1), we should include it in our forecast

# Plot residuals of the predictions
plot(hw_full.ZZZ.res, 
     xlab = "Time", ylab = "Residuals", bty = "l",
     xaxt = "n", xlim = c(2000, 2021), 
     main = "HW Residuals for full Data before MA", 
     col = "blue", lwd = 3) 
axis(1, at = seq(2000,2021, 1), labels = format(seq(2000, 2021, 1)))

# Apply trailing MA with 2 periods in the window to residuals.
HWres_ma.trailing.res_2 <- rollmean(hw_full.ZZZ.res, k = 2, align = "right")
HWres_ma.trailing.res_2

# Create forecast for residuals for the 12 periods into the future.
HWres_ma.trailing.res_2.pred <- forecast(HWres_ma.trailing.res_2, h = 7, level = 0)
HWres_ma.trailing.res_2.pred

# To develop real forecast for 12 periods into the future, 
# combine HW forecast and trailing MA forecast for residuals.
HWMA_ts.forecast.2 <- hw.ZZZ.full.pred$mean + HWres_ma.trailing.res_2.pred$mean
HWMA_ts.forecast.2

# Create a table with regression forecast, trailing MA residual's forecast, and 
# total forecast for 12 months into the future.
total.HWMA.pred <- data.frame(hw.ZZZ.full.pred$mean, HWres_ma.trailing.res_2.pred$mean, 
                              HWMA_ts.forecast.2)
total.HWMA.pred

plot(travel.ts, 
     xlab = "Years", ylab = "Passenger enplanements",bty = "l",
     xlim = c(2000, 2021.25), lwd=2,
     main = "HW-ZZZ Forecast and HW+Moving Average Two-level Forecast") 
axis(1, at = seq(2000, 2021.25, 1), labels = format(seq(2000, 2021.25, 1)))
#lines(reg.trend.seas.res, col = "black", lty =1, lwd = 1)
lines(hw_full.ZZZ.pred$fitted, col = "orange", lwd = 2)
#lines(hw_full.ZZZ.res, col = "brown", lwd = 2)
#lines(HWres_ma.trailing.res_2.pred$fitted, col = "orange", lwd = 2)
lines(hw_full.ZZZ.pred$mean, col = "blue", lty =5, lwd = 2)
lines(HWMA_ts.forecast.2, col = "green", lty =5, lwd = 2)
#lines(HWMA_ts.forecast.2, col = "blue", lty =1, lwd = 2)
legend(x= "bottomleft", legend = c("Travel Passenger Enplanements","Holt Winter's ZZZ - Entire Data", 
                                  "HW forecast for future", "HW+MA Forecast for future"), 
       col = c("black","orange","blue","green"), 
       lty = c(1,1,5,5,5), lwd =c(2), bty = "n",        
       pch=c(".",".", ".", ".","."), merge=TRUE, cex=0.8, y.intersp=0.6)
lines(c(2020.5,2020.5 ), c(0,80000))
lines(c(2021.5, 2021.5), c(0, 80000))
text(2009, 85000, "Training")
text(2021.5, 85000, "Future")
arrows(2000, 80000, 2020.5, 80000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.5, 80000, 2021.5, 80000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

# Plot residuals of the predictions
plot(HWres_ma.trailing.res_2, 
     xlab = "Time", ylab = "Residuals", bty = "l",
     xaxt = "n", xlim = c(2000, 2021), 
     main = "HW Residuals for full Data after MA", 
     col = "blue", lwd = 3) 
axis(1, at = seq(2000,2021, 1), labels = format(seq(2000, 2021, 1)))

round(accuracy(hw_full.ZZZ.pred$fitted+HWres_ma.trailing.res_2.pred$fitted, travel.ts), 3)

################# MODEL-5 HW + AR(1) for residuals -  TRAVEL ##################################

#Develop HW model to capture trend and seasonality
# Use optimal alpha, beta, & gamma to fit HW over the training period.
hw_train.ZZZ <- ets(train.ts, model = "ZZZ")
hw_train.ZZZ

# Use forecast() function to make predictions using this HW model for 
# validation period (nValid). 
# Show predictions in tabular format.
hw.ZZZ.valid.pred <- forecast(hw_train.ZZZ, h =nValid , level = 0)
hw.ZZZ.valid.pred

plot(travel.ts, 
     xlab = "Year", ylab = "Passenger enplanements", bty = "l",
     xaxt = "n", xlim = c(2000, 2021.25), 
     main = "Holt-Winter's ZZZ models with Optimal Smoothing Parameters", flty = 2) 
axis(1, at = seq(2000, 2021.25, 1), labels = format(seq(2000, 2021.25, 1)))
lines(hw.ZZZ.valid.pred$fitted, col = "orange", lwd = 2)
lines(hw.ZZZ.valid.pred$mean, col = "blue", lty =5, lwd = 2)
legend(x= "bottomleft",legend = c( "Travel-Passenger enplanements",
                                   "Holt Winter's ZZZ", "ZZZ Forecast"), 
       col = c("black","orange", "blue"), 
       lty = c(1,1,5,1,5), lwd =c(2), bty = "n",
       pch=c(".",".", ".", ".","."), merge=TRUE, cex=0.8, y.intersp=0.6)
lines(c(2019,2019 ), c(0,80000))
lines(c(2020.7, 2020.7), c(0, 80000))
text(2009, 85000, "Training")
text(2020.5, 85000, "Validation")
arrows(2000, 80000, 2019, 80000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019, 80000, 2020.7, 80000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

# Plot residuals of the HW model predictions.
plot(hw.ZZZ.valid.pred$residuals, 
     xlab = "Time", ylab = "Residuals", ylim = c(-20000, 6000), bty = "l",
     xaxt = "n", xlim = c(2000, 2021.25), 
     main = "Regresssion Residuals for Training and Validation Data", 
     col = "brown", lwd = 2) 
axis(1, at = seq(2000, 2021.25, 1), labels = format(seq(2000, 2021.25, 1)))
lines(valid.ts - hw.ZZZ.valid.pred$mean, col = "blue", lwd = 2, lty = 1)

lines(c(2019,2019 ), c(-20000,5000))
lines(c(2020.7, 2020.7), c(-20000, 5000))
text(2009, 4000, "Training")
text(2020.5, 4000, "Validation")
arrows(2000, 4000, 2019, 4000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019, 4000, 2020.7, 4000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

# Use Acf() function to identify autocorrelation for the model residuals 
# (training and validation sets), and plot autocorrelation for different 
# lags (up to maximum of 12).
Acf(hw.ZZZ.valid.pred$residuals, lag.max = 12, 
    main = "Autocorrelation for Passenger Enplanments Training Residuals")
Acf(valid.ts - hw.ZZZ.valid.pred$mean, lag.max = 12, 
    main = "Autocorrelation for Passenger Enplanments Validation Residuals")

# Use Arima() function to fit AR(1) model for training residuals. The Arima model of 
# order = c(1,0,0) gives an AR(1) model.
# Use summary() to identify parameters of AR(1) model. 
res.ar1 <- Arima(hw.ZZZ.valid.pred$residuals, order = c(1,0,0))
summary(res.ar1)
res.ar1$fitted

# Use forecast() function to make prediction of residuals in validation set.
res.ar1.pred <- forecast(res.ar1, h = nValid, level = 0)
res.ar1.pred

# Develop a data frame to demonstrate the training AR model results 
# vs. original training series, training regression model, 
# and its residuals.  
train.df <- data.frame(train.ts, hw_train.ZZZ$fitted, 
                       hw.ZZZ.valid.pred$residuals, res.ar1$fitted, res.ar1$residuals)
names(train.df) <- c("Passenger Enplanments", "HW model", "HW Residuals",
                     "AR.Model", "AR.Model.Residuals")
train.df

# Plot residuals of the predictions for training data before AR(1).
plot(hw.ZZZ.valid.pred$residuals, 
     xlab = "Time", ylab = "Residuals", ylim = c(-20000, 6000), bty = "l",
     xaxt = "n", xlim = c(2000, 2021.25), 
     main = "HW Residuals for Training Data before AR(1)", 
     col = "brown", lwd = 3) 
axis(1, at = seq(2000, 2021.25, 1), labels = format(seq(2000, 2021.25, 1)))

# Plot residuals of the residuals for training data after AR(1).
plot(res.ar1$residuals, 
     xlab = "Time", ylab = "Residuals", ylim = c(-20000, 6000), bty = "l",
     xaxt = "n", xlim = c(2000, 2021.25), 
     main = "HW Residuals for Training Data after AR(1)", 
     col = "brown", lwd = 3) 
axis(1, at = seq(2000, 2021.25, 1), labels = format(seq(2000, 2021.25, 1)))

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2000, 2000), c(-20000, 6000))
lines(c(2019, 2019), c(-20000, 6000))
text(2010, 4000, "Training")
arrows(2000, 4000, 2019, 4000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

# Use Acf() function to identify autocorrelation for the training 
# residual of residuals and plot autocorrelation for different lags 
# (up to maximum of 12).
Acf(res.ar1$residuals, lag.max = 12, 
    main = "Autocorrelation for Passenger Enplanements Training Residuals of Residuals")

#combined forecast
valid.two.level.pred <- hw.ZZZ.valid.pred$mean + res.ar1.pred$mean

valid.df <- data.frame(valid.ts, hw.ZZZ.valid.pred$mean, 
                       res.ar1.pred$mean, valid.two.level.pred)
names(valid.df) <- c("Passenger Enplanements", "HW Forecast", 
                     "AR(1)Forecast", "Combined.Forecast")
valid.df

## FIT HOLT-WINTER MODEL FOR ENTIRE DATASET
##FORECAST AND PLOT DATA, AND MEASURE ACCURACY.

#develop HW model using ets()
HW_AR.full <- ets(travel.ts, model = "ZZZ")

# See summary of linear trend equation and associated parameters.
summary(HW_AR.full)

# Apply forecast() function to make predictions with HW model into the future  
HW_AR.full.pred <- forecast(HW_AR.full, h = 7, level = 0)
HW_AR.full.pred

# Plot residuals of the predictions for entire data before AR(1).
plot(HW_AR.full.pred$residuals, 
     xlab = "Time", ylab = "Residuals", ylim = c(-50000, 6000), bty = "l",
     xaxt = "n", xlim = c(2000, 2021.25), 
     main = "HW Residuals for entire Data before AR(1)", 
     col = "brown", lwd = 3) 
axis(1, at = seq(2000, 2021.25, 1), labels = format(seq(2000, 2021.25, 1)))

Acf(HW_AR.full.pred$residuals, lag.max = 12, 
    main = "Autocorrelation for the HW residuals- entire data set")

# Use Arima() function to fit AR(1) model for regression residuals.
# The ARIMA model order of order = c(1,0,0) gives an AR(1) model.
# Use forecast() function to make prediction of residuals into the future
residual.ar1 <- Arima(HW_AR.full.pred$residuals, order = c(1,0,0))
residual.ar1.pred <- forecast(residual.ar1, h = 7, level = 0)

# Use summary() to identify parameters of AR(1) model.
summary(residual.ar1)


# Use Acf() function to identify autocorrealtion for the residual of residuals 
# and plot autocorrelation for different lags (up to maximum of 12).
Acf(residual.ar1$residuals, lag.max = 12, 
    main = "Autocorrelation for Passenger Enplanements residual of Residuals for Entire Data Set")

# Identify forecast for the future 7 periods as sum of Holt-Winter model
# and AR(1) model for residuals.
HW.ar1.pred <- HW_AR.full.pred$mean + residual.ar1.pred$mean
HW.ar1.pred

# Create a data table with HW forecast and AR(1) model for residuals for 7 future periods 
#and combined two-level forecast for
# 7 future periods. 
table.df <- data.frame(HW_AR.full.pred$mean, 
                       residual.ar1.pred$mean, HW.ar1.pred)
names(table.df) <- c("HW Forecast", "AR(1)Forecast","Combined.Forecast")
table.df

plot(travel.ts, 
     xlab = "Years", ylab = "Passenger enplanements(000s)",bty = "l",
     xlim = c(2000, 2021.25), lwd=2,
     main = "HW-ZZZ Forecast and HW+AR(1)Two-level Forecast") 
axis(1, at = seq(2000, 2021.25,1), labels = format(seq(2000, 2021.25, 1)))
lines(HW_AR.full.pred$fitted, col = "orange", lwd = 2)
lines(HW_AR.full.pred$mean, col = "blue", lty =5, lwd = 2)
lines(HW.ar1.pred, col = "green", lty =5, lwd = 2)
legend(x= "bottomleft", legend = c("Travel Passenger Enplanements","Holt Winter's ZZZ - Entire Data", 
                                   "HW forecast for future", "HW+AR(1) Forecast for future"), 
       col = c("black", "orange","blue","green"), 
       lty = c(1,1,1,5,5,5), lwd =c(2), bty = "n",        
       pch=c(".",".", ".", ".","."), merge=TRUE, cex=0.8, y.intersp=0.6)
lines(c(2020.5,2020.5 ), c(0,80000))
lines(c(2021.5, 2021.5), c(0, 80000))
text(2009, 85000, "Training")
text(2021.5, 85000, "Future")
arrows(2000, 80000, 2020.5, 80000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.5, 80000, 2021.5, 80000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

round(accuracy(HW_AR.full.pred$fitted+residual.ar1.pred$fitted, travel.ts), 3)

################# MODEL-6 ARIMA+ MA for residuals -  TRAVEL ##################################

#Develop ARIMA model for the training dataset
ar1_train <- auto.arima(train.ts)
ar1_train

# Use forecast() function to make prediction of validation period
ar1_train.pred <- forecast(ar1_train, h = nValid, level = 0)
ar1_train.pred

plot(travel.ts, 
     xlab = "Year", ylab = "Passenger enplanements(in 000s)", bty = "l",
     xaxt = "n", xlim = c(2000, 2021.25), 
     main = "Auto ARIMA forecast for validation period", flty = 2) 
axis(1, at = seq(2000, 2021.25, 1), labels = format(seq(2000, 2021.25, 1)))
lines(ar1_train.pred$fitted, col = "orange", lwd = 2)
lines(ar1_train.pred$mean, col = "blue", lty =5, lwd = 2)
legend(x= "bottomleft",legend = c( "Travel-Passenger enplanements",
                                   "Auto ARIMA", "Auto ARIMA Forecast for validation"), 
       col = c("black","orange", "blue"), 
       lty = c(1,1,5,1,5), lwd =c(2), bty = "n",
       pch=c(".",".", ".", ".","."), merge=TRUE, cex=0.8, y.intersp=0.6)
lines(c(2019,2019 ), c(0,80000))
lines(c(2020.7, 2020.7), c(0, 80000))
text(2009, 85000, "Training")
text(2020.5, 85000, "Validation")
arrows(2000, 80000, 2019, 80000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019, 80000, 2020.7, 80000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

#get the train-validation residuals
ar1_train.res <- ar1_train.pred$residuals
ar1_train.res

# Plot residuals of the train-validation predictions
plot(ar1_train.res, 
     xlab = "Time", ylab = "Residuals", bty = "l",
     xaxt = "n", xlim = c(2000, 2021), 
     main = "HW Residuals for full Data before MA", 
     col = "blue", lwd = 3) 
axis(1, at = seq(2000,2021, 1), labels = format(seq(2000, 2021, 1)))

# Apply trailing MA with 2 periods in the window to residuals.
ar1_ma.trailing.res_2 <- rollmean(ar1_train.res, k = 2, align = "right")
ar1_ma.trailing.res_2

# Create forecast for residuals for the 7 periods into the future.
ar1_ma.trailing.res_2.pred <- forecast(ar1_ma.trailing.res_2, h = nValid, level = 0)
ar1_ma.trailing.res_2.pred

# To develop real forecast for validation period. 
# combine HW forecast and trailing MA forecast for residuals.
ar1MA_ts.forecast.2 <- ar1_train.pred$mean + ar1_ma.trailing.res_2.pred$mean
ar1MA_ts.forecast.2

# Create a table with regression forecast, trailing MA residual's forecast, and 
# total forecast for 12 months into the future.
total.ar1MA.pred <- data.frame(ar1_train.pred$mean, ar1_ma.trailing.res_2.pred$mean, 
                               ar1MA_ts.forecast.2)
total.ar1MA.pred

plot(travel.ts, 
     xlab = "Years", ylab = "Passenger enplanements",bty = "l",
     xlim = c(2000, 2021.25), lwd=2,
     main = "Auto ARIMA and MA of residuals ") 
axis(1, at = seq(2000, 2021.25, 1), labels = format(seq(2000, 2021.25, 1)))
#lines(reg.trend.seas.res, col = "black", lty =1, lwd = 1)
lines(ar1_train.pred$fitted, col = "orange", lwd = 2)
#lines(ar1_train.res, col = "brown", lwd = 2)
#lines(ar1_ma.trailing.res_2.pred$fitted, col = "orange", lwd = 2)
lines(ar1_train.pred$mean, col = "blue", lty =5, lwd = 2)
lines(ar1MA_ts.forecast.2, col = "green", lty =5, lwd = 2)
#lines(HWMA_ts.forecast.2, col = "blue", lty =1, lwd = 2)
legend(x="bottomleft", legend = c("Travel Passenger Enplanements","Auto ARIMA model", 
                                  "Auto ARIMA model forecast for validation", "Auto Arima +MA Forecast for validation"), 
       col = c("black","orange", "blue","green"), 
       lty = c(1,1,5,5), lwd =c(2), bty = "n",        
       pch=c(".",".", ".", ".","."), merge=TRUE, cex=0.8, y.intersp=0.6)

lines(c(2019,2019 ), c(0,80000))
lines(c(2020.7, 2020.7), c(0, 80000))
text(2009, 85000, "Training")
text(2020.5, 85000, "Validation")
arrows(2000, 80000, 2019, 80000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019, 80000, 2020.7, 80000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

round(accuracy(ar1_train.pred$fitted+ar1_ma.trailing.res_2.pred$fitted, travel.ts), 3)

##FULL DATASET

#Develop ARIMA model for the training dataset
ar1_full <- auto.arima(travel.ts)
ar1_full
summary(ar1_full)
# Use forecast() function to make prediction of future 7 periods
ar1_full.pred <- forecast(ar1_full, h = 7, level = 0)
ar1_full.pred

plot(travel.ts, 
     xlab = "Year", ylab = "Passenger enplanements(in 000s)", bty = "l",
     xaxt = "n", xlim = c(2000, 2022), ylim=c(0,100000),
     main = "AR(1) forecast for validation period", flty = 2) 
axis(1, at = seq(2000, 2022, 1), labels = format(seq(2000, 2022, 1)))
lines(ar1_full.pred$fitted, col = "orange", lwd = 2)
lines(ar1_full.pred$mean, col = "blue", lty =5, lwd = 2)
legend(x= "bottomleft",legend = c( "Travel-Passenger enplanements",
                                   "AR(1)", "AR(1) Forecast"), 
       col = c("black","orange", "blue"), 
       lty = c(1,1,5,1,5), lwd =c(2), bty = "n",
       pch=c(".",".", ".", ".","."), merge=TRUE, cex=0.8, y.intersp=0.6)
lines(c(2020.6,2020.6 ), c(0,100000))
lines(c(2022, 2022), c(0, 100000))
text(2009, 90000, "Training")
text(2021, 90000, "Future")
arrows(2000, 100000, 2020.6, 100000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.6, 100000, 2022, 100000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

#get the full dataset residuals
ar1_full.res <- ar1_full.pred$residuals
ar1_full.res

# Plot residuals of the train-validation predictions
plot(ar1_full.res, 
     xlab = "Time", ylab = "Residuals", bty = "l",
     xaxt = "n", xlim = c(2000, 2021), 
     main = "HW Residuals for full Data before MA", 
     col = "blue", lwd = 3) 
axis(1, at = seq(2000,2021, 1), labels = format(seq(2000, 2021, 1)))

# Apply trailing MA with 2 periods in the window to residuals.
ar1full_ma.trailing.res_2 <- rollmean(ar1_full.res, k = 2, align = "right")
ar1full_ma.trailing.res_2

# Create forecast for residuals for the 7 periods into the future.
ar1full_ma.trailing.res_2.pred <- forecast(ar1full_ma.trailing.res_2, h = 7, level = 0)
ar1full_ma.trailing.res_2.pred

# To develop real forecast for 7 periods into the future, 
# combine HW forecast and trailing MA forecast for residuals.
ar1MAfull_ts.forecast.2 <- ar1_full.pred$mean + ar1full_ma.trailing.res_2.pred$mean
ar1MAfull_ts.forecast.2

# Create a table with AR1 forecast, trailing MA residual's forecast, and 
# total forecast for 7 months into the future.
total.ar1MAfull.pred <- data.frame(ar1_full.pred$mean, ar1full_ma.trailing.res_2.pred$mean, 
                                   ar1MAfull_ts.forecast.2)
total.ar1MAfull.pred

plot(travel.ts, 
     xlab = "Years", ylab = "Passenger enplanements(in 000s)",bty = "l",
     xlim = c(2000, 2021.25), lwd=2,
     main = "Auto ARIMA and Moving Average Two-level Forecast") 
axis(1, at = seq(2000, 2021.25, 1), labels = format(seq(2000, 2021.25, 1)))
#lines(reg.trend.seas.res, col = "black", lty =1, lwd = 1)
lines(ar1_full.pred$fitted, col = "orange", lwd = 2)
#lines(ar1_full.res, col = "brown", lwd = 2)
#lines(ar1full_ma.trailing.res_2.pred$fitted, col = "orange", lwd = 2)
lines(ar1_full.pred$mean, col = "blue", lty =5, lwd = 2)
lines(ar1MAfull_ts.forecast.2, col = "green", lty =5, lwd = 2)
#lines(HWMA_ts.forecast.2, col = "blue", lty =1, lwd = 2)
legend(2008,40000, legend = c("Travel Passenger Enplanements","AR1 model ", 
                              "Auto ARIMA forecast for future", "Auto ARIMA+MA Forecast for future"), 
       col = c("black","orange", "blue","green"), 
       lty = c(1,1,5,5), lwd =c(2), bty = "n",        
       pch=c(".",".", ".", ".","."), merge=TRUE, cex=0.8, y.intersp=0.6)

lines(c(2020.5,2020.5 ), c(0,80000))
lines(c(2021.5, 2021.5), c(0, 80000))
text(2009, 85000, "Training")
text(2021.5, 85000, "Future")
arrows(2000, 80000, 2020.5, 80000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.5, 80000, 2021.5, 80000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

round(accuracy(ar1_full.pred$fitted+ar1full_ma.trailing.res_2.pred$fitted, travel.ts), 3)
######PLOT FOR LINEAR REG + EXT VARIABLE - NEGATIVE FOREAST
plot(travel.ts, 
     xlab = "Years", ylab = "Passenger Enplanements(000s)", bty = "l",
     xlim = c(2000, 2021.25), ylim = c(-200000,200000),
     main = "Regression Model and Forecast for Travel data") 
axis(1, at = seq(2000, 2021.25, 5), labels = format(seq(2000, 2021.25, 5)))
lines(reg.trend.seas$fitted, col = "brown", lwd = 2)
lines(reg.trend.seas.pred$mean, col = "orange", lty =5, lwd = 2)
legend(x= "bottomleft", legend = c("Travel", "Regression", "Regression Forecast"), 
       col = c("black", "brown", "orange"), 
       lty = c(1,1,5), lwd =c(2), bty = "n",        
       pch=c(".",".", ".", ".","."), merge=TRUE, cex=0.8, y.intersp=0.6)

######PLOT OF ALL 5 MODELS EXCEPT REGRESSION
plot(travel.ts, 
     xlab = "Years", ylab = "Passenger enplanements(in 000s)",bty = "l",
     xlim = c(2000, 2021.25), lwd=2,
     main = "Forecast for Travel data - All models(except Regression with ext. variables)") 
axis(1, at = seq(2000, 2021.25, 1), labels = format(seq(2000, 2021.25, 1)))
#lines(reg.trend.seas.res, col = "black", lty =1, lwd = 1)
lines(travel_ma.trailing_2, col = "green", lwd = 2, lty = 1) #MA 6.5 MAPE
lines(travel_ma.trailing_2.pred$mean, col = "green", lwd = 2, lty = 5) #MA 6.5 MAPE
lines(hw_full.ZZZ.pred$fitted, col = "orange", lwd = 2) #ZZZ+MA(2) 7.0 MAPE
lines(HWMA_ts.forecast.2, col = "orange", lty =5, lwd = 2) #ZZZ+MA(2) 7.0 MAPE
lines(HW_AR.full.pred$fitted, col = "blue", lwd = 2) #ZZZ+AR residuals 6.4 MAPE
lines(HW.ar1.pred, col = "blue", lty =5, lwd = 2) #ZZZ+AR residuals 6.4 MAPE
#lines(ar1_full.pred$mean, col = "violet", lty =5, lwd = 2)#Auto arima
lines(ar1MAfull_ts.forecast.2, col = "violet", lty =5, lwd = 2)
lines(hw.ZZZ.full.pred$mean, col = "red", lty =5, lwd = 2)

#lines(ar1_full.pred$fitted, col = "red", lwd = 2) #auto.arima 8 MAPE 
#lines(ar1MAfull_ts.forecast.2, col = "red", lty =5, lwd = 2) #auto arima+MA of residual 8 MAPE
legend(2003,35000, legend = c("Travel Passenger Enplanements","MA Entire", 
                              "MA Forecast", "ZZZ + AR Entire", "ZZZ + AR Forecast", "ZZZ+ MA Entire",
                              "ZZZ + MA Forecast","ZZZ model forecast","Auto Arima forecast"), 
       col = c("black","green", "green","blue","blue","orange", "orange","Red","Violet"), 
       lty = c(1,1,5,1,5,1,5,5,5), lwd =c(2), bty = "n",        
       pch=c(".",".", ".", ".","."), merge=TRUE, cex=0.8, y.intersp=0.6)

lines(c(2020.5,2020.5 ), c(0,80000))
lines(c(2021.5, 2021.5), c(0, 80000))
text(2009, 85000, "Training")
text(2021.5, 85000, "Future")
arrows(2000, 80000, 2020.5, 80000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.5, 80000, 2021.5, 80000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

##### PLOT OF FINAL 3 MODELS

plot(travel.ts, 
     xlab = "Years", ylab = "Passenger enplanements(in 000s)",bty = "l",
     xlim = c(2000, 2021.25), lwd=2,
     main = "Comparison Of Top Three Model Results") 
axis(1, at = seq(2000, 2021.25, 1), labels = format(seq(2000, 2021.25, 1)))
#lines(reg.trend.seas.res, col = "black", lty =1, lwd = 1)
lines(travel_ma.trailing_2, col = "green", lwd = 2, lty = 1) #MA 6.5 MAPE
lines(travel_ma.trailing_2.pred$mean, col = "green", lwd = 2, lty = 5) #MA 6.5 MAPE
lines(hw_full.ZZZ.pred$fitted, col = "orange", lwd = 2) #ZZZ+MA(2) 7.0 MAPE
lines(HWMA_ts.forecast.2, col = "orange", lty =5, lwd = 2) #ZZZ+MA(2) 7.0 MAPE
lines(HW_AR.full.pred$fitted, col = "blue", lwd = 2) #ZZZ+AR residuals 6.4 MAPE
lines(HW.ar1.pred, col = "blue", lty =5, lwd = 2) #ZZZ+AR residuals 6.4 MAPE
#lines(ar1_full.pred$fitted, col = "red", lwd = 2) #auto.arima 8 MAPE 
#lines(ar1MAfull_ts.forecast.2, col = "red", lty =5, lwd = 2) #auto arima+MA of residual 8 MAPE
legend(2002,35000, legend = c("Travel Passenger Enplanements","MA Entire", 
                              "MA Forecast", "ZZZ Entire", "ZZZ + AR Forecast", "ZZZ+ MA Entire",
                              "ZZZ + MA Forecast"), 
       col = c("black","green", "green","blue","blue","orange", "orange"), 
       lty = c(1,1,5,1,5,1,5), lwd =c(2), bty = "n",        
       pch=c(".",".", ".", ".","."), merge=TRUE, cex=0.8, y.intersp=0.6)

lines(c(2020.5,2020.5 ), c(0,80000))
lines(c(2021.5, 2021.5), c(0, 80000))
text(2009, 85000, "Training")
text(2021.5, 85000, "Future")
arrows(2000, 80000, 2020.5, 80000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.5, 80000, 2021.5, 80000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

####### TESTING HYPOTHESIS THAT TRAVEL WILL BOUNCE BACK IF COVID CASES REDUCE ##################

########## FORECASTING TRAVEL USING Two-level model - Linear Trend+ Season + Covid updated forecast  ###########################

# Fit a regression model with linear trend.
reg.trend.seas <- tslm(travel.ts ~ trend + season + covid.ts)
summary(reg.trend.seas) #Adjusted R-squared:  0.6796 

forecast_param <- data.frame(trend = c(249:255), covid.ts = forecast.ts.dummy)
class(forecast.data$forecast)

# Create forecast for the 7 periods into the future.
reg.trend.seas.pred <- forecast(reg.trend.seas, newdata = forecast_param, h = 7, level = 0)
reg.trend.seas.pred

plot(travel.ts, 
     xlab = "Years", ylab = "covid", bty = "l",
     xlim = c(2000, 2021.25), lwd=2,
     main = "Regression Model and Forecast") 
axis(1, at = seq(2000, 2021.25, 1), labels = format(seq(2000, 2021.25, 1)))
lines(reg.trend.seas$fitted, col = "brown", lwd = 2)
lines(reg.trend.seas.pred$mean, col = "brown", lty =5, lwd = 2)
legend(x= "topright", legend = c("COVID", "Regression", "Regression Forecast"), 
       col = c("black", "brown", "brown"), 
       lty = c(1,1,5), lwd =c(2), bty = "n",        
       pch=c(".",".", ".", ".","."), merge=TRUE, cex=0.8, y.intersp=0.6)

reg.trend.seas.res <- reg.trend.seas$residuals
reg.trend.seas.res

#trend exists in residuals post regression, so we should include it in the forecast
autocor <- Acf(reg.trend.seas.res, lag = 12, main = "Autocorrelation for Travel data post regression")

# Apply trailing MA with 2 periods in the window to residuals.
ma.trailing.res_2 <- rollmean(reg.trend.seas.res, k = 2, align = "right")
ma.trailing.res_2

# Create forecast for residuals for the 12 periods into the future.
ma.trailing.res_2.pred <- forecast(ma.trailing.res_2, h = 7, level = 0)
ma.trailing.res_2.pred

# To develop real forecast for 12 periods into the future, 
# combine regression forecast and trailing MA forecast for residuals.
ts.forecast.2 <- reg.trend.seas.pred$mean + ma.trailing.res_2.pred$mean
ts.forecast.2

# Create a table with regression forecast, trailing MA residual's forecast, and 
# total forecast for 12 months into the future.
total.reg.ma.pred <- data.frame(reg.trend.seas.pred$mean, ma.trailing.res_2.pred$mean, 
                                ts.forecast.2)
total.reg.ma.pred

#check y label and title
plot(travel.ts, 
     xlab = "Years", ylab = "Passenger enplanements(in 000s)",bty = "l",
     xlim = c(2000, 2021.25), lwd=2,
     main = "Residuals, Trailing MA of residuals and Residual Forecast") 
axis(1, at = seq(2000, 2021.25, 1), labels = format(seq(2000, 2021.25, 1)))
#lines(reg.trend.seas.res, col = "black", lty =1, lwd = 1)
lines(reg.trend.seas$fitted, col = "brown", lwd = 2)
lines(reg.trend.seas.pred$mean, col = "brown", lty =5, lwd = 2)
lines(ts.forecast.2, col = "blue", lty =5, lwd = 2)
#lines(ma.trailing.res_2.pred$mean, col = "blue", lty =5, lwd = 2)
legend(x= "topleft", legend = c("Passenger Enplanements", "Regression Entire Data", "Regression Forecast", "Total Forecast Regression + MA Residuals"), 
       col = c("black","brown","brown", "blue"), 
       lty = c(1,1,5,5), lwd =c(2), bty = "n",        
       pch=c(".",".", ".", ".","."), merge=TRUE, cex=0.8, y.intersp=0.6)

round(accuracy(reg.trend.seas.pred$fitted, travel.ts), 3)
round(accuracy(reg.trend.seas.pred$fitted+ma.trailing.res_2, travel.ts), 3)
round(accuracy(snaive(travel.ts)$fitted, travel.ts), 3)


