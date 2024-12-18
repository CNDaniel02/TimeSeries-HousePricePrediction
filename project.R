library(readxl)
library(ggplot2)
library(lubridate)
library(zoo)
library(dplyr)
library(forecast)
library(tseries)
data<-read_excel("E:/Study/STA137/Project/MedianPricesofExistingDetachedHomesHistoricalData.xlsx",skip = 7)
#no data before line 69
data <- data %>%
  slice(69:n()) %>% 
  select(`Mon-Yr`, SoCal) #'Mon-Yr''SoCal' col

head(data)

###########################################################1

# make $### to number
data$SoCal <- as.numeric(data$SoCal)

# origin plot
ggplot(data, aes(x = `Mon-Yr`, y = SoCal)) +
  geom_line(color = "blue") +
  labs(title = "SoCal Home Prices Over Time", 
       x = "Date", 
       y = "SoCal Home Prices") +
  theme_minimal()

# boxplot check for outliers
ggplot(data, aes(x = 1, y = SoCal)) +
  geom_boxplot() +
  labs(title = "Box Plot of SoCal Home Prices", y = "SoCal Home Prices") +
  theme_minimal() +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())

#clean NA
socalClean <- na.omit(data)


# boxcox transformation
lamda <- BoxCox.lambda(data$SoCal, method = "loglik")
dataTransform <- BoxCox(data$SoCal, lamda)
print(lamda)

#plot transformed data
ggplot(data, aes(x = `Mon-Yr`, y = dataTransform)) +
  geom_line() +
  labs(title = "Transformed SoCal Home Prices Over Time", x = "Date", y = "Transformed SoCal Home Prices") +
  theme_minimal()



# Convert transformed data to time series
socal_ts <- ts(dataTransform, start = c(1995, 9), frequency = 12)

# Decompose time series
dataDecomp <- decompose(socal_ts, type = "multiplicative")
plot(dataDecomp)

# Extract trend and seasonal components
trend <- dataDecomp$trend
season <- dataDecomp$seasonal


# Calculate residuals and remove NA values
residuals <- socal_ts - trend - season
residualsClean <- na.omit(residuals)

# ACF and PACF for residuals
acf(residualsClean, main = "ACF of Residuals")
pacf(residualsClean, main = "PACF of Residuals")

# Ljung-Box test for whiteness
Box.test(residualsClean, type = "Ljung-Box")

# QQ plot and Shapiro-Wilk test for normality
qqnorm(residualsClean)
qqline(residualsClean)
shapiTest <- shapiro.test(residualsClean)
print(shapiTest)

#####################################################analyze 4
# Fit ARMA model
armaModel <- auto.arima(residualsClean, max.p = 5, max.q = 5, stationary = TRUE, seasonal = FALSE)
summary(armaModel)

# Check ACF and PACF
armaResiduals <- residuals(armaModel)

acf(armaResiduals, main = "ACF of ARMA Residuals")
pacf(armaResiduals, main = "PACF of ARMA Residuals")

# Ljung-Box test for checking white noise
Box.test(armaResiduals, type = "Ljung-Box")

# Shapiro-Wilk test for normality
shapiTest2 <- shapiro.test(armaResiduals)
print(shapiTest2)

# QQ plot for normality
qqnorm(armaResiduals)
qqline(armaResiduals)

#############################################################analyze 5

#predict
trendClean <- na.omit(trend)

# Predict the future trend 10 years
trendForecast <- forecast(trendClean, h = 120)

#Predict the seasonal component 
seasonForecast <- rep(tail(season, 12), 10)#12 month repeat 10 times

# predict the rough component
armaModel <- auto.arima(residualsClean)  #fit arma for cleaned residuals
armaForecast <- forecast(armaModel, h = 120)  # 10 year forcast

# Combine smooth and rough forecasts
smoothForecast <- trendForecast$mean + seasonForecast
finalForecast <- smoothForecast + armaForecast$mean 


#original scale
actFinalForecast <- InvBoxCox(finalForecast, lamda)

actTrendForecast <- InvBoxCox(trendForecast$mean, lamda)

actual_socal_ts <- InvBoxCox(socal_ts, lamda)

#Plot forecasted with original
plot(actFinalForecast, main = "Forecast of SoCal Home Prices (Original Scale)", 
     xlab = "Date", ylab = "SoCal Home Prices", col = "blue", type = "l")
summary(season)


# original
lines(actual_socal_ts, col = "black")

legend("topright", legend = c("Original", "Forecast"), col = c("black", "blue"), lty = 1)

ts.plot(actual_socal_ts, actTrendForecast, col = c("blue", "red"), lty = 1:2)
legend("topright", legend = c("Original", "Forecast"), col = c("blue", "red"), lty = 1:2)


length(seasonForecast)

timeSeq <- seq(2024, 2024 + (120 - 1) / 12, by = 1/12)

length(timeSeq)


par(mfrow = c(3, 1))


plot(timeSeq, actTrendForecast , type = "l", 
     main = "Predicted Trend for Next 10 Years", 
     xlab = "Year", ylab = "Trend", col = "blue")

plot(timeSeq, seasonForecast, type = "l", 
     main = "Predicted Seasonal Component for Next 10 Years", 
     xlab = "Year", ylab = "Seasonal Effect", col = "green")

plot(timeSeq, armaForecast$mean, type = "l", 
     main = "Predicted Residuals for Next 10 Years", 
     xlab = "Year", ylab = "Residuals", col = "red")

par(mfrow = c(1, 1))

ts.plot(actual_socal_ts, actFinalForecast, col = c("blue", "red"), lty = 1:2, 
        main = "SoCal Home Prices: Original vs Forecast (10 Years)",
        xlab = "Year", ylab = "Home Prices")
legend("topleft", legend = c("Original", "Forecast"), col = c("blue", "red"), lty = 1:2)


# original scale
actFinalForecast <- InvBoxCox(finalForecast, lamda)
#print actual value
forecast_years <- seq(2024, 2024 + (length(actFinalForecast) - 1) / 12, by = 1/12)
forecast_df <- data.frame(Year = forecast_years, Predicted_Home_Prices = actFinalForecast)
print(forecast_df)













