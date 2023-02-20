library(forecast)
library(ggplot2)
library(tseries)
library(readr)
# load and time sequencialize data
data <- read.csv("./BITCOIN.csv")

train_size <- as.integer(length(data$Value) * 0.67)
test_size <- length(data$Value) - train_size

train <- ts(data[1:train_size,]$Value, frequency = 365)
test <- data[(train_size + 1):length(data$Value),]$Value

# check stationary
kpss.test(train) # p-value<0.01, no stationary
autoplot(train)
# check seasonality
season <- tbats(train)
isSeasonal <- !is.null(season$seasonal)
isSeasonal # Not seasonal
# obtain ARIMA model
fit <- auto.arima(train, seasonal = FALSE,
                  approximation = FALSE, trace = TRUE)
fit
# create forecast model
forecast_data <- forecast(fit, h=test_size)
autoplot(forecast_data, xlab="year", ylab="price", main="")

fd <- data.frame(forecast_data)$Point.Forecast
sqrt(mean((fd-test)^2)) # caluculate RMSE

# build up stlf model
model <- stlf(train)
f <- forecast(model, h=test_size)
stlf_fd <- data.frame(f)$Point.Forecast
sqrt(mean(stlf_fd-test)^2) # caluculate RMSE
# draw the plot of stlf
library(ggthemes)
df <- data.frame(
  index = 1:length(data$Value),
  value = data$Value,
  type = 'raw') %>%
  rbind(
    data.frame(
      index = 1:length(train),
      value = as.numeric(train),
      type = 'train')) %>%
  rbind(
    data.frame(
      index = 1:length(test)+length(train),
      value = as.numeric(stlf_fd),
      type = 'test'))

ggplot(data = df) +
  geom_line(
    mapping = aes(
      x = index,
      y = value,
      color = type)) +
  geom_point(
    mapping = aes(
      x = index,
      y = value,
      color = type)) +
  geom_vline(
    xintercept = length(train) + 0.5) +
  theme_economist() +
  scale_color_economist()
