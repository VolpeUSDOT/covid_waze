# Testing time-series RF model approach with Waze data at county and daily level.

# Setup ----
library(tidyverse)
library(tsibble)
library(randomForest)
library(forecast)
library(lubridate)

# Data
df <- read_csv('Output/2020-05-26/Waze_Covid_joined.csv')

  ) %>% 
  drop_na() %>% 
  as_tsibble(index = "Date") %>% 
  filter(Date <= "2018-12-01")

# convert to ts format
tax_ts <- as.ts(tax_tbl)

# implicit missings
has_gaps(tax_tbl)

# explicit missings
colSums(is.na(tax_tbl[, "Value"]))

# visualize
plot_org <- tax_tbl %>% 
  ggplot(aes(Date, Value / 1000)) + # to get the axis on a more manageable scale
  geom_line() +
  theme_minimal() +
  labs(title = "German Wage and Income Taxes 1999 - 2018", x = "Year", y = "Euros")

plot_org

# pretend we're in December 2017 and have to forecast the next twelve months
tax_ts_org <- window(tax_ts, end = c(2017, 12))

# estimate the required order of differencing
n_diffs <- nsdiffs(tax_ts_org)

# log transform and difference the data
tax_ts_trf <- tax_ts_org %>% 
  log() %>% 
  diff(n_diffs)

# check out the difference! (pun)
plot_trf <- tax_ts_trf %>% 
  autoplot() +
  xlab("Year") +
  ylab("Euros") +
  ggtitle("German Wage and Income Taxes 1999 - 2018") +
  theme_minimal()

gridExtra::grid.arrange(plot_org, plot_trf)

# Time Delay Embedding ----

# Time delay embedding represents a time series in a Euclidean space with the embedding dimension K.
# To do this in R, use the base function embed().
# All you have to do is plug in the time series object and set the embedding dimension as one greater than the desired number of lags.

lag_order <- 6 # the desired number of lags (six months)
horizon <- 12 # the forecast horizon (twelve months)

tax_ts_mbd <- embed(tax_ts_trf, lag_order + 1) # embedding magic!

tax_ts_trf[1] # Feb 1999 value
tax_ts_trf[1:7] # Feb 1999 to Aug 1999
tax_ts_mbd[1,] # First row of embedded vector: Aug 1999 backwards to Feb 1999. Embedding dimension is 7 (months) in this case.
tax_ts_mbd[2,] # Second row of embedded vector: Sept 1999 backwards to March 1999. 

# Direct Forecasting ----

y_train <- tax_ts_mbd[, 1] # the target (First column of the data, e.g. starting with August 1999)
X_train <- tax_ts_mbd[, -1] # everything but the target (Second to last columns of the data)

y_test <- window(tax_ts, start = c(2018, 1), end = c(2018, 12)) # the year 2018
X_test <- tax_ts_mbd[nrow(tax_ts_mbd), c(1:lag_order)] # the test set consisting
# of the six most recent values (we have six lags) of the training set. It's the
# same for all models.

# RF Forecasting ----

# Horizon in this case: 12 months. Loop for each value, make a separate model!

forecasts_rf <- numeric(horizon)

for (i in 1:horizon){
  # set seed
  set.seed(2019)
  
  # fit the model
  fit_rf <- randomForest(X_train, y_train)
  
  # predict using the test set
  forecasts_rf[i] <- predict(fit_rf, X_test)
  
  # here is where we repeatedly reshape the training data to reflect the time distance
  # corresponding to the current forecast horizon.
  y_train <- y_train[-1] 
  
  X_train <- X_train[-nrow(X_train), ] 
}

# Back transform the results ----



# calculate the exp term
exp_term <- exp(cumsum(forecasts_rf))

# extract the last observation from the time series (y_t)
last_observation <- as.vector(tail(tax_ts_org, 1))

# calculate the final predictions
backtransformed_forecasts <- last_observation * exp_term

# convert to ts format
y_pred <- ts(
  backtransformed_forecasts,
  start = c(2018, 1),
  frequency = 12
)

# add the forecasts to the original tibble
tax_tbl <- tax_tbl %>% 
  mutate(Forecast = c(rep(NA, length(tax_ts_org)), y_pred))

# visualize the forecasts
plot_fc <- tax_tbl %>% 
  ggplot(aes(x = Date)) +
  geom_line(aes(y = Value / 1000)) +
  geom_line(aes(y = Forecast / 1000), color = "blue") +
  theme_minimal() +
  labs(
    title = "Forecast of the German Wage and Income Tax for the Year 2018",
    x = "Year",
    y = "Euros"
  )

accuracy(y_pred, y_test)

benchmark <- forecast(snaive(tax_ts_org), h = horizon)

tax_ts %>% 
  autoplot() +
  autolayer(benchmark, PI = FALSE)

accuracy(benchmark, y_test)