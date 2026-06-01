library(readr)

df <- read_csv("electricity_daily_REN_2025.csv")
head(df)
str(df)
summary(df)


install.packages(c("dplyr", "ggplot2", "lubridate", "forecast", "tseries"))

library(forecast)
library(tseries)
library(lubridate)
library(ggplot2)
library(dplyr)

# Fix date column name
df <- df %>% rename(DATE = ...1)

# Drop mostly-NA columns
df <- df %>% select(-ONDAS, -INJECAO_BATERIAS, -CONSUMO_BATERIAS)

# Confirm no missing values in target
sum(is.na(df$CONSUMO))

# Create time series object (daily)
consumo_ts <- ts(df$CONSUMO, 
                 start = c(2010, 1), 
                 frequency = 365.25)

# Quick sanity plot
autoplot(consumo_ts) +
  labs(title = "Daily Electricity Consumption - Portugal (REN)",
       y = "Consumption (GWh)", x = "")

ggplot(df, aes(x = DATE, y = CONSUMO)) +
  geom_line(colour = "steelblue") +
  labs(title = "Daily Electricity Consumption - Portugal (REN)",
       y = "Consumption (GWh)", x = "")

# ========= Zoom into a single year to spot weekly patterns =========

df %>%
  filter(year(DATE) == 2024) %>%
  ggplot(aes(x = DATE, y = CONSUMO)) +
  geom_line(colour = "steelblue") +
  labs(title = "Daily Electricity Consumption - 2024", y = "Consumption (GWh)", x = "")


## 4 - Seasonal exploration — average by day of week and month
# Day of week effect
df %>%
  mutate(weekday = wday(DATE, label = TRUE, week_start = 1)) %>%
  group_by(weekday) %>%
  summarise(mean_consumo = mean(CONSUMO)) %>%
  ggplot(aes(x = weekday, y = mean_consumo)) +
  geom_col(fill = "steelblue") +
  labs(title = "Average Consumption by Day of Week", y = "Mean Consumption", x = "")

# Month effect
df %>%
  mutate(month = month(DATE, label = TRUE)) %>%
  group_by(month) %>%
  summarise(mean_consumo = mean(CONSUMO)) %>%
  ggplot(aes(x = month, y = mean_consumo)) +
  geom_col(fill = "coral") +
  labs(title = "Average Consumption by Month", y = "Mean Consumption", x = "")

## 5 - Boxplots by month and weekday

df %>%
  mutate(month = month(DATE, label = TRUE)) %>%
  ggplot(aes(x = month, y = CONSUMO)) +
  geom_boxplot(fill = "steelblue", alpha = 0.6) +
  labs(title = "Consumption Distribution by Month", y = "Consumption (GWh)", x = "")

##ACF & PACF

par(mfrow = c(1, 2))
acf(df$CONSUMO, lag.max = 60, main = "ACF - CONSUMO")
pacf(df$CONSUMO, lag.max = 60, main = "PACF - CONSUMO")

## differencing to achieve stationary

# 1. Seasonal difference (lag = 7, removes weekly seasonality)
consumo_diff7 <- diff(df$CONSUMO, lag = 7)

# 2. Then regular first difference (removes trend)
consumo_diff1_7 <- diff(consumo_diff7, lag = 1)

# Plot the result
par(mfrow = c(1,1))
plot(consumo_diff1_7, type = "l", main = "CONSUMO - Seasonal + Regular Differencing", ylab = "")

# ACF and PACF of differenced series
par(mfrow = c(1, 2))
acf(consumo_diff1_7, lag.max = 60, main = "ACF - Differenced")
pacf(consumo_diff1_7, lag.max = 60, main = "PACF - Differenced")

# Stationarity tests on differenced series
adf.test(consumo_diff1_7)
kpss.test(consumo_diff1_7)

