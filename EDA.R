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

dir.create("figs", showWarnings = FALSE)

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
ggsave("figs/daily_consumptio.png", width = 7, height = 4)

# ========= Zoom into a single year to spot weekly patterns =========

df %>%
  filter(year(DATE) == 2024) %>%
  ggplot(aes(x = DATE, y = CONSUMO)) +
  geom_line(colour = "steelblue") +
  labs(title = "Daily Electricity Consumption - 2024", y = "Consumption (GWh)", x = "")
ggsave("figs/daily_consumption_2024.png", width = 7, height = 4)


## 4 - Seasonal exploration — average by day of week and month
# Day of week effect
df %>%
  mutate(weekday = wday(DATE, label = TRUE, week_start = 1)) %>%
  group_by(weekday) %>%
  summarise(mean_consumo = mean(CONSUMO)) %>%
  ggplot(aes(x = weekday, y = mean_consumo)) +
  geom_col(fill = "steelblue") +
  labs(title = "Average Consumption by Day of Week", y = "Mean Consumption", x = "")
ggsave("figs/average_consumption_dayweek.png", width = 7, height = 4)

# Month effect
df %>%
  mutate(month = month(DATE, label = TRUE)) %>%
  group_by(month) %>%
  summarise(mean_consumo = mean(CONSUMO)) %>%
  ggplot(aes(x = month, y = mean_consumo)) +
  geom_col(fill = "coral") +
  labs(title = "Average Consumption by Month", y = "Mean Consumption", x = "")
ggsave("figs/average_consumption_month.png", width = 7, height = 4)

## 5 - Boxplots by month and weekday

df %>%
  mutate(month = month(DATE, label = TRUE)) %>%
  ggplot(aes(x = month, y = CONSUMO)) +
  geom_boxplot(fill = "steelblue", alpha = 0.6) +
  labs(title = "Consumption Distribution by Month", y = "Consumption (GWh)", x = "")
ggsave("figs/distribution_consumption_month.png", width = 7, height = 4)


# Find optimal lambda
lambda <- BoxCox.lambda(df$CONSUMO)
lambda

# Plot transformed series
df$CONSUMO_BC <- BoxCox(df$CONSUMO, lambda)

ggplot(df, aes(x = DATE, y = CONSUMO_BC)) +
  geom_line(colour = "steelblue") +
  labs(title = paste0("Box-Cox Transformed Series (λ = ", round(lambda, 3), ")"),
       y = "Transformed Consumption", x = "")

# Add working day flag
df <- df %>%
  mutate(
    weekday = wday(DATE, week_start = 1),
    is_workday = ifelse(weekday <= 5, 1, 0)
  )

# Monthly working day adjustment
df <- df %>%
  mutate(month_year = floor_date(DATE, "month")) %>%
  group_by(month_year) %>%
  mutate(
    workdays_in_month = sum(is_workday),
    CONSUMO_ADJ = CONSUMO / workdays_in_month * 20  # normalise to 20 working days
  ) %>%
  ungroup()

# Compare raw vs adjusted
ggplot(df, aes(x = DATE)) +
  geom_line(aes(y = CONSUMO, colour = "Raw")) +
  geom_line(aes(y = CONSUMO_ADJ, colour = "Adjusted")) +
  labs(title = "Raw vs Working Day Adjusted Consumption",
       y = "Consumption (GWh)", x = "", colour = "")

##ACF & PACF

png("figs/acf_pacf.png", width = 800, height = 400)
par(mfrow = c(1, 2))
acf(df$CONSUMO, lag.max = 60, main = "ACF - CONSUMO")
pacf(df$CONSUMO, lag.max = 60, main = "PACF - CONSUMO")
dev.off()

## differencing to achieve stationary

# 1. Seasonal difference (lag = 7, removes weekly seasonality)
consumo_diff7 <- diff(df$CONSUMO, lag = 7)

# 2. Then regular first difference (removes trend)
consumo_diff1_7 <- diff(consumo_diff7, lag = 1)

# Plot the result
par(mfrow = c(1,1))
plot(consumo_diff1_7, type = "l", main = "CONSUMO - Seasonal + Regular Differencing", ylab = "")

# ACF and PACF of differenced series
png("figs/acf_pacf_diff.png", width = 800, height = 400)
par(mfrow = c(1, 2))
acf(consumo_diff1_7, lag.max = 60, main = "ACF - Differenced")
pacf(consumo_diff1_7, lag.max = 60, main = "PACF - Differenced")
dev.off()

# Stationarity tests on differenced series
adf.test(consumo_diff1_7)
kpss.test(consumo_diff1_7)

