head(df_clean$DATE)
tail(df_clean$DATE)

n_test    <- 28
train     <- df_clean[1:(nrow(df_clean) - n_test), ]
test      <- df_clean[(nrow(df_clean) - n_test + 1):nrow(df_clean), ]
n_context <- 60
context   <- tail(train, n_context)

cat("Train: ", as.character(min(train$DATE)), "a", as.character(max(train$DATE)), "\n")
cat("Test:  ", as.character(min(test$DATE)),  "a", as.character(max(test$DATE)),  "\n")

train_ts <- ts(train$CONSUMO, frequency = 7)

m2_train <- Arima(train_ts, order = c(1,1,1), seasonal = c(1,1,1))

sarima_forecast <- forecast(m2_train, h = n_test, level = c(80, 95))

sarima_plot_df <- data.frame(
  DATE     = c(context$DATE, test$DATE),
  actual   = c(context$CONSUMO, test$CONSUMO),
  mean     = c(rep(NA, n_context), as.numeric(sarima_forecast$mean)),
  lower_80 = c(rep(NA, n_context), as.numeric(sarima_forecast$lower[, 1])),
  upper_80 = c(rep(NA, n_context), as.numeric(sarima_forecast$upper[, 1])),
  lower_95 = c(rep(NA, n_context), as.numeric(sarima_forecast$lower[, 2])),
  upper_95 = c(rep(NA, n_context), as.numeric(sarima_forecast$upper[, 2]))
)

ggplot(sarima_plot_df, aes(x = DATE)) +
  geom_ribbon(aes(ymin = lower_95, ymax = upper_95), fill = "steelblue", alpha = 0.2) +
  geom_ribbon(aes(ymin = lower_80, ymax = upper_80), fill = "steelblue", alpha = 0.3) +
  geom_line(aes(y = actual, colour = "Actual")) +
  geom_line(aes(y = mean, colour = "Forecast"), na.rm = TRUE) +
  scale_colour_manual(values = c("Actual" = "black", "Forecast" = "steelblue")) +
  labs(title = "SARIMA(1,1,1)(1,1,1)[7] - 28-day Forecast",
       y = "Consumption (GWh)", x = "", colour = "")
ggsave("figs/sarima_forecast.png", width = 7, height = 4)

gjr_fit_train <- ugarchfit(spec = garch_spec_gjr,
                           data = residuals(m2_train))

gjr_forecast <- ugarchforecast(gjr_fit_train, n.ahead = n_test)

sarima_mean  <- as.numeric(sarima_forecast$mean)
garch_sigma  <- as.numeric(sigma(gjr_forecast))

lower_95 <- sarima_mean - 1.96 * garch_sigma
upper_95 <- sarima_mean + 1.96 * garch_sigma
lower_80 <- sarima_mean - 1.282 * garch_sigma
upper_80 <- sarima_mean + 1.282 * garch_sigma

forecast_df <- data.frame(
  DATE     = test$DATE,
  actual   = test$CONSUMO,
  mean     = sarima_mean,
  lower_80 = lower_80,
  upper_80 = upper_80,
  lower_95 = lower_95,
  upper_95 = upper_95
)

garch_plot_df <- data.frame(
  DATE     = c(context$DATE, test$DATE),
  actual   = c(context$CONSUMO, test$CONSUMO),
  mean     = c(rep(NA, n_context), sarima_mean),
  lower_80 = c(rep(NA, n_context), lower_80),
  upper_80 = c(rep(NA, n_context), upper_80),
  lower_95 = c(rep(NA, n_context), lower_95),
  upper_95 = c(rep(NA, n_context), upper_95)
)

ggplot(garch_plot_df, aes(x = DATE)) +
  geom_ribbon(aes(ymin = lower_95, ymax = upper_95),
              fill = "steelblue", alpha = 0.2) +
  geom_ribbon(aes(ymin = lower_80, ymax = upper_80),
              fill = "steelblue", alpha = 0.3) +
  geom_line(aes(y = actual, colour = "Actual")) +
  geom_line(aes(y = mean, colour = "Forecast"), na.rm = TRUE) +
  scale_colour_manual(values = c("Actual" = "black", "Forecast" = "steelblue")) +
  labs(title = "SARIMA + GJR-GARCH - 28-day Forecast",
       y = "Consumption (GWh)", x = "", colour = "")
ggsave("figs/garch_forecast.png", width = 7, height = 4)

vol_df <- data.frame(
  DATE  = c(context$DATE, test$DATE),
  sigma = c(tail(as.numeric(sigma(gjr_fit_train)), n_context), as.numeric(sigma(gjr_forecast))),
  type  = c(rep("Historical", n_context), rep("Forecast", n_test))
)

ggplot(vol_df, aes(x = DATE, y = sigma, colour = type)) +
  geom_line() +
  scale_colour_manual(values = c("Historical" = "black", "Forecast" = "steelblue")) +
  labs(title = "GJR-GARCH(1,1) - Conditional Volatility",
       y = "Conditional Std. Dev. (GWh)", x = "", colour = "")
ggsave("figs/vol_forecast.png", width = 7, height = 4)

accuracy_metrics <- function(actual, predicted) {
  e <- actual - predicted
  data.frame(
    MAE  = mean(abs(e)),
    RMSE = sqrt(mean(e^2)),
    MAPE = mean(abs(e / actual)) * 100
  )
}

acc_sarima <- accuracy_metrics(test$CONSUMO, sarima_mean)
acc_garch  <- accuracy_metrics(test$CONSUMO, sarima_mean)

data.frame(
  Model = c("SARIMA(1,1,1)(1,1,1)[7]",
            "SARIMA + GJR-GARCH(1,1)"),
  MAE   = c(acc_sarima$MAE,  acc_garch$MAE),
  RMSE  = c(acc_sarima$RMSE, acc_garch$RMSE),
  MAPE  = c(acc_sarima$MAPE, acc_garch$MAPE)
)

sarima_boot <- forecast(m2_train, h = n_test,
                        level = c(80, 95),
                        bootstrap = TRUE,
                        npaths = 1000)

boot_plot_df <- data.frame(
  DATE     = c(context$DATE, test$DATE),
  actual   = c(context$CONSUMO, test$CONSUMO),
  mean     = c(rep(NA, n_context), as.numeric(sarima_boot$mean)),
  lower_80 = c(rep(NA, n_context), as.numeric(sarima_boot$lower[, 1])),
  upper_80 = c(rep(NA, n_context), as.numeric(sarima_boot$upper[, 1])),
  lower_95 = c(rep(NA, n_context), as.numeric(sarima_boot$lower[, 2])),
  upper_95 = c(rep(NA, n_context), as.numeric(sarima_boot$upper[, 2]))
)

ggplot(boot_plot_df, aes(x = DATE)) +
  geom_ribbon(aes(ymin = lower_95, ymax = upper_95), fill = "forestgreen", alpha = 0.2) +
  geom_ribbon(aes(ymin = lower_80, ymax = upper_80), fill = "forestgreen", alpha = 0.3) +
  geom_line(aes(y = actual, colour = "Actual")) +
  geom_line(aes(y = mean, colour = "Forecast"), na.rm = TRUE) +
  scale_colour_manual(values = c("Actual" = "black", "Forecast" = "forestgreen")) +
  labs(title = "SARIMA - Bootstrap Forecast Intervals",
       y = "Consumption (GWh)", x = "", colour = "")
ggsave("figs/boot_forecast.png", width = 7, height = 4)
