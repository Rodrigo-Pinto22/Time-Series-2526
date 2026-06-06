library(forecast)

consumo_ts_clean <- ts(df_clean$CONSUMO, frequency = 7)

ets_fit <- ets(consumo_ts_clean)
summary(ets_fit)

png("figs/ets_diag.png", width = 800, height = 600)
checkresiduals(ets_fit)
dev.off()

n_test    <- 28
n_context <- 60
train     <- df_clean[1:(nrow(df_clean) - n_test), ]
test      <- df_clean[(nrow(df_clean) - n_test + 1):nrow(df_clean), ]
context   <- tail(train, n_context)
train_ts  <- ts(train$CONSUMO, frequency = 7)

ets_train <- ets(train_ts)
ets_fc    <- forecast(ets_train, h = n_test, level = c(80, 95))

ets_plot_df <- data.frame(
  DATE     = c(context$DATE, test$DATE),
  actual   = c(context$CONSUMO, test$CONSUMO),
  mean     = c(rep(NA, n_context), as.numeric(ets_fc$mean)),
  lower_80 = c(rep(NA, n_context), as.numeric(ets_fc$lower[, 1])),
  upper_80 = c(rep(NA, n_context), as.numeric(ets_fc$upper[, 1])),
  lower_95 = c(rep(NA, n_context), as.numeric(ets_fc$lower[, 2])),
  upper_95 = c(rep(NA, n_context), as.numeric(ets_fc$upper[, 2]))
)

ggplot(ets_plot_df, aes(x = DATE)) +
  geom_ribbon(aes(ymin = lower_95, ymax = upper_95), fill = "coral", alpha = 0.2) +
  geom_ribbon(aes(ymin = lower_80, ymax = upper_80), fill = "coral", alpha = 0.3) +
  geom_line(aes(y = actual, colour = "Actual")) +
  geom_line(aes(y = mean, colour = "Forecast"), na.rm = TRUE) +
  scale_colour_manual(values = c("Actual" = "black", "Forecast" = "coral")) +
  labs(title = paste0("ETS(", ets_train$method, ") - 28-day Forecast"),
       y = "Consumption (GWh)", x = "", colour = "")
ggsave("figs/ets_forecast.png", width = 7, height = 4)

ets_mean <- as.numeric(ets_fc$mean)
acc_ets  <- accuracy_metrics(test$CONSUMO, ets_mean)

data.frame(
  Model = c("SARIMA(1,1,1)(1,1,1)[7]",
            "SARIMA + GJR-GARCH(1,1)",
            paste0("ETS(", ets_train$method, ")")),
  MAE   = c(acc_sarima$MAE,  acc_garch$MAE,  acc_ets$MAE),
  RMSE  = c(acc_sarima$RMSE, acc_garch$RMSE, acc_ets$RMSE),
  MAPE  = c(acc_sarima$MAPE, acc_garch$MAPE, acc_ets$MAPE)
)
