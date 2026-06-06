library(forecast)

consumo_ts_clean <- ts(df_clean$CONSUMO, frequency = 7)

ets_fit <- ets(consumo_ts_clean)
summary(ets_fit)

png("figs/ets_diag.png", width = 800, height = 600)
checkresiduals(ets_fit)
dev.off()


n_test   <- 28
train    <- df_clean[1:(nrow(df_clean) - n_test), ]
test     <- df_clean[(nrow(df_clean) - n_test + 1):nrow(df_clean), ]
train_ts <- ts(train$CONSUMO, frequency = 7)

ets_train <- ets(train_ts)
ets_fc    <- forecast(ets_train, h = n_test, level = c(80, 95))


autoplot(ets_fc) +
  autolayer(ts(test$CONSUMO, frequency = 7), series = "Actual") +
  labs(title = paste0("ETS(", ets_train$method, ") - 28-day Forecast"),
       y = "Consumption (GWh)", x = "") +
  scale_colour_manual(values = c("Actual" = "black"))
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
