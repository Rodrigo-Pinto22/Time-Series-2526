# Ver as datas disponíveis
head(df_clean$DATE)
tail(df_clean$DATE)

# Sugestão: últimas 4 semanas (28 dias) como test set
n_test <- 28

train <- df_clean[1:(nrow(df_clean) - n_test), ]
test  <- df_clean[(nrow(df_clean) - n_test + 1):nrow(df_clean), ]

# Confirmar datas
cat("Train: ", as.character(min(train$DATE)), "a", as.character(max(train$DATE)), "\n")
cat("Test:  ", as.character(min(test$DATE)),  "a", as.character(max(test$DATE)),  "\n")

train_ts <- ts(train$CONSUMO, frequency = 7)

m2_train <- Arima(train_ts, order = c(1,1,1), seasonal = c(1,1,1))


sarima_forecast <- forecast(m2_train, h = n_test, level = c(80, 95))

# Plot
autoplot(sarima_forecast) +
  autolayer(ts(test$CONSUMO, frequency = 7), series = "Actual") +
  labs(title = "SARIMA(1,1,1)(1,1,1)[7] - 28-day Forecast",
       y = "Consumption (GWh)", x = "") +
  scale_colour_manual(values = c("Actual" = "black"))


# Refit GJR-GARCH no training set
gjr_fit_train <- ugarchfit(spec = garch_spec_gjr, 
                           data = residuals(m2_train))

# Previsão GARCH (variância condicional)
gjr_forecast <- ugarchforecast(gjr_fit_train, n.ahead = n_test)

# Extrair previsões
sarima_mean  <- as.numeric(sarima_forecast$mean)
garch_sigma  <- as.numeric(sigma(gjr_forecast))

# Intervalos de confiança combinados (SARIMA mean ± 1.96 * GARCH sigma)
lower_95 <- sarima_mean - 1.96 * garch_sigma
upper_95 <- sarima_mean + 1.96 * garch_sigma
lower_80 <- sarima_mean - 1.282 * garch_sigma
upper_80 <- sarima_mean + 1.282 * garch_sigma

# Dataframe para plot
forecast_df <- data.frame(
  DATE     = test$DATE,
  actual   = test$CONSUMO,
  mean     = sarima_mean,
  lower_80 = lower_80,
  upper_80 = upper_80,
  lower_95 = lower_95,
  upper_95 = upper_95
)

# Plot SARIMA + GARCH
ggplot(forecast_df, aes(x = DATE)) +
  geom_ribbon(aes(ymin = lower_95, ymax = upper_95), 
              fill = "steelblue", alpha = 0.2) +
  geom_ribbon(aes(ymin = lower_80, ymax = upper_80), 
              fill = "steelblue", alpha = 0.3) +
  geom_line(aes(y = mean, colour = "Forecast")) +
  geom_line(aes(y = actual, colour = "Actual")) +
  labs(title = "SARIMA + GJR-GARCH - 28-day Forecast",
       y = "Consumption (GWh)", x = "", colour = "") +
  scale_colour_manual(values = c("Actual" = "black", "Forecast" = "steelblue"))


# Função auxiliar
accuracy_metrics <- function(actual, predicted) {
  e <- actual - predicted
  data.frame(
    MAE  = mean(abs(e)),
    RMSE = sqrt(mean(e^2)),
    MAPE = mean(abs(e / actual)) * 100
  )
}

# SARIMA
acc_sarima <- accuracy_metrics(test$CONSUMO, sarima_mean)

# SARIMA + GARCH (mesma média, intervalos diferentes)
acc_garch <- accuracy_metrics(test$CONSUMO, sarima_mean)

# Tabela comparativa
data.frame(
  Model = c("SARIMA(1,1,1)(1,1,1)[7]", 
            "SARIMA + GJR-GARCH(1,1)"),
  MAE   = c(acc_sarima$MAE,  acc_garch$MAE),
  RMSE  = c(acc_sarima$RMSE, acc_garch$RMSE),
  MAPE  = c(acc_sarima$MAPE, acc_garch$MAPE)
)

# Bootstrap dos intervalos de confiança do SARIMA
sarima_boot <- forecast(m2_train, h = n_test, 
                        level = c(80, 95), 
                        bootstrap = TRUE,
                        npaths = 1000)

autoplot(sarima_boot) +
  autolayer(ts(test$CONSUMO, frequency = 7), series = "Actual") +
  labs(title = "SARIMA - Bootstrap Forecast Intervals",
       y = "Consumption (GWh)", x = "")