## Model SARIMA

library(forecast)

# Create ts object with weekly seasonality
consumo_ts <- ts(df$CONSUMO, frequency = 7)
# with yearly seasonality
#consumo_ts <- ts(df$CONSUMO, start = c(2010, 1), frequency = 365.25)

# Candidate models
m1 <- Arima(consumo_ts, order = c(2,1,1), seasonal = c(1,1,1))
m2 <- Arima(consumo_ts, order = c(1,1,1), seasonal = c(1,1,1))
m3 <- Arima(consumo_ts, order = c(2,1,0), seasonal = c(1,1,1))
m4 <- Arima(consumo_ts, order = c(0,1,1), seasonal = c(0,1,1))

# Compare AIC/BIC
data.frame(
  Model = c("SARIMA(2,1,1)(1,1,1)[7]",
            "SARIMA(1,1,1)(1,1,1)[7]",
            "SARIMA(2,1,0)(1,1,1)[7]",
            "SARIMA(0,1,1)(0,1,1)[7]"),
  AIC = c(AIC(m1), AIC(m2), AIC(m3), AIC(m4)),
  BIC = c(BIC(m1), BIC(m2), BIC(m3), BIC(m4))
)

# auto.arima suggest a model
auto_model <- auto.arima(consumo_ts, stepwise = FALSE, approximation = FALSE)
summary(auto_model)
AIC(auto_model)
BIC(auto_model)

checkresiduals(m2)
Box.test(residuals(m2), lag = 14, type = "Ljung-Box")
Box.test(residuals(m2), lag = 20, type = "Ljung-Box")

checkresiduals(auto_model)
Box.test(residuals(auto_model), lag = 14, type = "Ljung-Box")
Box.test(residuals(auto_model), lag = 20, type = "Ljung-Box")
