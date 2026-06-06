library(forecast)
library(astsa)
library(lmtest)
library(tseries)

consumo_ts <- ts(df$CONSUMO, frequency = 7)

ggtsdisplay(consumo_ts, series = "Energia")

adf_test <- adf.test(consumo_ts)
print(adf_test)

kpss_result <- kpss.test(consumo_ts)
print(kpss_result)

m1 <- Arima(consumo_ts, order = c(1,1,0), seasonal = c(1,1,0))
m2 <- Arima(consumo_ts, order = c(1,1,0), seasonal = c(2,1,0))
m3 <- Arima(consumo_ts, order = c(2,1,0), seasonal = c(1,1,0))
m4 <- Arima(consumo_ts, order = c(2,1,0), seasonal = c(2,1,0))

data.frame(
  Model = c("SARIMA(1,1,0)(1,1,0)[7]",
            "SARIMA(1,1,0)(2,1,0)[7]",
            "SARIMA(2,1,0)(1,1,0)[7]",
            "SARIMA(2,1,0)(2,1,0)[7]"),
  AIC = c(AIC(m1), AIC(m2), AIC(m3), AIC(m4)),
  BIC = c(BIC(m1), BIC(m2), BIC(m3), BIC(m4))
)

checkresiduals(m4)

m5 <- Arima(consumo_ts, order = c(2,1,0), seasonal = c(2,1,1))
m6 <- Arima(consumo_ts, order = c(2,1,1), seasonal = c(2,1,0))
m7 <- Arima(consumo_ts, order = c(2,1,1), seasonal = c(2,1,1))

data.frame(
  Model = c("SARIMA(2,1,0)(2,1,1)[7]",
            "SARIMA(2,1,1)(2,1,0)[7]",
            "SARIMA(2,1,1)(2,1,1)[7]"),
  AIC = c(AIC(m5), AIC(m6), AIC(m7)),
  BIC = c(BIC(m5), BIC(m6), BIC(m7))
)

coeftest(m7)
checkresiduals(m7)

m8 <- Arima(consumo_ts, order = c(1,1,1), seasonal = c(2,1,1))
m9 <- Arima(consumo_ts, order = c(1,1,1), seasonal = c(1,1,1))

data.frame(
  Model = c("SARIMA(1,1,1)(2,1,1)[7]", "SARIMA(1,1,1)(1,1,1)[7]"),
  AIC = c(AIC(m8), AIC(m9)),
  BIC = c(BIC(m8), BIC(m9))
)

coeftest(m9)
checkresiduals(m9)

cov2cor(vcov(m9))
