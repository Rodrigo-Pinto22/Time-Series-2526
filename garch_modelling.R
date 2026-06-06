library(mvtnorm)
library(rugarch)
library(FinTS)
library(fGarch)

m2 <- Arima(consumo_ts, order = c(1,1,1), seasonal = c(1,1,1))
summary(m2)

ArchTest(residuals(m2), lags = 12)

df_clean <- df %>% filter(DATE != as.Date("2025-04-28"))
consumo_ts_clean <- ts(df_clean$CONSUMO, frequency = 7)
m2_clean <- Arima(consumo_ts_clean, order = c(1,1,1), seasonal = c(1,1,1))

garch_spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
  distribution.model = "std"
)

garch_fit <- ugarchfit(spec = garch_spec, data = residuals(m2_clean))
garch_fit

garch_spec_gjr <- ugarchspec(
  variance.model = list(model = "gjrGARCH", garchOrder = c(1,1)),
  mean.model = list(armaOrder = c(0,0), include.mean = FALSE),
  distribution.model = "std"
)

garch_spec_gjr_fit <- ugarchfit(spec = garch_spec_gjr, data = residuals(m2_clean))
garch_spec_gjr_fit

garch_fit_egarch <- garchFit(
  formula = ~ aparch(1,1),
  data = residuals(m2_clean),
  cond.dist = "std",
  include.mean = FALSE,
  trace = FALSE
)

summary(garch_fit_egarch)

summary(m2_clean)
coeftest(m2_clean)
checkresiduals(m2_clean)
Box.test(residuals(m2_clean), lag = 14, type = "Ljung-Box")
Box.test(residuals(m2_clean), lag = 20, type = "Ljung-Box")

png("figs/sarima_diagram.png", width = 800, height = 600)
sarima(df_clean$CONSUMO, p=1, d=1, q=1, P=1, D=1, Q=1, S=7)
dev.off()

ArchTest(residuals(m2_clean), lags = 12)

gjr_fit_clean <- ugarchfit(spec = garch_spec_gjr, data = residuals(m2_clean))

png("figs/gjr_diag.png", width = 800, height = 800)
par(mfrow = c(2,2))
plot(gjr_fit_clean, which = 1)
plot(gjr_fit_clean, which = 11)
plot(gjr_fit_clean, which = 9)
plot(gjr_fit_clean, which = 12)
dev.off()
