#install.packages("mvtnorm")
library(mvtnorm)
#install.packages("rugarch")
library(rugarch)
#install.packages("FinTS")
library(FinTS)
#install.packages("fGarch")
library(fGarch)



m2 <- Arima(consumo_ts, order = c(1,1,1), seasonal = c(1,1,1))
summary(m2)

ArchTest(residuals(m2), lags = 12)

# Remove the blackout observation and refit SARIMA
df_clean <- df %>% filter(DATE != as.Date("2025-04-28"))
consumo_ts_clean <- ts(df_clean$CONSUMO, frequency = 7)
m2_clean <- Arima(consumo_ts_clean, order=c(1,1,1), seasonal=c(1,1,1))

# Specify GARCH(1,1) with Student-t distribution
garch_spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
  distribution.model = "std"  # Student-t
)

# Fit on SARIMA residuals
garch_fit <- ugarchfit(spec = garch_spec, data = residuals(m2_clean)) #m2

# Summary
garch_fit


# GARCH(1,1) with Student-t distribution fitted on SARIMA(1,1,1)(1,1,1)[7] residuals.
# All parameters significant. alpha1 + beta1 ≈ 0.999 (near-integrated GARCH, high persistence).
# Shape parameter ν ≈ 2.9 confirms heavy tails.
# Sign Bias Test suggests asymmetric response to shocks → EGARCH to be explored.

#Trying this garch to account for the asymmetry:
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


# APARCH(1,1) with Student-t fitted on SARIMA residuals.
# Best model among GARCH(1,1), GJR-GARCH(1,1) and APARCH(1,1) based on AIC/BIC.
# gamma1 = 0.190 (significant) → asymmetric volatility response.
# delta = 1.242 → power transformation closer to absolute value than squared residuals.
# alpha1 + beta1 = 0.952 → high volatility persistence.


#Ljung-Box on R still significant — mean structure inherited from SARIMA
#Ljung-Box on R² still significant — some residual ARCH effects remain

summary(m2_clean)
coeftest(m2_clean)
checkresiduals(m2_clean)
Box.test(residuals(m2_clean), lag = 14, type = "Ljung-Box")
Box.test(residuals(m2_clean), lag = 20, type = "Ljung-Box")
png("figs/sarima_diagram.png", width = 800, height = 600)
sarima(df_clean$CONSUMO, p=1, d=1, q=1, P=1, D=1, Q=1, S=7)
dev.off()


# Ljung-Box rejection is likely driven by large sample size (n≈5833) rather than
# a model misspecification — ACF of residuals shows no practically significant
# autocorrelation. Removing the April 28, 2025 blackout outlier improved all
# error metrics (sigma², RMSE, MAE, MAPE) and residual behaviour.
# m2_clean (outlier removed) is adopted as the final SARIMA model.
# → Retry GARCH (GARCH(1,1), GJR-GARCH(1,1), APARCH(1,1)) on residuals(m2_clean)
# Recheck residuals
ArchTest(residuals(m2_clean), lags = 12)

# Plot de diagnósticos do GJR-GARCH
gjr_fit_clean <- ugarchfit(spec = garch_spec_gjr, data = residuals(m2_clean))

# Plots de diagnóstico
png("figs/gjr_diag.png", width = 800, height = 800)
par(mfrow = c(2,2))
plot(gjr_fit_clean, which = 1)
plot(gjr_fit_clean, which = 11)
plot(gjr_fit_clean, which = 9)
plot(gjr_fit_clean, which = 12)
dev.off()
