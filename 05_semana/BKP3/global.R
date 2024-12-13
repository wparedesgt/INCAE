
# global.R
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(plotly)
library(highcharter)
library(dplyr)
library(tidyr)
library(forecast)
library(DT)
library(scales)
library(zoo)
library(corrplot)
library(reshape2)
library(tseries)
library(lubridate)

# Cargar y preparar datos
historical <- read.csv("Deutsche_Bahn_Metrics.csv")
historical$Fecha <- as.Date(historical$Fecha)

# Separar datos por canal
digital_data <- historical[historical$Canal == "Digital", ]
traditional_data <- historical[historical$Canal == "Tradicional", ]

# Preparar series temporales para ARIMA
ts_ventas_digital <- ts(digital_data$Ventas, frequency = 12, 
                        start = c(year(min(digital_data$Fecha)), month(min(digital_data$Fecha))))
ts_ventas_trad <- ts(traditional_data$Ventas, frequency = 12,
                     start = c(year(min(traditional_data$Fecha)), month(min(traditional_data$Fecha))))

# Ajustar modelos ARIMA
fit_ventas_digital <- auto.arima(ts_ventas_digital,
                                 stepwise = FALSE,
                                 approximation = FALSE,
                                 trace = TRUE,
                                 seasonal = TRUE)

fit_ventas_trad <- auto.arima(ts_ventas_trad,
                              stepwise = FALSE,
                              approximation = FALSE,
                              trace = TRUE,
                              seasonal = TRUE)

# Generar pronósticos
forecast_periods <- 12
forecast_ventas_digital <- forecast(fit_ventas_digital, h = forecast_periods, level = c(80, 95))
forecast_ventas_trad <- forecast(fit_ventas_trad, h = forecast_periods, level = c(80, 95))

# Preparar datos de pronóstico
future_dates <- seq(max(historical$Fecha), by = "month", length.out = forecast_periods + 1)[-1]

forecast_data <- data.frame(
  Fecha = rep(future_dates, 2),
  Canal = rep(c("Digital", "Tradicional"), each = forecast_periods),
  Ventas_Forecast = c(forecast_ventas_digital$mean, forecast_ventas_trad$mean),
  Ventas_Lower_95 = c(forecast_ventas_digital$lower[,"95%"], forecast_ventas_trad$lower[,"95%"]),
  Ventas_Upper_95 = c(forecast_ventas_digital$upper[,"95%"], forecast_ventas_trad$upper[,"95%"]),
  Ventas_Lower_80 = c(forecast_ventas_digital$lower[,"80%"], forecast_ventas_trad$lower[,"80%"]),
  Ventas_Upper_80 = c(forecast_ventas_digital$upper[,"80%"], forecast_ventas_trad$upper[,"80%"])
)

# Funciones de diagnóstico y métricas
calculate_arima_diagnostics <- function(model, actual_data) {
  residuals <- residuals(model)
  fitted_values <- fitted(model)
  
  list(
    residuals = residuals,
    fitted = fitted_values,
    ljung_box = Box.test(residuals, lag = 12, type = "Ljung-Box"),
    shapiro = shapiro.test(residuals),
    aic = AIC(model),
    bic = BIC(model),
    rmse = sqrt(mean(residuals^2)),
    mape = mean(abs((actual_data - fitted_values) / actual_data)) * 100
  )
}

# Calcular diagnósticos
diag_digital <- calculate_arima_diagnostics(fit_ventas_digital, digital_data$Ventas)
diag_trad <- calculate_arima_diagnostics(fit_ventas_trad, traditional_data$Ventas)
