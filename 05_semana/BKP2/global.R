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

# Actualización en global.R - Agregar después de las librerías existentes

# Modificar la generación de pronósticos
forecast_periods <- 12  # Cambiado a 12 meses
forecast_ventas_digital <- forecast(fit_ventas_digital, h = forecast_periods)
forecast_ventas_trad <- forecast(fit_ventas_trad, h = forecast_periods)

# Preparar datos para pronósticos extendidos
future_dates <- seq(max(historical$Fecha), by = "month", length.out = forecast_periods + 1)[-1]

forecast_data <- data.frame(
  Fecha = rep(future_dates, 2),
  Canal = rep(c("Digital", "Tradicional"), each = forecast_periods),
  Ventas_Forecast = c(forecast_ventas_digital$mean, forecast_ventas_trad$mean),
  Ventas_Lower = c(forecast_ventas_digital$lower[,"95%"], forecast_ventas_trad$lower[,"95%"]),
  Ventas_Upper = c(forecast_ventas_digital$upper[,"95%"], forecast_ventas_trad$upper[,"95%"])
)

# Agregar función para calcular métricas de pronóstico
calculate_forecast_metrics <- function(model, actual_data) {
  forecast_metrics <- accuracy(model)
  residuals <- residuals(model)
  
  return(list(
    accuracy_metrics = forecast_metrics,
    residuals = residuals,
    ljung_box = Box.test(residuals, lag = 12, type = "Ljung-Box"),
    seasonal_strength = if(frequency(model$x) > 1) {
      decompose(model$x)$seasonal |> abs() |> mean(na.rm = TRUE)
    } else {
      NA
    }
  ))
}


# Cargar y preparar datos
historical <- read.csv("Deutsche_Bahn_Metrics.csv")
historical$Fecha <- as.Date(historical$Fecha)

# Separar datos por canal
digital_data <- historical[historical$Canal == "Digital", ]
traditional_data <- historical[historical$Canal == "Tradicional", ]

# Crear modelos ARIMA para pronósticos
fit_ventas_digital <- auto.arima(digital_data$Ventas)
fit_ventas_trad <- auto.arima(traditional_data$Ventas)

# Generar pronósticos
forecast_periods <- 6
forecast_ventas_digital <- forecast(fit_ventas_digital, h = forecast_periods)
forecast_ventas_trad <- forecast(fit_ventas_trad, h = forecast_periods)

# Preparar datos para pronósticos
future_dates <- seq(max(historical$Fecha), by = "month", length.out = forecast_periods + 1)[-1]

forecast_data <- data.frame(
  Fecha = rep(future_dates, 2),
  Canal = rep(c("Digital", "Tradicional"), each = forecast_periods),
  Ventas_Forecast = c(forecast_ventas_digital$mean, forecast_ventas_trad$mean),
  Ventas_Lower = c(forecast_ventas_digital$lower[,"95%"], forecast_ventas_trad$lower[,"95%"]),
  Ventas_Upper = c(forecast_ventas_digital$upper[,"95%"], forecast_ventas_trad$upper[,"95%"])
)

# Calcular KPIs
calculate_kpis <- function(data) {
  list(
    ventas_promedio = mean(data$Ventas),
    satisfaccion_promedio = mean(data$Satisfacción),
    proyectos_total = sum(data$Proyectos),
    roi_capacitacion = cor(data$Capacitación, data$Ventas),
    indice_innovacion = mean(data$Innovación)
  )
}

# Preparar matriz de correlación
correlation_vars <- c("Ventas", "Satisfacción", "Innovación", "Capacitación", "Liderazgo")
correlation_matrix <- cor(historical[correlation_vars])

