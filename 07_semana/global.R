# global.R
# Librerías necesarias
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(plotly)
library(dplyr)
library(DT)
library(highcharter)
library(RColorBrewer)
library(tidyr)
library(lubridate)

# Datos simulados
set.seed(123)
data <- data.frame(
  Fecha = sample(seq.Date(as.Date("2019-01-01"), as.Date("2021-12-31"), by = "day"), 900, replace = TRUE),
  Meta = rep(c(
    "Identificar variaciones críticas",
    "Optimizar uso de plástico",
    "Explorar asociaciones estratégicas",
    "Implementar gemelo digital",
    "Incrementar calidad del producto",
    "Reducir desperdicio",
    "Marco flexible de adopción tecnológica",
    "Consolidar conocimientos compartidos",
    "Expandir asociaciones estratégicas"
  ), each = 100),
  Indicador = rep(c(
    "Porcentaje de variaciones detectadas",
    "Toneladas de plástico ahorradas",
    "Número de startups evaluadas",
    "Porcentaje de líneas digitalizadas",
    "Tasa de desperdicio (%)",
    "Número de empleados capacitados",
    "Tasa de adopción tecnológica (%)",
    "Progreso en documentación de prácticas (%)",
    "Acuerdos con centros de innovación"
  ), each = 100),
  Valor = round(runif(900, 10, 100), 2),
  Planta = ifelse(rep(c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE), each = 100),
                  paste0("Planta_", sample(1:9, 900, replace = TRUE)), NA),
  Colaboradores = ifelse(rep(c(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE), each = 100),
                         sample(1:5, 900, replace = TRUE), NA),
  Empleado_capacitado = ifelse(rep(c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE), each = 100),
                               round(runif(900, 50, 100), 2), NA),
  Notas = sample(c("Acción prioritaria", "En progreso", "Sin problemas"), 900, replace = TRUE)
)

# Funciones de análisis
generate_trend_analysis <- function(data) {
  ultimo_valor <- tail(data$Valor_Promedio, 1)
  primer_valor <- head(data$Valor_Promedio, 1)
  cambio_porcentual <- ((ultimo_valor - primer_valor) / primer_valor) * 100
  
  tendencia <- if(cambio_porcentual > 0) "creciente" else if(cambio_porcentual < 0) "decreciente" else "estable"
  
  sprintf("La tendencia general es %s con un cambio del %.1f%% desde el inicio del período. 
          El último valor registrado fue %.1f.", 
          tendencia, abs(cambio_porcentual), ultimo_valor)
}

generate_distribution_analysis <- function(data) {
  resumen <- summary(data$Valor)
  variabilidad <- sd(data$Valor, na.rm = TRUE)
  
  sprintf("La mediana es %.1f con un rango intercuartil de %.1f a %.1f. 
          La desviación estándar es %.1f, indicando %s variabilidad en los datos.", 
          resumen["Median"], resumen["1st Qu."], resumen["3rd Qu."], 
          variabilidad,
          if(variabilidad > 20) "alta" else if(variabilidad > 10) "moderada" else "baja")
}

generate_heatmap_analysis <- function(data) {
  correlacion_max <- which.max(data$Valor_Promedio)
  correlacion_min <- which.min(data$Valor_Promedio)
  
  sprintf("Los valores más altos se observan en %s con %.1f, mientras que los más bajos están en %s con %.1f. 
          Esto sugiere áreas de oportunidad para mejora en los indicadores más bajos.", 
          data$Meta[correlacion_max], data$Valor_Promedio[correlacion_max],
          data$Meta[correlacion_min], data$Valor_Promedio[correlacion_min])
}


# Función para determinar el color del valueBox
get_status_color <- function(valor) {
  if(valor > 80) "green"
  else if(valor > 60) "yellow" 
  else "red"
}



