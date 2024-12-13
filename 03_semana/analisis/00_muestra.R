
library(plotly)
library(tidyverse)

Dashboard_Cultura_Liderazgo_FCC <- read_csv("03_semana/datos/Dashboard_Cultura_Liderazgo_FCC.csv")

Dashboard_Cultura_Liderazgo_FCC$Compromiso_Empleados <- Dashboard_Cultura_Liderazgo_FCC$Compromiso_Empleados /100
Dashboard_Cultura_Liderazgo_FCC$Participacion_Capacitacion <- Dashboard_Cultura_Liderazgo_FCC$Participacion_Capacitacion / 100
Dashboard_Cultura_Liderazgo_FCC$Proyectos_Innovacion <- Dashboard_Cultura_Liderazgo_FCC$Proyectos_Innovacion / 100
Dashboard_Cultura_Liderazgo_FCC$Eficacia_Liderazgo <- Dashboard_Cultura_Liderazgo_FCC$Eficacia_Liderazgo /100
Dashboard_Cultura_Liderazgo_FCC$Satisfaccion_Empleados <- Dashboard_Cultura_Liderazgo_FCC$Satisfaccion_Empleados /100
Dashboard_Cultura_Liderazgo_FCC$Colaboracion_Interdepartamental <- Dashboard_Cultura_Liderazgo_FCC$Colaboracion_Interdepartamental / 100

datos_cultura_liderazgo <- Dashboard_Cultura_Liderazgo_FCC

write.csv(Dashboard_Cultura_Liderazgo_FCC, '03_semana/datos/Dashboard_Cultura_Liderazgo_FCC.csv')


# 1. Bubble Plot para Compromiso de Empleados
fig1 <- plot_ly(
  data = datos_cultura_liderazgo,
  x = ~Fecha,
  y = ~Compromiso_Empleados,
  size = ~Compromiso_Empleados,
  type = 'scatter',
  mode = 'markers',
  marker = list(
    sizemode = 'diameter',
    color = ~Compromiso_Empleados,
    colorscale = 'Viridis',
    showscale = TRUE,
    opacity = 0.7
  )
) %>%
  layout(
    title = "Compromiso de Empleados en el Tiempo",
    xaxis = list(title = "Fecha"),
    yaxis = list(title = "Nivel de Compromiso"),
    showlegend = FALSE
  )

# 2. Heatmap para Participación en Capacitación
# Primero creamos una matriz para el heatmap
matrix_data <- matrix(
  datos_cultura_liderazgo$Participacion_Capacitacion,
  nrow = length(unique(format(datos_cultura_liderazgo$Fecha, "%Y"))),
  byrow = TRUE
)

fig2 <- plot_ly(
  z = ~matrix_data,
  type = "heatmap",
  colorscale = "Viridis"
) %>%
  layout(
    title = "Mapa de Calor - Participación en Capacitación",
    xaxis = list(title = "Semanas"),
    yaxis = list(title = "Años"),
    showlegend = TRUE
  )

# 3. Box Plot para Proyectos de Innovación por año
datos_cultura_liderazgo$Año <- format(datos_cultura_liderazgo$Fecha, "%Y")

fig3 <- plot_ly(
  data = datos_cultura_liderazgo,
  y = ~Proyectos_Innovacion,
  x = ~Año,
  type = "box",
  color = ~Año,
  colors = "Viridis"
) %>%
  layout(
    title = "Distribución de Proyectos de Innovación por Año",
    xaxis = list(title = "Año"),
    yaxis = list(title = "Número de Proyectos")
  )

# 4. Scatter Plot con línea de tendencia para Eficacia de Liderazgo
fig4 <- plot_ly(
  data = datos_cultura_liderazgo,
  x = ~Fecha,
  y = ~Eficacia_Liderazgo,
  type = 'scatter',
  mode = 'markers',
  marker = list(
    color = ~Eficacia_Liderazgo,
    colorscale = 'Viridis',
    showscale = TRUE,
    size = 10
  )
) %>%
  add_lines(
    y = ~fitted(loess(Eficacia_Liderazgo ~ as.numeric(Fecha))),
    line = list(color = 'red', width = 2),
    name = 'Tendencia'
  ) %>%
  layout(
    title = "Eficacia de Liderazgo con Línea de Tendencia",
    xaxis = list(title = "Fecha"),
    yaxis = list(title = "Índice de Eficacia")
  )

# 5. Histograma para Satisfacción de Empleados
fig5 <- plot_ly(
  data = datos_cultura_liderazgo,
  x = ~Satisfaccion_Empleados,
  type = "histogram",
  nbinsx = 30,
  marker = list(
    color = "rgba(31, 119, 180, 0.6)",
    line = list(color = "white", width = 1)
  )
) %>%
  layout(
    title = "Distribución de la Satisfacción de Empleados",
    xaxis = list(title = "Nivel de Satisfacción"),
    yaxis = list(title = "Frecuencia"),
    bargap = 0.1
  )

# 6. Bubble Plot con animación para Colaboración Interdepartamental
fig6 <- plot_ly(
  data = datos_cultura_liderazgo,
  x = ~Eficacia_Liderazgo,
  y = ~Colaboracion_Interdepartamental,
  size = ~Compromiso_Empleados,
  color = ~Satisfaccion_Empleados,
  type = 'scatter',
  mode = 'markers',
  marker = list(
    sizemode = 'diameter',
    opacity = 0.7,
    colorscale = 'Viridis',
    showscale = TRUE
  ),
  text = ~paste("Fecha:", Fecha)
) %>%
  layout(
    title = "Relación entre Colaboración, Eficacia y Satisfacción",
    xaxis = list(title = "Eficacia de Liderazgo"),
    yaxis = list(title = "Colaboración Interdepartamental"),
    showlegend = FALSE
  )

# Mostrar los gráficos
fig1
fig2
fig3
fig4
fig5
fig6
