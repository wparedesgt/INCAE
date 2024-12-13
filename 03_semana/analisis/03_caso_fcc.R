
library(tidyverse)
library(readr)
library(plotly)

Dashboard_Cultura_Liderazgo_FCC <- read_csv("03_semana/datos/Dashboard_Cultura_Liderazgo_FCC.csv")

Dashboard_Cultura_Liderazgo_FCC$Compromiso_Empleados <- Dashboard_Cultura_Liderazgo_FCC$Compromiso_Empleados /100
Dashboard_Cultura_Liderazgo_FCC$Participacion_Capacitacion <- Dashboard_Cultura_Liderazgo_FCC$Participacion_Capacitacion / 100
Dashboard_Cultura_Liderazgo_FCC$Proyectos_Innovacion <- Dashboard_Cultura_Liderazgo_FCC$Proyectos_Innovacion / 100
Dashboard_Cultura_Liderazgo_FCC$Eficacia_Liderazgo <- Dashboard_Cultura_Liderazgo_FCC$Eficacia_Liderazgo /100
Dashboard_Cultura_Liderazgo_FCC$Satisfaccion_Empleados <- Dashboard_Cultura_Liderazgo_FCC$Satisfaccion_Empleados /100
Dashboard_Cultura_Liderazgo_FCC$Colaboracion_Interdepartamental <- Dashboard_Cultura_Liderazgo_FCC$Colaboracion_Interdepartamental / 100

datos_cultura_liderazgo <- Dashboard_Cultura_Liderazgo_FCC

write.csv(Dashboard_Cultura_Liderazgo_FCC, '03_semana/datos/Dashboard_Cultura_Liderazgo_FCC.csv')
saveRDS(Dashboard_Cultura_Liderazgo_FCC, '03_semana/datos/Dashboard_Cultura_Liderazgo_FCC.rds')



# 1. Bubble Plot con escala de colores correctamente invertida
fig1 <- plot_ly(
  data = datos_cultura_liderazgo,
  x = ~Fecha,
  y = ~Compromiso_Empleados,
  size = ~Compromiso_Empleados * 5000,  # Factor de escala para el tamaño
  type = 'scatter',
  mode = 'markers',
  marker = list(
    sizemode = 'area',
    color = ~Compromiso_Empleados,
    colorscale = 'YlOrRd',    # Escala de amarillo a rojo
    reversescale = TRUE,      # Invertimos la escala para que los valores altos sean más intensos
    showscale = TRUE,
    opacity = 0.8,
    sizeref = 0.1
  ),
  text = ~paste(
    "Fecha:", format(Fecha, "%Y-%m-%d"),
    "<br>Compromiso:", sprintf("%.4f", Compromiso_Empleados)
  ),
  hoverinfo = 'text'
) %>%
  layout(
    title = list(
      text = "Compromiso de Empleados en el Tiempo",
      font = list(size = 18)
    ),
    xaxis = list(
      title = "Fecha",
      gridcolor = '#E2E2E2',
      showgrid = TRUE
    ),
    yaxis = list(
      title = "Nivel de Compromiso",
      gridcolor = '#E2E2E2',
      showgrid = TRUE,
      tickformat = '.4f'
    ),
    paper_bgcolor = 'white',
    plot_bgcolor = 'white',
    showlegend = FALSE
  )

fig1


# Box Plot para Participación en Capacitación por año con tonos verdes
fig2 <- plot_ly(
  data = datos_cultura_liderazgo,
  y = ~Participacion_Capacitacion,
  x = ~Año,
  type = "box",
  color = ~Año,
  colors = c("#00441B", "#006D2C", "#238B45", "#41AB5D", "#74C476", "#A1D99B")  # Paleta de verdes
) %>%
  layout(
    title = list(
      text = "Distribución de Participación en Capacitación por Año",
      font = list(size = 18)
    ),
    xaxis = list(
      title = "Año",
      gridcolor = '#E2E2E2'
    ),
    yaxis = list(
      title = "Nivel de Participación",
      gridcolor = '#E2E2E2',
      tickformat = '.4f'
    ),
    paper_bgcolor = 'white',
    plot_bgcolor = 'white',
    showlegend = FALSE,
    boxmode = "group"
  )

fig2


# Box Plot para Proyectos de Innovación por año
fig3 <- plot_ly(
  data = datos_cultura_liderazgo,
  y = ~Proyectos_Innovacion,
  x = ~Año,
  type = "box",
  color = ~Año,
  colors = c("#08306B", "#08519C", "#2171B5", "#4292C6", "#6BAED6", "#9ECAE1")  # Paleta de azules
) %>%
  layout(
    title = list(
      text = "Distribución de Proyectos de Innovación por Año",
      font = list(size = 18)
    ),
    xaxis = list(
      title = "Año",
      gridcolor = '#E2E2E2'
    ),
    yaxis = list(
      title = "Número de Proyectos",
      gridcolor = '#E2E2E2',
      tickformat = '.4f'
    ),
    paper_bgcolor = 'white',
    plot_bgcolor = 'white',
    showlegend = FALSE,
    boxmode = "group"
  )

fig3

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

fig4

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

# 6. Bubble Plot Multivariable para Colaboración
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
  text = ~paste(
    "Fecha:", Fecha,
    "<br>Colaboración:", round(Colaboracion_Interdepartamental, 4),
    "<br>Eficacia:", round(Eficacia_Liderazgo, 4),
    "<br>Compromiso:", round(Compromiso_Empleados, 4)
  ),
  hoverinfo = 'text'
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

# Visualización con colores invertidos
fig6 <- plot_ly(
  data = datos_cultura_liderazgo,
  x = ~Eficacia_Liderazgo,
  y = ~Colaboracion_Interdepartamental,
  type = 'scatter',
  mode = 'markers',
  marker = list(
    size = 12,
    color = ~Satisfaccion_Empleados,
    colorscale = 'RdYlBu',    # Mantenemos RdYlBu
    reversescale = FALSE,     # Cambiamos a FALSE para invertir los colores
    showscale = TRUE,
    line = list(color = 'black', width = 1),
    colorbar = list(
      title = list(
        text = "Satisfacción",
        font = list(size = 12)
      ),
      nticks = 8,
      tickfont = list(size = 10),
      thickness = 20,
      len = 0.8
    )
  ),
  text = ~paste(
    "Fecha:", format(Fecha, "%Y-%m-%d"),
    "<br>Colaboración:", sprintf("%.4f", Colaboracion_Interdepartamental),
    "<br>Eficacia:", sprintf("%.4f", Eficacia_Liderazgo),
    "<br>Satisfacción:", sprintf("%.4f", Satisfaccion_Empleados)
  ),
  hoverinfo = 'text'
) %>%
  add_lines(
    x = ~Eficacia_Liderazgo,
    y = ~fitted(loess(Colaboracion_Interdepartamental ~ Eficacia_Liderazgo)),
    line = list(color = 'gray', dash = 'dash'),
    showlegend = TRUE,
    name = 'Tendencia'
  ) %>%
  layout(
    title = list(
      text = "Análisis de Relación: Eficacia vs Colaboración",
      font = list(size = 16)
    ),
    xaxis = list(
      title = "Eficacia de Liderazgo",
      gridcolor = '#E2E2E2',
      zeroline = FALSE,
      range = c(min(datos_cultura_liderazgo$Eficacia_Liderazgo) * 0.95,
                max(datos_cultura_liderazgo$Eficacia_Liderazgo) * 1.05)
    ),
    yaxis = list(
      title = "Colaboración Interdepartamental",
      gridcolor = '#E2E2E2',
      zeroline = FALSE,
      range = c(min(datos_cultura_liderazgo$Colaboracion_Interdepartamental) * 0.95,
                max(datos_cultura_liderazgo$Colaboracion_Interdepartamental) * 1.05)
    ),
    paper_bgcolor = 'white',
    plot_bgcolor = 'white',
    showlegend = TRUE,
    legend = list(x = 0.85, y = 0.1)
  )

fig6
