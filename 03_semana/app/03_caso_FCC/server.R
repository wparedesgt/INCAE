server <- function(input, output) {
  
  datos_cultura_liderazgo$Fecha <- as.Date(datos_cultura_liderazgo$Fecha)
  
  # InfoBoxes
  output$compromiso_box <- renderInfoBox({
    ultimo_compromiso <- tail(datos_cultura_liderazgo$Compromiso_Empleados, 1)
    infoBox(
      "Compromiso",
      paste0(format(ultimo_compromiso * 100, digits = 2), "%"),
      icon = icon("heart"),
      color = "red",
      fill = TRUE
    )
  })
  
  output$capacitacion_box <- renderInfoBox({
    ultima_capacitacion <- tail(datos_cultura_liderazgo$Participacion_Capacitacion, 1)
    infoBox(
      "Participación en Capacitación",
      paste0(format(ultima_capacitacion * 100, digits = 2), "%"),
      icon = icon("book"),
      color = "yellow",
      fill = TRUE
    )
  })
  
  output$innovacion_box <- renderInfoBox({
    ultimos_proyectos <- tail(datos_cultura_liderazgo$Proyectos_Innovacion, 1)
    infoBox(
      "Proyectos de Innovación",
      ultimos_proyectos,
      icon = icon("lightbulb"),
      color = "green",
      fill = TRUE
    )
  })
  
  # # Gráficos existentes mejorados
  # output$plot_compromiso <- renderPlotly({
  #   plot_ly(
  #     data = datos_cultura_liderazgo,
  #     x = ~Fecha,
  #     y = ~Compromiso_Empleados,
  #     type = 'scatter',
  #     mode = 'lines+markers',
  #     marker = list(color = '#1976D2'),
  #     line = list(color = '#1976D2')
  #   ) %>%
  #     layout(
  #       title = "Evolución del Compromiso",
  #       xaxis = list(title = "Fecha"),
  #       yaxis = list(title = "Nivel de Compromiso", tickformat = ".2%")
  #     )
  # })
  
  # 1. Compromiso de Empleados - Bubble Chart
  output$plot_compromiso <- renderPlotly({
    
    
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
  })
  
  output$plot_compromiso_detalle <- renderPlotly({
    plot_ly(
      data = datos_cultura_liderazgo,
      x = ~Fecha,
      y = ~Compromiso_Empleados,
      type = 'scatter',
      mode = 'lines+markers',
      marker = list(
        size = 8,
        color = ~Compromiso_Empleados,
        colorscale = 'Viridis'
      )
    ) %>%
      layout(
        title = "Análisis Detallado del Compromiso",
        xaxis = list(title = "Fecha"),
        yaxis = list(title = "Nivel de Compromiso", tickformat = ".2%")
      )
  })
  
  
  # 2. Participación en Programas de Capacitación - Histogram
  output$plot_capacitacion <- renderPlotly({
    
    
    # 2. Participación en Programas de Capacitación - Histograma mejorado con comentario
    fig_capacitacion <- plot_ly(
      data = datos_cultura_liderazgo,
      x = ~Participacion_Capacitacion,
      type = 'histogram',
      nbinsx = 40,  # Aumentando el número de bins para una mayor claridad
      marker = list(color = 'lightblue')
    ) %>%
      layout(
        title = "Distribución de Participación en Programas de Capacitación",
        xaxis = list(title = "Participación en Capacitación (%)"),
        yaxis = list(title = "Frecuencia"),
        annotations = list(
          list(
            text = "Este histograma muestra la distribución de participación en programas de capacitación, resaltando las frecuencias de distintos niveles de compromiso.",
            xref = "paper", yref = "paper", x = 0.5, y = -0.15, showarrow = FALSE
          )
        )
      )
    
    
    
  })
  
  # 3. Proyectos de Innovación Internos - Bar Chart
  output$plot_innovacion <- renderPlotly({
    plot_ly(
      data = datos_cultura_liderazgo,
      x = ~Fecha,
      y = ~Proyectos_Innovacion,
      type = 'bar',
      marker = list(color = 'orange'),
      name = 'Proyectos de Innovación'
    ) %>% layout(
      title = 'Proyectos de Innovación Internos (Bar Chart)',
      xaxis = list(title = 'Fecha'),
      yaxis = list(title = 'Número de Proyectos de Innovación')
    )
  })
  
  # 4. Eficacia del Liderazgo - Box Plot
  output$plot_liderazgo <- renderPlotly({
    plot_ly(
      data = datos_cultura_liderazgo,
      y = ~Eficacia_Liderazgo,
      type = 'box',
      marker = list(color = 'green'),
      name = 'Eficacia del Liderazgo'
    ) %>% layout(
      title = 'Distribución de Eficacia del Liderazgo (Box Plot)',
      yaxis = list(title = 'Eficacia del Liderazgo (%)', tickformat = ".0%")
    )
  })
  
  # 5. Satisfacción de Empleados - Funnel Chart
  output$plot_satisfaccion <- renderPlotly({
    
    
    # 5. Satisfacción de Empleados - Scatter Plot con comentario
    fig_satisfaccion <- plot_ly(
      data = datos_cultura_liderazgo,
      x = ~Fecha,
      y = ~Satisfaccion_Empleados,
      type = 'scatter',
      mode = 'markers',
      marker = list(color = 'purple')
    ) %>%
      layout(
        title = "Satisfacción de Empleados en el Tiempo",
        xaxis = list(title = "Fecha"),
        yaxis = list(title = "Satisfacción de Empleados (%)"),
        annotations = list(
          list(
            text = "El gráfico de dispersión permite observar la variabilidad de la satisfacción de empleados en el tiempo, identificando patrones de mejora o descenso.",
            xref = "paper", yref = "paper", x = 0.5, y = -0.15, showarrow = FALSE
          )
        )
      )
    
  })
  
  # 6. Colaboración Interdepartamental - Network Graph (símil)
  output$plot_colaboracion <- renderPlotly({
    
    
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
    
    
  })
}