# server.R
server <- function(input, output) {
  
  # Value Boxes
  output$vbox_ventas_total <- renderValueBox({
    valueBox(
      paste0(round(mean(historical$Ventas), 1), "K €"),
      "Ventas Promedio",
      icon = icon("euro-sign"),
      color = "blue"
    )
  })
  
  output$vbox_satisfaccion <- renderValueBox({
    valueBox(
      paste0(round(mean(historical$Satisfacción), 1), "%"),
      "Satisfacción Cliente",
      icon = icon("smile"),
      color = "green"
    )
  })
  
  output$vbox_proyectos <- renderValueBox({
    valueBox(
      sum(historical$Proyectos),
      "Proyectos Totales",
      icon = icon("project-diagram"),
      color = "yellow"
    )
  })
  
  output$vbox_innovacion <- renderValueBox({
    valueBox(
      round(mean(historical$Innovación), 1),
      "Índice Innovación",
      icon = icon("lightbulb"),
      color = "red"
    )
  })
  
  # Gráficos principales
  output$ventas_trend <- renderHighchart({
    hchart(historical, "line", hcaes(x = Fecha, y = Ventas, group = Canal)) %>%
      hc_title(text = "Evolución de Ventas por Canal") %>%
      hc_xAxis(title = list(text = "Fecha")) %>%
      hc_yAxis(title = list(text = "Ventas (K €)")) %>%
      hc_tooltip(shared = TRUE, crosshairs = TRUE)
  })
  
  output$satisfaction_training <- renderPlotly({
    plot_ly(historical, x = ~Capacitación, y = ~Satisfacción, color = ~Canal,
            type = "scatter", mode = "markers") %>%
      add_lines(x = ~Capacitación, y = fitted(lm(Satisfacción ~ Capacitación, data = historical)),
                line = list(color = "gray", dash = "dash")) %>%
      layout(title = "Relación Satisfacción vs Capacitación",
             xaxis = list(title = "Horas de Capacitación"),
             yaxis = list(title = "Satisfacción (%)"))
  })
  
  # Distribución de Proyectos
  output$projects_dist <- renderPlotly({
    proj_summary <- historical %>%
      group_by(Canal) %>%
      summarise(
        Total_Proyectos = sum(Proyectos),
        Promedio_Mensual = mean(Proyectos)
      )
    
    plot_ly() %>%
      add_bars(data = proj_summary, x = ~Canal, y = ~Total_Proyectos, 
               name = "Total Proyectos", marker = list(color = "#1f77b4")) %>%
      add_trace(y = ~Promedio_Mensual, name = "Promedio Mensual", 
                type = "scatter", mode = "lines+markers", 
                line = list(color = "#ff7f0e")) %>%
      layout(
        title = "Distribución de Proyectos por Canal",
        xaxis = list(title = "Canal"),
        yaxis = list(title = "Número de Proyectos"),
        barmode = "group"
      )
  })
  
  # Índice de Innovación
  output$innovation_index <- renderPlotly({
    innovation_trend <- historical %>%
      group_by(Fecha, Canal) %>%
      summarise(Innovación_Media = mean(Innovación)) %>%
      ungroup()
    
    plot_ly(innovation_trend, x = ~Fecha) %>%
      add_lines(y = ~Innovación_Media, color = ~Canal, 
                line = list(width = 3)) %>%
      layout(
        title = "Evolución del Índice de Innovación",
        xaxis = list(title = "Fecha"),
        yaxis = list(title = "Índice de Innovación"),
        showlegend = TRUE
      )
  })
  
  # Análisis Comparativo de Ventas
  output$ventas_comparison <- renderHighchart({
    ventas_summary <- historical %>%
      group_by(Canal) %>%
      summarise(
        Ventas_Promedio = mean(Ventas),
        Ventas_Min = min(Ventas),
        Ventas_Max = max(Ventas)
      )
    
    highchart() %>%
      hc_chart(type = "column") %>%
      hc_title(text = "Análisis Comparativo de Ventas por Canal") %>%
      hc_xAxis(categories = ventas_summary$Canal) %>%
      hc_yAxis(title = list(text = "Ventas (K €)")) %>%
      hc_add_series(
        name = "Promedio",
        data = ventas_summary$Ventas_Promedio,
        color = "#2ecc71"
      ) %>%
      hc_add_series(
        name = "Mínimo",
        data = ventas_summary$Ventas_Min,
        color = "#e74c3c"
      ) %>%
      hc_add_series(
        name = "Máximo",
        data = ventas_summary$Ventas_Max,
        color = "#3498db"
      ) %>%
      hc_tooltip(shared = TRUE)
  })
  
  # Matriz de Correlación
  output$correlation_matrix <- renderPlotly({
    corr_matrix <- cor(historical[, correlation_vars])
    melted_corr <- melt(corr_matrix)
    
    plot_ly(
      x = melted_corr[,1],
      y = melted_corr[,2],
      z = melted_corr[,3],
      type = "heatmap",
      colorscale = "Viridis"
    ) %>%
      layout(
        title = "Matriz de Correlación de Variables Clave",
        xaxis = list(title = ""),
        yaxis = list(title = "")
      )
  })
  
  # Pronóstico de ventas
  output$sales_forecast <- renderHighchart({
    forecast_data_combined <- rbind(
      historical %>% select(Fecha, Canal, Ventas),
      forecast_data %>% select(Fecha, Canal, Ventas = Ventas_Forecast)
    )
    
    hchart(forecast_data_combined, "line", hcaes(x = Fecha, y = Ventas, group = Canal)) %>%
      hc_title(text = "Pronóstico de Ventas por Canal") %>%
      hc_xAxis(title = list(text = "Fecha")) %>%
      hc_yAxis(title = list(text = "Ventas (K €)")) %>%
      hc_add_theme(hc_theme_google()) %>%
      hc_tooltip(shared = TRUE, crosshairs = TRUE)
  })
  
  # Métricas de precisión del modelo
  output$forecast_metrics <- renderDT({
    metrics_digital <- accuracy(forecast_ventas_digital)
    metrics_trad <- accuracy(forecast_ventas_trad)
    
    metrics_df <- data.frame(
      Canal = c("Digital", "Tradicional"),
      RMSE = c(metrics_digital[1,"RMSE"], metrics_trad[1,"RMSE"]),
      MAE = c(metrics_digital[1,"MAE"], metrics_trad[1,"MAE"]),
      MAPE = c(metrics_digital[1,"MAPE"], metrics_trad[1,"MAPE"])
    )
    
    datatable(metrics_df, options = list(dom = 't'))
  })
  
  # Tabla de KPIs
  output$kpi_table <- renderDT({
    kpis_digital <- calculate_kpis(digital_data)
    kpis_trad <- calculate_kpis(traditional_data)
    
    kpi_df <- data.frame(
      Métrica = c("Ventas Promedio (K €)", "Satisfacción (%)", "Proyectos Totales",
                  "ROI Capacitación", "Índice Innovación"),
      Digital = c(
        round(kpis_digital$ventas_promedio, 2),
        round(kpis_digital$satisfaccion_promedio, 2),
        kpis_digital$proyectos_total,
        round(kpis_digital$roi_capacitacion, 3),
        round(kpis_digital$indice_innovacion, 2)
      ),
      Tradicional = c(
        round(kpis_trad$ventas_promedio, 2),
        round(kpis_trad$satisfaccion_promedio, 2),
        kpis_trad$proyectos_total,
        round(kpis_trad$roi_capacitacion, 3),
        round(kpis_trad$indice_innovacion, 2)
      )
    )
    
    datatable(kpi_df, options = list(dom = 't'))
  })
  
  
  # Actualización en server.R - Agregar nuevas funciones de renderizado
  
  # Pronóstico extendido de ventas
  output$sales_forecast_extended <- renderHighchart({
    # Combinar datos históricos y pronósticos
    forecast_data_combined <- rbind(
      historical %>% select(Fecha, Canal, Ventas),
      forecast_data %>% select(Fecha, Canal, Ventas = Ventas_Forecast)
    )
    
    # Crear gráfico con bandas de confianza
    hc <- highchart() %>%
      hc_title(text = "Pronóstico de Ventas a 12 Meses") %>%
      hc_xAxis(type = "datetime", title = list(text = "Fecha")) %>%
      hc_yAxis(title = list(text = "Ventas (K €)"))
    
    # Agregar datos históricos
    for(canal in unique(historical$Canal)) {
      hist_data <- historical[historical$Canal == canal, ]
      hc <- hc %>%
        hc_add_series(
          data = hist_data,
          type = "line",
          hcaes(x = Fecha, y = Ventas),
          name = paste("Histórico -", canal),
          id = canal
        )
    }
    
    # Agregar pronósticos
    for(canal in unique(forecast_data$Canal)) {
      forecast_canal <- forecast_data[forecast_data$Canal == canal, ]
      hc <- hc %>%
        hc_add_series(
          data = forecast_canal,
          type = "line",
          hcaes(x = Fecha, y = Ventas_Forecast),
          name = paste("Pronóstico -", canal),
          dashStyle = "Dash",
          linkedTo = canal
        ) %>%
        hc_add_series(
          data = forecast_canal,
          type = "arearange",
          hcaes(x = Fecha, low = Ventas_Lower, high = Ventas_Upper),
          name = paste("Intervalo de Confianza -", canal),
          linkedTo = canal,
          fillOpacity = 0.3,
          zIndex = -1
        )
    }
    
    hc %>%
      hc_tooltip(shared = TRUE) %>%
      hc_legend(enabled = TRUE)
  })
  
  # Bandas de confianza
  output$confidence_bands <- renderPlotly({
    plot_ly() %>%
      add_ribbons(
        data = forecast_data[forecast_data$Canal == "Digital",],
        x = ~Fecha,
        ymin = ~Ventas_Lower,
        ymax = ~Ventas_Upper,
        name = "IC Digital",
        fillcolor = "rgba(0, 100, 255, 0.2)"
      ) %>%
      add_ribbons(
        data = forecast_data[forecast_data$Canal == "Tradicional",],
        x = ~Fecha,
        ymin = ~Ventas_Lower,
        ymax = ~Ventas_Upper,
        name = "IC Tradicional",
        fillcolor = "rgba(255, 100, 0, 0.2)"
      ) %>%
      add_lines(
        data = forecast_data[forecast_data$Canal == "Digital",],
        x = ~Fecha,
        y = ~Ventas_Forecast,
        name = "Pronóstico Digital",
        line = list(color = "blue")
      ) %>%
      add_lines(
        data = forecast_data[forecast_data$Canal == "Tradicional",],
        x = ~Fecha,
        y = ~Ventas_Forecast,
        name = "Pronóstico Tradicional",
        line = list(color = "orange")
      ) %>%
      layout(
        title = "Bandas de Confianza del Pronóstico",
        xaxis = list(title = "Fecha"),
        yaxis = list(title = "Ventas (K €)")
      )
  })
  
  # Diagnóstico del modelo
  output$model_diagnostics <- renderPlotly({
    # Calcular residuos para ambos canales
    residuals_digital <- residuals(fit_ventas_digital)
    residuals_trad <- residuals(fit_ventas_trad)
    
    plot_ly() %>%
      add_boxplot(
        y = ~residuals_digital,
        name = "Digital",
        boxpoints = "all",
        jitter = 0.3,
        pointpos = -1.8
      ) %>%
      add_boxplot(
        y = ~residuals_trad,
        name = "Tradicional",
        boxpoints = "all",
        jitter = 0.3,
        pointpos = -1.8
      ) %>%
      layout(
        title = "Distribución de Residuos por Canal",
        yaxis = list(title = "Residuos"),
        showlegend = TRUE
      )
  })
  
  # Métricas de precisión extendidas
  output$forecast_metrics_extended <- renderDT({
    # Calcular métricas para ambos canales
    metrics_digital <- calculate_forecast_metrics(fit_ventas_digital, digital_data$Ventas)
    metrics_trad <- calculate_forecast_metrics(fit_ventas_trad, traditional_data$Ventas)
    
    metrics_df <- data.frame(
      Canal = c("Digital", "Tradicional"),
      RMSE = c(
        metrics_digital$accuracy_metrics[1,"RMSE"],
        metrics_trad$accuracy_metrics[1,"RMSE"]
      ),
      MAE = c(
        metrics_digital$accuracy_metrics[1,"MAE"],
        metrics_trad$accuracy_metrics[1,"MAE"]
      ),
      MAPE = c(
        metrics_digital$accuracy_metrics[1,"MAPE"],
        metrics_trad$accuracy_metrics[1,"MAPE"]
      ),
      Ljung_Box_p = c(
        metrics_digital$ljung_box$p.value,
        metrics_trad$ljung_box$p.value
      ),
      Seasonal_Strength = c(
        metrics_digital$seasonal_strength,
        metrics_trad$seasonal_strength
      )
    )
    
    datatable(
      metrics_df,
      options = list(
        dom = 't',
        pageLength = 5
      ),
      rownames = FALSE
    ) %>%
      formatRound(columns = 2:6, digits = 3)
  })
  
  
}