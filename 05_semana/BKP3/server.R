# Server
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
  
  # Tendencia de Ventas
  output$ventas_trend <- renderHighchart({
    hchart(historical, "line", hcaes(x = Fecha, y = Ventas, group = Canal)) %>%
      hc_title(text = "Evolución de Ventas por Canal") %>%
      hc_xAxis(title = list(text = "Fecha")) %>%
      hc_yAxis(title = list(text = "Ventas (K €)")) %>%
      hc_tooltip(shared = TRUE, crosshairs = TRUE)
  })
  
  # Matriz de Correlación
  output$correlation_matrix <- renderPlotly({
    corr_vars <- c("Ventas", "Satisfacción", "Innovación", "Capacitación", "Liderazgo")
    corr_matrix <- cor(historical[corr_vars])
    
    plot_ly(
      x = corr_vars,
      y = corr_vars,
      z = corr_matrix,
      type = "heatmap",
      colorscale = "Viridis"
    ) %>%
      layout(
        title = "Correlación entre Variables",
        xaxis = list(title = ""),
        yaxis = list(title = "")
      )
  })
  
  # Diagnóstico ARIMA - Digital
  output$arima_diag_digital <- renderPlotly({
    plot_ly() %>%
      add_trace(
        x = digital_data$Fecha,
        y = diag_digital$residuals,
        type = "scatter",
        mode = "lines",
        name = "Residuos"
      ) %>%
      add_trace(
        x = digital_data$Fecha,
        y = rep(0, length(digital_data$Fecha)),
        type = "scatter",
        mode = "lines",
        line = list(dash = "dash"),
        name = "Línea Base"
      ) %>%
      layout(
        title = "Residuos del Modelo ARIMA - Canal Digital",
        xaxis = list(title = "Fecha"),
        yaxis = list(title = "Residuos")
      )
  })
  
  # Pronóstico de Ventas
  output$sales_forecast <- renderHighchart({
    # Combinar datos históricos y pronósticos
    forecast_data_combined <- rbind(
      historical %>% select(Fecha, Canal, Ventas),
      forecast_data %>% select(Fecha, Canal, Ventas = Ventas_Forecast)
    )
    
    hc <- highchart() %>%
      hc_title(text = "Pronóstico de Ventas a 12 Meses") %>%
      hc_xAxis(type = "datetime", title = list(text = "Fecha")) %>%
      hc_yAxis(title = list(text = "Ventas (K €)"))
    
    # Datos históricos
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
    
    # Pronósticos
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
          hcaes(x = Fecha, low = Ventas_Lower_95, high = Ventas_Upper_95),
          name = paste("IC 95% -", canal),
          linkedTo = canal,
          fillOpacity = 0.3,
          zIndex = -1
        )
    }
    
    hc %>%
      hc_tooltip(shared = TRUE) %>%
      hc_legend(enabled = TRUE)
  })
  
  # Métricas ARIMA
  output$arima_metrics <- renderDT({
    metrics_df <- data.frame(
      Canal = c("Digital", "Tradicional"),
      AIC = c(diag_digital$aic, diag_trad$aic),
      BIC = c(diag_digital$bic, diag_trad$bic),
      RMSE = c(diag_digital$rmse, diag_trad$rmse),
      MAPE = c(diag_digital$mape, diag_trad$mape),
      Ljung_Box_p = c(diag_digital$ljung_box$p.value, diag_trad$ljung_box$p.value),
      Shapiro_p = c(diag_digital$shapiro$p.value, diag_trad$shapiro$p.value)
    )
    
    datatable(metrics_df, options = list(pageLength = 5)) %>%
      formatRound(columns = 2:7, digits = 3)
  })
  
  # KPI Table
  output$kpi_table <- renderDT({
    kpi_data <- historical %>%
      group_by(Canal) %>%
      summarise(
        Ventas_Promedio = mean(Ventas),
        Satisfaccion_Promedio = mean(Satisfacción),
        Proyectos_Totales = sum(Proyectos),
        Innovacion_Promedio = mean(Innovación),
        Capacitacion_Total = sum(Capacitación)
      )
    
    datatable(kpi_data, options = list(pageLength = 5)) %>%
      formatRound(columns = 2:5, digits = 2)
  })
  
  
  # Agregar estos renderizadores al servidor (server.R)
  
  # Distribución de Ventas
  output$ventas_dist <- renderPlotly({
    plot_ly() %>%
      add_histogram(data = historical[historical$Canal == "Digital",], 
                    x = ~Ventas, 
                    name = "Digital",
                    nbinsx = 30,
                    alpha = 0.6) %>%
      add_histogram(data = historical[historical$Canal == "Tradicional",], 
                    x = ~Ventas, 
                    name = "Tradicional",
                    nbinsx = 30,
                    alpha = 0.6) %>%
      layout(
        title = "Distribución de Ventas por Canal",
        xaxis = list(title = "Ventas (K €)"),
        yaxis = list(title = "Frecuencia"),
        barmode = "overlay"
      )
  })
  
  # Diagnóstico ARIMA - Canal Tradicional
  output$arima_diag_trad <- renderPlotly({
    plot_ly() %>%
      add_trace(
        x = traditional_data$Fecha,
        y = diag_trad$residuals,
        type = "scatter",
        mode = "lines",
        name = "Residuos"
      ) %>%
      add_trace(
        x = traditional_data$Fecha,
        y = rep(0, length(traditional_data$Fecha)),
        type = "scatter",
        mode = "lines",
        line = list(dash = "dash"),
        name = "Línea Base"
      ) %>%
      layout(
        title = "Residuos del Modelo ARIMA - Canal Tradicional",
        xaxis = list(title = "Fecha"),
        yaxis = list(title = "Residuos")
      )
  })
  
  # Intervalos de Confianza
  output$confidence_intervals <- renderPlotly({
    plot_ly() %>%
      # Intervalo 95% - Digital
      add_ribbons(
        data = forecast_data[forecast_data$Canal == "Digital",],
        x = ~Fecha,
        ymin = ~Ventas_Lower_95,
        ymax = ~Ventas_Upper_95,
        name = "IC 95% Digital",
        fillcolor = "rgba(0,100,255,0.2)",
        line = list(color = "transparent")
      ) %>%
      # Intervalo 80% - Digital
      add_ribbons(
        data = forecast_data[forecast_data$Canal == "Digital",],
        x = ~Fecha,
        ymin = ~Ventas_Lower_80,
        ymax = ~Ventas_Upper_80,
        name = "IC 80% Digital",
        fillcolor = "rgba(0,100,255,0.4)",
        line = list(color = "transparent")
      ) %>%
      # Intervalo 95% - Tradicional
      add_ribbons(
        data = forecast_data[forecast_data$Canal == "Tradicional",],
        x = ~Fecha,
        ymin = ~Ventas_Lower_95,
        ymax = ~Ventas_Upper_95,
        name = "IC 95% Tradicional",
        fillcolor = "rgba(255,100,0,0.2)",
        line = list(color = "transparent")
      ) %>%
      # Intervalo 80% - Tradicional
      add_ribbons(
        data = forecast_data[forecast_data$Canal == "Tradicional",],
        x = ~Fecha,
        ymin = ~Ventas_Lower_80,
        ymax = ~Ventas_Upper_80,
        name = "IC 80% Tradicional",
        fillcolor = "rgba(255,100,0,0.4)",
        line = list(color = "transparent")
      ) %>%
      # Líneas de pronóstico
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
        title = "Intervalos de Confianza del Pronóstico",
        xaxis = list(title = "Fecha"),
        yaxis = list(title = "Ventas (K €)"),
        showlegend = TRUE
      )
  })
  
  # Precisión del Pronóstico
  output$forecast_accuracy <- renderDT({
    accuracy_df <- data.frame(
      Métrica = c("RMSE", "MAE", "MAPE", "Error Medio", "Error Estándar"),
      Digital = accuracy(forecast_ventas_digital)[1, c("RMSE", "MAE", "MAPE", "ME", "STDERR")],
      Tradicional = accuracy(forecast_ventas_trad)[1, c("RMSE", "MAE", "MAPE", "ME", "STDERR")]
    )
    
    datatable(accuracy_df, 
              options = list(
                pageLength = 5,
                dom = 't'
              )) %>%
      formatRound(columns = 2:3, digits = 3)
  })
  
  # Tendencias Temporales
  output$temporal_trends <- renderPlotly({
    plot_ly() %>%
      # Tendencia de Ventas
      add_trace(
        data = historical,
        x = ~Fecha,
        y = ~Ventas,
        color = ~Canal,
        name = "Ventas",
        type = "scatter",
        mode = "lines",
        showlegend = TRUE
      ) %>%
      # Agregar línea de tendencia
      add_trace(
        data = historical,
        x = ~Fecha,
        y = fitted(loess(Ventas ~ as.numeric(Fecha), data = historical)),
        name = "Tendencia General",
        type = "scatter",
        mode = "lines",
        line = list(dash = "dash"),
        showlegend = TRUE
      ) %>%
      layout(
        title = "Análisis de Tendencias Temporales",
        xaxis = list(title = "Fecha"),
        yaxis = list(title = "Ventas (K €)"),
        hovermode = "x unified"
      )
  })
  
  
  
}

