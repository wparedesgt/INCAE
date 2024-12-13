server <- function(input, output) {
  
  # Función calculate_smart_kpis actualizada para retornar estados de color válidos
  calculate_smart_kpis <- reactive({
    # Eficiencia General
    eficiencia_actual <- mean(data$Valor[data$Meta == "Incrementar calidad del producto"], na.rm = TRUE)
    eficiencia_anterior <- mean(data$Valor[data$Meta == "Incrementar calidad del producto" & 
                                             data$Fecha <= (max(data$Fecha) - 30)], na.rm = TRUE)
    cambio_eficiencia <- ((eficiencia_actual - eficiencia_anterior) / eficiencia_anterior) * 100
    
    # Sostenibilidad
    ahorro_plastico <- sum(data$Valor[data$Meta == "Optimizar uso de plástico"], na.rm = TRUE)
    tendencia_sostenibilidad <- mean(diff(data$Valor[data$Meta == "Optimizar uso de plástico"]), na.rm = TRUE)
    
    # Innovación
    nivel_digitalizacion <- mean(data$Valor[data$Meta == "Implementar gemelo digital"], na.rm = TRUE)
    proyectos_innovacion <- length(unique(data$Indicador[data$Meta == "Implementar gemelo digital"]))
    
    # Calidad
    calidad_actual <- mean(data$Valor[data$Meta == "Reducir desperdicio"], na.rm = TRUE)
    tendencia_calidad <- mean(diff(data$Valor[data$Meta == "Reducir desperdicio"]), na.rm = TRUE)
    
    list(
      eficiencia = list(
        valor = eficiencia_actual,
        cambio = cambio_eficiencia,
        tendencia = if(cambio_eficiencia > 0) "↑" else "↓"
      ),
      sostenibilidad = list(
        valor = ahorro_plastico,
        tendencia = if(tendencia_sostenibilidad > 0) "↑" else "↓"
      ),
      innovacion = list(
        valor = nivel_digitalizacion,
        proyectos = proyectos_innovacion
      ),
      calidad = list(
        valor = calidad_actual,
        tendencia = if(tendencia_calidad > 0) "↑" else "↓"
      )
    )
  })
  
  # Value Boxes corregidos
  output$kpi_eficiencia <- renderValueBox({
    kpis <- calculate_smart_kpis()
    valueBox(
      paste0(round(kpis$eficiencia$valor, 1), "% ", kpis$eficiencia$tendencia),
      paste("Eficiencia General", 
            ifelse(kpis$eficiencia$cambio > 0, 
                   paste0("(+", round(kpis$eficiencia$cambio, 1), "%)"),
                   paste0("(", round(kpis$eficiencia$cambio, 1), "%)"))),
      icon = icon("gauge-high"),
      color = get_status_color(kpis$eficiencia$valor)
    )
  })
  
  output$kpi_sostenibilidad <- renderValueBox({
    kpis <- calculate_smart_kpis()
    valueBox(
      paste0(round(kpis$sostenibilidad$valor), " ton ", kpis$sostenibilidad$tendencia),
      "Ahorro en Plástico",
      icon = icon("leaf"),
      color = "olive" # Color apropiado para sostenibilidad
    )
  })
  
  output$kpi_innovacion <- renderValueBox({
    kpis <- calculate_smart_kpis()
    valueBox(
      paste0(round(kpis$innovacion$valor, 1), "%"),
      paste("Nivel de Digitalización -", kpis$innovacion$proyectos, "proyectos"),
      icon = icon("lightbulb"),
      color = "blue" # Color apropiado para innovación
    )
  })
  
  output$kpi_calidad <- renderValueBox({
    kpis <- calculate_smart_kpis()
    valueBox(
      paste0(round(kpis$calidad$valor, 1), "% ", kpis$calidad$tendencia),
      "Índice de Calidad",
      icon = icon("check-circle"),
      color = get_status_color(kpis$calidad$valor)
    )
  })
  
  
  # Datos reactivos para gráficos
  trend_data <- reactive({
    data %>%
      group_by(Fecha, Meta) %>%
      summarise(Valor_Promedio = mean(Valor, na.rm = TRUE), .groups = 'drop')
  })
  
 
  # Corregimos el gráfico de tendencias
  output$trend_chart <- renderHighchart({
    hc <- highchart() %>%
      hc_xAxis(categories = trend_data()$Fecha) %>%
      hc_yAxis(title = list(text = "Valor")) %>%
      hc_title(text = "Evolución de Indicadores por Meta")
    
    # Agregamos cada serie de datos
    unique_metas <- unique(trend_data()$Meta)
    colors <- colorRampPalette(brewer.pal(9, "Set1"))(length(unique_metas))
    
    for(i in seq_along(unique_metas)) {
      meta_data <- trend_data() %>% filter(Meta == unique_metas[i])
      hc <- hc %>% hc_add_series(
        name = unique_metas[i],
        data = meta_data$Valor_Promedio,
        color = colors[i]
      )
    }
    
    hc %>%
      hc_tooltip(shared = TRUE) %>%
      hc_legend(verticalAlign = "bottom", layout = "horizontal") %>%
      hc_chart(zoomType = "x")
  })
  
  
  output$trend_analysis <- renderText({
    generate_trend_analysis(trend_data())
  })
  
  output$distribution_plot <- renderPlotly({
    plot_ly(data, x = ~Indicador, y = ~Valor, type = "box", color = ~Indicador) %>%
      layout(
        showlegend = FALSE,
        xaxis = list(title = ""),
        yaxis = list(title = "Valor")
      )
  })
  
  output$distribution_analysis <- renderText({
    generate_distribution_analysis(data)
  })
  
  # Mapa de calor
  heat_data <- reactive({
    data %>%
      group_by(Meta, Indicador) %>%
      summarise(Valor_Promedio = mean(Valor, na.rm = TRUE), .groups = 'drop')
  })
  
  # Mapa de calor mejorado
  output$heatmap <- renderPlotly({
    # Preparar los datos
    heat_data <- data %>%
      group_by(Meta, Indicador) %>%
      summarise(Valor_Promedio = mean(Valor, na.rm = TRUE), .groups = 'drop')
    
    # Crear una matriz pivote para el mapa de calor
    heat_matrix <- heat_data %>%
      pivot_wider(names_from = Meta, values_from = Valor_Promedio) %>%
      as.data.frame()
    
    # Extraer solo los valores numéricos para la matriz
    matriz_valores <- heat_matrix %>%
      select(-Indicador) %>%
      as.matrix()
    
    # Definir una paleta de colores más clara
    colores <- colorRampPalette(c("#313695", "#4575b4", "#74add1", "#f46d43", "#d73027"))(100)
    
    # Crear el mapa de calor
    plot_ly(
      x = colnames(matriz_valores),
      y = heat_matrix$Indicador,
      z = matriz_valores,
      type = "heatmap",
      colors = colores,
      hoverongaps = FALSE,
      hovertemplate = paste(
        "Meta: %{x}<br>",
        "Indicador: %{y}<br>",
        "Valor: %{z:.1f}<br>",
        "<extra></extra>"
      )
    ) %>%
      layout(
        title = "Mapa de Calor de Rendimiento",
        xaxis = list(
          title = "",
          tickangle = 45,
          tickfont = list(size = 10)
        ),
        yaxis = list(
          title = "",
          tickfont = list(size = 10)
        ),
        margin = list(
          l = 150,
          b = 150
        )
      )
  })
  
  # Análisis del mapa de calor mejorado
  output$heatmap_analysis <- renderText({
    heat_data <- data %>%
      group_by(Meta, Indicador) %>%
      summarise(Valor_Promedio = mean(Valor, na.rm = TRUE), .groups = 'drop')
    
    # Encontrar valores máximos y mínimos
    max_valor <- heat_data %>%
      filter(Valor_Promedio == max(Valor_Promedio))
    
    min_valor <- heat_data %>%
      filter(Valor_Promedio == min(Valor_Promedio))
    
    sprintf("Los valores más altos se observan en %s con %.1f, mientras que los más bajos están en %s con %.1f. 
          Esto sugiere áreas de oportunidad para mejora en los indicadores más bajos.",
            max_valor$Meta[1], max_valor$Valor_Promedio[1],
            min_valor$Meta[1], min_valor$Valor_Promedio[1])
  })
  
  # KPI Evolution
  output$kpi_evolution <- renderHighchart({
    data %>%
      group_by(Fecha) %>%
      summarise(
        Eficiencia = mean(Valor, na.rm = TRUE),
        Calidad = mean(Valor[Meta == "Incrementar calidad del producto"], na.rm = TRUE),
        Sostenibilidad = mean(Valor[Meta == "Optimizar uso de plástico"], na.rm = TRUE),
        Innovacion = mean(Valor[Meta == "Implementar gemelo digital"], na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      gather(key = "KPI", value = "Valor", -Fecha) %>%
      hchart(
        "line",
        hcaes(x = Fecha, y = Valor, group = KPI)
      ) %>%
      hc_title(text = "Evolución de KPIs en el Tiempo") %>%
      hc_yAxis(title = list(text = "Valor")) %>%
      hc_tooltip(shared = TRUE)
  })
  
  output$kpi_evolution_analysis <- renderText({
    "Los KPIs muestran una evolución positiva general, con mejoras significativas en eficiencia y calidad."
  })
  
  # Goal Breakdown
  output$goal_breakdown <- renderPlotly({
    data %>%
      group_by(Meta) %>%
      summarise(Valor_Promedio = mean(Valor, na.rm = TRUE), .groups = 'drop') %>%
      plot_ly(x = ~Meta, y = ~Valor_Promedio, type = "bar", color = ~Meta) %>%
      layout(showlegend = FALSE, xaxis = list(tickangle = 45))
  })
  
  output$goal_breakdown_analysis <- renderText({
    "El desglose por metas muestra las áreas con mejor desempeño y las que requieren atención."
  })
  
  # Trend Analysis Plot
  output$trend_analysis_plot <- renderPlotly({
    data %>%
      group_by(Fecha) %>%
      summarise(Valor_Promedio = mean(Valor, na.rm = TRUE), .groups = 'drop') %>%
      plot_ly(x = ~Fecha, y = ~Valor_Promedio, type = "scatter", mode = "lines+markers") %>%
      add_lines(y = ~fitted(loess(Valor_Promedio ~ as.numeric(Fecha))), 
                name = "Tendencia", line = list(dash = "dash"))
  })
  
  output$trend_analysis_text <- renderText({
    "La línea de tendencia muestra la dirección general del rendimiento a lo largo del tiempo."
  })
  
  # # Analysis Results
  # output$analysis_results <- renderPlotly({
  #   req(input$meta_filter, input$indicador_filter, input$fecha_filter)
  #   
  #   filtered_data <- data %>%
  #     filter(
  #       Meta == input$meta_filter,
  #       Indicador == input$indicador_filter,
  #       Fecha >= input$fecha_filter[1],
  #       Fecha <= input$fecha_filter[2]
  #     )
  #   
  #   plot_ly(filtered_data, x = ~Fecha, y = ~Valor, type = "scatter", mode = "lines+markers") %>%
  #     layout(
  #       title = paste("Análisis de", input$meta_filter),
  #       xaxis = list(title = "Fecha"),
  #       yaxis = list(title = "Valor")
  #     )
  # })
  # 
  # output$analysis_results_text <- renderText({
  #   req(input$meta_filter)
  #   sprintf("Análisis detallado para %s mostrando la evolución temporal del indicador seleccionado.", 
  #           input$meta_filter)
  # })
  
  # Resultados del análisis mejorados
  output$analysis_results <- renderPlotly({
    req(input$meta_filter, input$indicador_filter, input$fecha_filter)
    
    filtered_data <- data %>%
      filter(
        Meta == input$meta_filter,
        Indicador == input$indicador_filter,
        Fecha >= input$fecha_filter[1],
        Fecha <= input$fecha_filter[2]
      ) %>%
      # Agregamos un resumen mensual para reducir el ruido
      mutate(Mes = floor_date(Fecha, "month")) %>%
      group_by(Mes) %>%
      summarise(
        Valor_Promedio = mean(Valor, na.rm = TRUE),
        Valor_Min = min(Valor, na.rm = TRUE),
        Valor_Max = max(Valor, na.rm = TRUE),
        .groups = 'drop'
      )
    
    # Calcular la línea de tendencia
    lm_model <- lm(Valor_Promedio ~ as.numeric(Mes), data = filtered_data)
    trend_line <- predict(lm_model, newdata = filtered_data)
    
    # Crear el gráfico con plotly
    plot_ly() %>%
      # Agregar el área de rango entre min y max
      add_ribbons(
        data = filtered_data,
        x = ~Mes,
        ymin = ~Valor_Min,
        ymax = ~Valor_Max,
        name = "Rango",
        fillcolor = "rgba(173, 216, 230, 0.3)",
        line = list(color = "transparent"),
        showlegend = TRUE
      ) %>%
      # Agregar la línea principal de valores promedio
      add_lines(
        data = filtered_data,
        x = ~Mes,
        y = ~Valor_Promedio,
        name = "Valor Promedio",
        line = list(color = "rgb(0, 114, 188)", width = 2),
        showlegend = TRUE
      ) %>%
      # Agregar la línea de tendencia
      add_lines(
        x = filtered_data$Mes,
        y = trend_line,
        name = "Tendencia",
        line = list(color = "rgb(255, 127, 14)", width = 2, dash = "dash"),
        showlegend = TRUE
      ) %>%
      # Configurar el diseño
      layout(
        title = list(
          text = paste("Análisis de", input$meta_filter),
          font = list(size = 16)
        ),
        xaxis = list(
          title = "Fecha",
          tickformat = "%b %Y",
          showgrid = TRUE,
          gridcolor = "rgba(0,0,0,0.1)"
        ),
        yaxis = list(
          title = "Valor",
          showgrid = TRUE,
          gridcolor = "rgba(0,0,0,0.1)",
          zeroline = TRUE,
          zerolinecolor = "rgba(0,0,0,0.2)"
        ),
        hovermode = "x unified",
        showlegend = TRUE,
        legend = list(
          orientation = "h",
          y = -0.2
        ),
        margin = list(b = 100)
      )
  })
  
  # Análisis de resultados mejorado
  output$analysis_results_text <- renderText({
    req(input$meta_filter, input$indicador_filter, input$fecha_filter)
    
    filtered_data <- data %>%
      filter(
        Meta == input$meta_filter,
        Indicador == input$indicador_filter,
        Fecha >= input$fecha_filter[1],
        Fecha <= input$fecha_filter[2]
      )
    
    # Calcular estadísticas relevantes
    valor_inicial <- filtered_data$Valor[1]
    valor_final <- tail(filtered_data$Valor, 1)
    cambio_porcentual <- ((valor_final - valor_inicial) / valor_inicial) * 100
    promedio <- mean(filtered_data$Valor, na.rm = TRUE)
    tendencia <- ifelse(cambio_porcentual > 0, "positiva", "negativa")
    
    sprintf("Análisis detallado para %s: La tendencia es %s con un cambio del %.1f%% durante el período. 
          El valor promedio es %.1f, con un rango entre %.1f y %.1f. 
          La línea punteada muestra la tendencia general del indicador.",
            input$meta_filter,
            tendencia,
            abs(cambio_porcentual),
            promedio,
            min(filtered_data$Valor, na.rm = TRUE),
            max(filtered_data$Valor, na.rm = TRUE))
  })
  
  
  
  
  # Tabla de datos
  output$data_table <- renderDT({
    datatable(
      data,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
      ),
      class = 'cell-border stripe hover'
    )
  })
}