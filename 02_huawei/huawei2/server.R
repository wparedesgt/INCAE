
# server.R
server <- function(input, output, session) {
  
  # Datos filtrados reactivos
  filtered_data <- reactive({
    data %>%
      filter(
        year(Fecha) >= input$years[1],
        year(Fecha) <= input$years[2],
        Mercado %in% input$mercados
      )
  })
  
  # Value Boxes
  output$total_inversion <- renderValueBox({
    valueBox(
      paste0("$", format(sum(filtered_data()$Inversion_USD)/1e6, digits = 2), "M"),
      "Inversión Total",
      icon = icon("dollar-sign"),
      color = "green"
    )
  })
  
  output$promedio_satisfaccion <- renderValueBox({
    valueBox(
      paste0(round(mean(filtered_data()$Satisfaccion_Cliente_Porcentaje), 1), "%"),
      "Satisfacción Promedio",
      icon = icon("thumbs-up"),
      color = "blue"
    )
  })
  
  output$total_proyectos <- renderValueBox({
    valueBox(
      sum(filtered_data()$Proyectos_Co_creados),
      "Proyectos Co-creados",
      icon = icon("project-diagram"),
      color = "purple"
    )
  })
  
  output$tiempo_promedio <- renderValueBox({
    valueBox(
      paste0(round(mean(filtered_data()$Tiempo_de_Respuesta_dias)), " días"),
      "Tiempo de Respuesta Promedio",
      icon = icon("clock"),
      color = "red"
    )
  })
  
  # Gráficos
  output$grafico_inversion <- renderPlotly({
    p <- filtered_data() %>%
      group_by(Tipo_de_Iniciativa) %>%
      summarise(Total_Inversion = sum(Inversion_USD)) %>%
      plot_ly(x = ~Tipo_de_Iniciativa, 
              y = ~Total_Inversion, 
              type = "bar",
              marker = list(color = viridis(3))) %>%
      layout(title = "Inversión por Tipo de Iniciativa",
             xaxis = list(title = "Tipo de Iniciativa"),
             yaxis = list(title = "Inversión (USD)",
                          tickformat = "$,.0f"))
    p
  })
  
  output$grafico_satisfaccion <- renderPlotly({
    p <- filtered_data() %>%
      plot_ly(x = ~Fecha, 
              y = ~Satisfaccion_Cliente_Porcentaje, 
              color = ~Mercado,
              type = "scatter",
              mode = "lines+markers") %>%
      layout(title = "Evolución de la Satisfacción del Cliente",
             xaxis = list(title = "Fecha"),
             yaxis = list(title = "Satisfacción (%)",
                          range = c(70, 100)))
    p
  })
  
  output$grafico_inversion_detallado <- renderPlotly({
    p <- filtered_data() %>%
      plot_ly() %>%
      add_trace(x = ~Fecha, 
                y = ~Inversion_USD, 
                color = ~Mercado,
                size = ~Proyectos_Co_creados,
                type = "scatter",
                mode = "markers",
                marker = list(sizeref = 0.3)) %>%
      layout(title = "Inversión por Mercado y Tiempo",
             xaxis = list(title = "Fecha"),
             yaxis = list(title = "Inversión (USD)",
                          tickformat = "$,.0f"))
    p
  })
  
  
  ########
  
  
  output$grafico_satisfaccion_mercado <- renderPlotly({
    # Creamos una matriz de calor (heatmap) más informativa
    p <- filtered_data() %>%
      plot_ly() %>%
      add_trace(
        x = ~Fecha,
        y = ~Mercado,
        z = ~Satisfaccion_Cliente_Porcentaje,
        type = "heatmap",
        colorscale = "Viridis",
        text = ~paste(
          "Fecha:", format(Fecha, "%Y-%m-%d"),
          "<br>Mercado:", Mercado,
          "<br>Satisfacción:", round(Satisfaccion_Cliente_Porcentaje, 1), "%",
          "<br>Tiempo de Respuesta:", Tiempo_de_Respuesta_dias, "días",
          "<br>Inversión:", scales::dollar(Inversion_USD)
        ),
        hoverinfo = "text"
      ) %>%
      layout(
        title = "Matriz de Satisfacción del Cliente por Mercado y Tiempo",
        xaxis = list(title = "Fecha", tickangle = 45),
        yaxis = list(title = "Mercado"),
        colorbar = list(title = "Satisfacción (%)")
      )
    p
  })
  
  
  
  
  
  ######
  
  
  
  output$grafico_ingresos <- renderPlotly({
    p <- filtered_data() %>%
      group_by(Mercado) %>%
      summarise(Total_Ingresos = sum(Ingresos_Incrementales_USD)) %>%
      plot_ly(x = ~Mercado, 
              y = ~Total_Ingresos, 
              type = "bar",
              marker = list(color = viridis(4))) %>%
      layout(title = "Ingresos Incrementales por Mercado",
             xaxis = list(title = "Mercado"),
             yaxis = list(title = "Ingresos (USD)",
                          tickformat = "$,.0f"))
    p
  })
  
  output$grafico_tiempo_respuesta <- renderPlotly({
    p <- filtered_data() %>%
      group_by(Mercado) %>%
      summarise(Tiempo_Promedio = mean(Tiempo_de_Respuesta_dias)) %>%
      plot_ly(x = ~Mercado, 
              y = ~Tiempo_Promedio, 
              type = "bar",
              marker = list(color = viridis(4))) %>%
      layout(title = "Tiempo de Respuesta Promedio por Mercado",
             xaxis = list(title = "Mercado"),
             yaxis = list(title = "Días"))
    p
  })
  
  # Tabla de datos
  output$tabla_datos <- renderDT({
    filtered_data() %>%
      mutate(
        Fecha = as.character(Fecha),
        Inversion_USD = scales::dollar(Inversion_USD),
        Ingresos_Incrementales_USD = scales::dollar(Ingresos_Incrementales_USD)
      ) %>%
      datatable(
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel', 'pdf')
        ),
        rownames = FALSE,
        class = 'cell-border stripe'
      )
  })
}