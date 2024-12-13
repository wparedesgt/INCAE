# Server
server <- function(input, output, session) {
  
  # Cargar datos
  data <- reactive({
    read.csv("axa_transformation_data.csv") %>%
      mutate(date = as.Date(date))
  })
  
  # Actualizar inputs
  observe({
    updateSelectInput(session, "department",
                      choices = unique(data()$department))
    updateSelectInput(session, "initiative_type",
                      choices = unique(data()$initiative_type))
  })
  
  # Value Boxes
  output$total_initiatives_box <- renderValueBox({
    valueBox(
      n_distinct(data()$initiative_id),
      "Total de Iniciativas",
      icon = icon("list"),
      color = "blue"
    )
  })
  
  output$avg_completion_box <- renderValueBox({
    valueBox(
      paste0(round(mean(data()$completion_percentage), 1), "%"),
      "Completitud Promedio",
      icon = icon("percentage"),
      color = "green"
    )
  })
  
  output$total_impact_box <- renderValueBox({
    valueBox(
      paste0("$", format(sum(data()$impact_value)/1e6, big.mark = ","), "M"),
      "Impacto Total",
      icon = icon("dollar-sign"),
      color = "yellow"
    )
  })
  
  output$advanced_initiatives_box <- renderValueBox({
    valueBox(
      sum(data()$status %in% c("Advanced", "Near Completion")),
      "Iniciativas Avanzadas",
      icon = icon("forward"),
      color = "red"
    )
  })
  
  # Gráficos
  output$data_lake_progress <- renderPlotly({
    p <- data() %>%
      filter(initiative_id == "DS001") %>%
      plot_ly(x = ~date, y = ~metric_value, 
              type = 'scatter', mode = 'lines+markers',
              color = ~status, colors = c("red", "yellow", "green", "blue")) %>%
      layout(title = "Progreso de Migración al Lago de Datos",
             xaxis = list(title = "Fecha"),
             yaxis = list(title = "Porcentaje de Completitud"))
    p
  })
  
  output$financial_impact <- renderPlotly({
    p <- data() %>%
      filter(impact_value > 0,
             initiative_id %in% c("PAT001", "PROD001")) %>%
      plot_ly(x = ~date, y = ~impact_value/1e6,
              type = 'scatter', mode = 'lines',
              color = ~initiative_id) %>%
      layout(title = "Impacto Financiero de Iniciativas",
             xaxis = list(title = "Fecha"),
             yaxis = list(title = "Impacto (Millones)"))
    p
  })
  
  output$heatmap_progress <- renderPlotly({
    p <- data() %>%
      plot_ly(x = ~date, y = ~department, z = ~completion_percentage,
              type = 'heatmap',
              colors = colorRamp(c("#FFF3E0", "#FB8C00", "#E65100"))) %>%
      layout(title = "Progreso por Departamento",
             xaxis = list(title = "Fecha"),
             yaxis = list(title = "Departamento"))
    p
  })
  
  # Tabla de datos
  output$data_table <- renderDT({
    filtered_data <- data() %>%
      filter(
        if (!is.null(input$department)) department %in% input$department else TRUE,
        if (!is.null(input$initiative_type)) initiative_type %in% input$initiative_type else TRUE,
        date >= input$date_range[1],
        date <= input$date_range[2]
      )
    
    datatable(filtered_data,
              options = list(pageLength = 10,
                             scrollX = TRUE))
  })
  
  # Smart KPI plots
  output$dacs_plot <- renderPlotly({
    # Simulación de datos DACS para ejemplo
    set.seed(123)
    dacs_data <- data.frame(
      department = unique(data()$department),
      dacs_score = runif(length(unique(data()$department)), 60, 95)
    )
    
    plot_ly(dacs_data, x = ~department, y = ~dacs_score,
            type = "bar", marker = list(color = "steelblue")) %>%
      layout(title = "DACS Score por Departamento",
             xaxis = list(title = "Departamento"),
             yaxis = list(title = "DACS Score"))
  })
  
  output$kpi_monitoring <- renderPlotly({
    # Simulación de datos de monitoreo
    dates <- seq(as.Date("2016-01-01"), as.Date("2016-12-31"), by = "month")
    monitoring_data <- data.frame(
      date = dates,
      kpi_value = cumsum(rnorm(length(dates), 2, 0.5))
    )
    
    plot_ly(monitoring_data, x = ~date, y = ~kpi_value,
            type = "scatter", mode = "lines+markers") %>%
      layout(title = "Evolución del KPI",
             xaxis = list(title = "Fecha"),
             yaxis = list(title = "Valor del KPI"))
  })
  
  output$dacs_components <- renderPlotly({
    # Simulación de componentes DACS
    components <- data.frame(
      Component = c("Base Value", "Churn Risk", "Growth Potential", "Engagement"),
      Value = c(0.8, 0.6, 0.75, 0.85)
    )
    
    plot_ly(components, x = ~Component, y = ~Value,
            type = "scatter", mode = "lines+markers",
            line = list(shape = "spline")) %>%
      layout(title = "Componentes del DACS Score",
             xaxis = list(title = "Componente"),
             yaxis = list(title = "Valor"))
  })
}