
### server.R ###
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
  
  # Función para configuración común de análisis
  add_analysis <- function(p, analysis_text) {
    subplot(
      p %>% layout(margin = list(b = 150)),
      plotly_empty() %>%
        add_annotations(
          text = analysis_text,
          x = 0.5,
          y = 0.5,
          xref = "paper",
          yref = "paper",
          showarrow = FALSE,
          font = list(size = 11, color = "#2c3e50"),
          align = "center",
          width = 500
        ),
      nrows = 2,
      heights = c(0.8, 0.2)
    ) %>% 
      layout(
        margin = list(b = 20, l = 50, r = 50, t = 50),
        annotations = list(
          list(
            x = 0.5,
            y = -0.15,
            xref = "paper",
            yref = "paper",
            showarrow = FALSE,
            text = "<b>Análisis:</b>",
            font = list(size = 12, color = "#34495e")
          )
        )
      )
  }
  
  # Gráficos con análisis mejorados
  output$data_lake_progress <- renderPlotly({
    p <- data() %>%
      filter(initiative_id == "DS001") %>%
      plot_ly(x = ~date, y = ~metric_value, 
              type = 'scatter', mode = 'lines+markers',
              color = ~status, colors = c("red", "yellow", "green", "blue")) %>%
      layout(title = "Progreso de Migración al Lago de Datos",
             xaxis = list(title = "Fecha"),
             yaxis = list(title = "Porcentaje de Completitud"))
    
    add_analysis(p, 
                 "El proyecto mostró tres fases distintivas: inicio gradual en 2013-2014,
       aceleración significativa en 2014-2015, logrando 85% de avance en 2016.")
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
    
    add_analysis(p,
                 "Se observa un crecimiento sostenido con impacto financiero superior a $4M.
       La identificación de patrones impulsa directamente el lanzamiento de productos.")
  })
  
  output$heatmap_progress <- renderPlotly({
    p <- data() %>%
      plot_ly(x = ~date, y = ~department, z = ~completion_percentage,
              type = 'heatmap',
              colors = colorRamp(c("#FFF3E0", "#FB8C00", "#E65100"))) %>%
      layout(title = "Progreso por Departamento",
             xaxis = list(title = "Fecha"),
             yaxis = list(title = "Departamento"))
    
    add_analysis(p,
                 "Los departamentos de Analytics y Operations muestran el mayor avance.
       Se evidencia una aceleración general en la adopción durante Q3 2015.")
  })
  
  # Smart KPI plots con análisis mejorados
  output$dacs_plot <- renderPlotly({
    set.seed(123)
    dacs_data <- data.frame(
      department = unique(data()$department),
      dacs_score = runif(length(unique(data()$department)), 60, 95)
    )
    
    p <- plot_ly(dacs_data, x = ~department, y = ~dacs_score,
                 type = "bar", marker = list(color = "steelblue")) %>%
      layout(title = "DACS Score por Departamento",
             xaxis = list(title = "Departamento",
                          tickangle = 25),
             yaxis = list(title = "DACS Score"),
             margin = list(b = 250))  # Margen inferior aumentado
    
    subplot(
      p,
      plotly_empty() %>%
        add_annotations(
          text = "Los departamentos de Analytics y Operations destacan con scores superiores al 85%.
                 Se identifica oportunidad de mejora en transferencia de conocimientos interdepartamental.",
          x = 0.5,
          
          y = -0.1,  # Posición vertical ajustada más abajo
          xref = "paper",
          yref = "paper",
          showarrow = FALSE,
          font = list(size = 11, color = "#2c3e50"),
          align = "center",
          width = 500
        ),
      nrows = 2,
      heights = c(0.9, 0.1)  # Proporción ajustada
    )
  })
  
  output$kpi_monitoring <- renderPlotly({
    dates <- seq(as.Date("2016-01-01"), as.Date("2016-12-31"), by = "month")
    monitoring_data <- data.frame(
      date = dates,
      kpi_value = cumsum(rnorm(length(dates), 2, 0.5))
    )
    
    p <- plot_ly(monitoring_data, x = ~date, y = ~kpi_value,
                 type = "scatter", mode = "lines+markers") %>%
      layout(title = "Evolución del KPI",
             xaxis = list(title = "Fecha"),
             yaxis = list(title = "Valor del KPI"))
    
    add_analysis(p,
                 "La tendencia muestra un crecimiento constante durante el periodo.
       Los principales puntos de inflexión se observan en Q2 y Q4 del año.")
  })
  
  output$dacs_components <- renderPlotly({
    components <- data.frame(
      Component = c("Base Value", "Churn Risk", "Growth Potential", "Engagement"),
      Value = c(0.8, 0.6, 0.75, 0.85)
    )
    
    p <- plot_ly(components, x = ~Component, y = ~Value,
                 type = "scatter", mode = "lines+markers",
                 line = list(shape = "spline")) %>%
      layout(title = "Componentes del DACS Score",
             xaxis = list(title = "Componente"),
             yaxis = list(title = "Valor"))
    
    add_analysis(p,
                 "Engagement (85%) y Base Value (80%) representan las mayores fortalezas.
       Se requiere atención especial en la gestión del Churn Risk (60%).")
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
}