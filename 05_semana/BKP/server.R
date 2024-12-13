# Servidor
server <- function(input, output) {
  
  # Gráfico de Feedback
  output$feedbackPlot <- renderPlot({
    create_plot(
      data = historical_data,
      projections = projections_data,
      y_var = "Feedback",
      title = "Evolución del Feedback recibido",
      y_label = "Feedback (%)"
    )
  })
  
  # Gráfico de Campañas
  output$campaignsPlot <- renderPlot({
    create_plot(
      data = historical_data,
      projections = projections_data,
      y_var = "Campaigns",
      title = "Evolución de las Campañas realizadas",
      y_label = "Número de Campañas"
    )
  })
  
  # Gráfico de Tasa de clics (CTR)
  output$ctrPlot <- renderPlot({
    create_plot(
      data = historical_data,
      projections = projections_data,
      y_var = "CTR",
      title = "Evolución de la Tasa de clics (CTR)",
      y_label = "CTR (%)"
    )
  })
}

