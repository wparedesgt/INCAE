# Instalar y cargar bibliotecas necesarias
if (!require("shiny")) install.packages("shiny", dependencies = TRUE)
if (!require("ggplot2")) install.packages("ggplot2", dependencies = TRUE)
if (!require("readr")) install.packages("readr", dependencies = TRUE)

library(shiny)
library(ggplot2)
library(readr)

# Servidor
server <- function(input, output) {
  
  # Generar los gráficos dinámicamente
  output$grafico_seleccionado <- renderPlot({
    if (input$grafico == "Tiempo de Lanzamiento") {
      # Cargar los datos
      historical_data <- read_csv("Grafico_Tiempo_Lanzamiento.csv")
      
      ggplot(historical_data, aes(x = as.Date(Fecha), y = Tiempo_Lanzamiento, color = Tipo)) +
        geom_line(size = 1.2) +
        geom_point(size = 2) +
        labs(
          title = "¿Cómo se combinan los tiempos de lanzamiento históricos y las predicciones?",
          x = "Fecha",
          y = "Tiempo de Lanzamiento (días)"
        ) +
        theme_minimal() +
        scale_color_manual(values = c("Histórico" = "blue", "Predicción" = "red"))
      
    } else if (input$grafico == "Satisfacción del Cliente") {
      # Cargar los datos
      satisfaction_data <- read_csv("Grafico_Satisfaccion_Cliente.csv")
      
      ggplot(satisfaction_data, aes(x = as.Date(Fecha), y = Valor, color = Tipo)) +
        geom_line(size = 1.2) +
        geom_point(size = 2) +
        labs(
          title = "¿Cómo evolucionan las tasas de satisfacción del cliente en los datos históricos y predicciones?",
          x = "Fecha",
          y = "Tasa de Satisfacción del Cliente (%)"
        ) +
        theme_minimal() +
        scale_color_manual(values = c("Histórico" = "green", "Predicción" = "purple"))
      
    } else if (input$grafico == "Adopción Ágil e Integración de TI") {
      # Cargar los datos
      scatter_data <- read_csv("Grafico_Adopcion_Integracion.csv")
      
      ggplot(scatter_data, aes(x = Tasa_Adopcion_Agil, y = Porcentaje_Integracion_TI, color = Tipo)) +
        geom_point(size = 3, alpha = 0.7) +
        labs(
          title = "¿Cuál es la relación entre la adopción ágil y la integración de TI en los datos históricos y predicciones?",
          x = "Tasa de Adopción Ágil (%)",
          y = "Porcentaje de Integración de TI (%)"
        ) +
        theme_minimal() +
        scale_color_manual(values = c("Histórico" = "orange", "Predicción" = "blue"))
      
    } else if (input$grafico == "Cultura de Agilismo") {
      # Cargar los datos
      cultura_data <- read_csv("Cultura_Agilismo_Desempeno.csv")
      
      ggplot(cultura_data, aes(x = as.Date(Fecha), y = Cultura_Agilismo)) +
        geom_line(size = 1.2, color = "blue") +
        geom_point(size = 2, color = "blue") +
        labs(
          title = "¿Cómo ha evolucionado el desempeño general de la cultura de agilismo?",
          x = "Fecha",
          y = "Cultura de Agilismo (Escala 1-5)"
        ) +
        theme_minimal()
    }
  })
  
  # Generar el análisis de cada gráfico
  output$analisis <- renderText({
    if (input$grafico == "Tiempo de Lanzamiento") {
      "Análisis: Este gráfico muestra la comparación entre los tiempos de lanzamiento históricos y las predicciones. 
      Las predicciones reflejan una tendencia similar a los datos históricos, con fluctuaciones moderadas."
      
    } else if (input$grafico == "Satisfacción del Cliente") {
      "Análisis: Este gráfico resalta la evolución de la satisfacción del cliente. Las predicciones indican estabilidad 
      con ligeras mejoras en los valores proyectados, lo que sugiere un impacto positivo de las estrategias implementadas."
      
    } else if (input$grafico == "Adopción Ágil e Integración de TI") {
      "Análisis: La relación entre la adopción ágil y la integración de TI muestra tendencias positivas tanto en los datos históricos 
      como en las predicciones, indicando una alineación creciente entre estas métricas críticas."
      
    } else if (input$grafico == "Cultura de Agilismo") {
      "Análisis: Este gráfico refleja cómo la Cultura de Agilismo ha evolucionado durante el período analizado. 
      Se observa una mejora constante en el desempeño general, destacando el impacto de la satisfacción del cliente, 
      la adopción ágil y la reducción de los tiempos de lanzamiento en el fortalecimiento de una cultura organizacional adaptable."
    }
  })
}
