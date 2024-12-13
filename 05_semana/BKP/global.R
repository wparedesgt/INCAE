# Cargar librerías necesarias
library(shiny)
library(ggplot2)
library(dplyr)

# Leer datos desde los archivos CSV
historical_data <- read.csv("Deutsche_Bahn_Metrics.csv")
# Simular datos proyectados (se pueden pre-generar y guardar como CSV)
projections_data <- read.csv("Projections_Data.csv")

# Convertir fechas a formato adecuado
historical_data$Fecha <- as.Date(historical_data$Fecha)
projections_data$Fecha <- as.Date(projections_data$Fecha)

# Función para crear los gráficos
create_plot <- function(historical_data, projections, y_var, title, y_label) {
  ggplot() +
    geom_line(data = data, aes(x = Fecha, y = !!sym(y_var)), color = "blue", size = 1) +
    geom_line(data = projections, aes(x = Fecha, y = !!sym(y_var)), color = "red", linetype = "dashed", size = 1) +
    labs(title = title, x = "Fecha", y = y_label) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
}

