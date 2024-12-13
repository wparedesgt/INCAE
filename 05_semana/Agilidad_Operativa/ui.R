# Instalar y cargar bibliotecas necesarias
if (!require("shiny")) install.packages("shiny", dependencies = TRUE)
if (!require("ggplot2")) install.packages("ggplot2", dependencies = TRUE)
if (!require("readr")) install.packages("readr", dependencies = TRUE)

library(shiny)
library(ggplot2)
library(readr)

# Interfaz de usuario
ui <- fluidPage(
  titlePanel("Dashboard de Predicciones - Deutsche Bahn Vertrieb GmbH"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "grafico",
        "Seleccione el gráfico para visualizar:",
        choices = c("Tiempo de Lanzamiento", "Satisfacción del Cliente", 
                    "Adopción Ágil e Integración de TI", "Cultura de Agilismo"),
        selected = "Tiempo de Lanzamiento"
      )
    ),
    
    mainPanel(
      plotOutput("grafico_seleccionado"),
      textOutput("analisis")
    )
  )
)


