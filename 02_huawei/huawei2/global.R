# global.R
# Instalación de paquetes necesarios
packages <- c("shiny", "shinydashboard", "shinydashboardPlus", "ggplot2", "plotly", 
              "dplyr", "DT", "tidyr", "scales", "viridis", "lubridate")

for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# Datos simulados con mejor estructura
data <- data.frame(
  Fecha = as.Date(c("1987-05-20", "1990-10-13", "1995-02-15", "2000-11-18", 
                    "2005-07-30", "2010-03-21", "2015-08-15", "2020-12-01")),
  Mercado = factor(c("China", "Europa", "África", "América Latina", "China", 
                     "Europa", "África", "América Latina")),
  Tipo_de_Iniciativa = factor(c("Innovación", "Expansión", "Alianza Estratégica", 
                                "Innovación", "Alianza Estratégica", 
                                "Innovación", "Alianza Estratégica", "Expansión")),
  Inversion_USD = c(24783959.45, 4578390.12, 5678000.89, 32478959.67, 
                    15000329.23, 21345789.00, 9876532.89, 43218945.67),
  Proyectos_Co_creados = c(10, 15, 8, 12, 7, 20, 9, 5),
  Tiempo_de_Respuesta_dias = c(130, 78, 95, 105, 145, 85, 120, 75),
  Satisfaccion_Cliente_Porcentaje = c(85.7, 92.4, 88.1, 95.3, 78.2, 89.5, 91.7, 84.3),
  Ingresos_Incrementales_USD = c(54367823.23, 12345678.34, 67893450.56, 
                                 87654321.23, 23456789.98, 45678912.00, 
                                 34567890.78, 56789012.45),
  Reduccion_Tiempo_Comercializacion_dias = c(35, 25, 15, 40, 30, 20, 10, 50),
  Indicadores_Estrategicos = factor(c("Avance", "Retroceso", "Avance", "Avance", 
                                      "Retroceso", "Avance", "Retroceso", "Avance"))
)

