############ Global ############
library(shiny)
library(plotly)
library(dplyr)
library(shinydashboard)
library(shinydashboardPlus)
library(DT)

# Cargar datos
#datos_cultura_liderazgo <- read.csv("Dashboard_Cultura_Liderazgo_FCC.csv")
datos_cultura_liderazgo <- readRDS('Dashboard_Cultura_Liderazgo_FCC.rds')
datos_cultura_liderazgo$Fecha <- as.Date(datos_cultura_liderazgo$Fecha)
