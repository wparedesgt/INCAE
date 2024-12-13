# # ui.R
# library(shiny)
# library(plotly)
# library(dplyr)
# library(shinydashboard)
# library(shinydashboardPlus)
# library(DT)

# Definir la interfaz de usuario con shinydashboard
ui <- dashboardPage(
  skin = "blue",
  
  # Header
  dashboardHeader(
    title = "Cultura y Liderazgo FCC",
    titleWidth = 300
  ),
  
  # Sidebar
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      id = "sidebar",
      menuItem("Dashboard Principal", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Compromiso", tabName = "compromiso", icon = icon("users")),
      menuItem("Capacitación", tabName = "capacitacion", icon = icon("graduation-cap")),
      menuItem("Innovación", tabName = "innovacion", icon = icon("lightbulb")),
      menuItem("Análisis de Liderazgo", tabName = "liderazgo", icon = icon("chart-line"))
    )
  ),
  
  # Body
  dashboardBody(
    tags$head(
      tags$style(HTML('
        .content-wrapper, .right-side {
          background-color: #f4f6f9;
        }
        .small-box {transition: all 0.3s;}
        .small-box:hover {transform: translateY(-2px);}
      '))
    ),
    
    tabItems(
      # Dashboard Principal
      tabItem(
        tabName = "dashboard",
        fluidRow(
          # InfoBoxes
          infoBoxOutput("compromiso_box", width = 4),
          infoBoxOutput("capacitacion_box", width = 4),
          infoBoxOutput("innovacion_box", width = 4)
        ),
        fluidRow(
          box(
            title = "Compromiso de Empleados",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            footer = 'El gráfico de burbujas muestra las variaciones en el compromiso de los empleados a lo largo del tiempo, identificando periodos de mayor motivación.',
            plotlyOutput("plot_compromiso")
          ),
          box(
            title = "Satisfacción de Empleados",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            footer = 'El gráfico de dispersión permite observar la variabilidad de la satisfacción de empleados en el tiempo, identificando patrones de mejora o descenso.',
            plotlyOutput("plot_satisfaccion")
          )
        )
      ),
      
      # Pestaña de Compromiso
      tabItem(
        tabName = "compromiso",
        fluidRow(
          box(
            title = "Tendencia de Compromiso",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            footer = '',
            plotlyOutput("plot_compromiso_detalle")
          )
        )
      ),
      
      # Pestaña de Capacitación
      tabItem(
        tabName = "capacitacion",
        fluidRow(
          box(
            title = "Estadísticas de Capacitación",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            footer = 'Este histograma muestra la distribución de participación en programas de capacitación, resaltando las frecuencias de distintos niveles de compromiso.',
            plotlyOutput("plot_capacitacion")
          )
        )
      ),
      
      # Pestaña de Innovación
      tabItem(
        tabName = "innovacion",
        fluidRow(
          box(
            title = "Proyectos de Innovación",
            status = "success",
            solidHeader = TRUE,
            width = 12,
            footer = 'Gráfico de barras que destaca los periodos de mayor actividad en proyectos de innovación, mostrando el impulso de creatividad en la organización.',
            plotlyOutput("plot_innovacion")
          )
        )
      ),
      
      # Pestaña de Liderazgo
      tabItem(
        tabName = "liderazgo",
        fluidRow(
          box(
            title = "Análisis de Eficacia del Liderazgo",
            status = "danger",
            solidHeader = TRUE,
            width = 6,
            footer = 'El box plot permite observar la dispersión en la percepción de la eficacia del liderazgo, destacando el rango y los valores centrales.',
            plotlyOutput("plot_liderazgo")
          ),
          box(
            title = "Colaboración Interdepartamental",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            footer = 'Facilita la comparación de los niveles de colaboración interdepartamental en diferentes periodos',
            plotlyOutput("plot_colaboracion")
          )
        )
      )
    )
  )
)