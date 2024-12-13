# ui.R
ui <- dashboardPage(
  dashboardHeader(title = "Analytics Dashboard - Análisis Estratégico"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Resumen Ejecutivo", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Análisis de Inversión", tabName = "inversion", icon = icon("money-bill")),
      menuItem("Satisfacción Cliente", tabName = "satisfaccion", icon = icon("smile")),
      menuItem("Métricas de Mercado", tabName = "mercado", icon = icon("chart-line")),
      menuItem("Datos Crudos", tabName = "datos", icon = icon("table")),
      # Añadimos los filtros directamente en el sidebar
      hr(),
      sliderInput("years", "Rango de Años:",
                  min = 1987, max = 2020,
                  value = c(1987, 2020),
                  step = 1),
      checkboxGroupInput("mercados", "Filtrar Mercados:",
                         choices = unique(data$Mercado),
                         selected = unique(data$Mercado))
    )
  ),
  
  dashboardBody(
    # Custom CSS
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f4f6f9;
        }
        .box {
          box-shadow: 0 0 1px rgba(0,0,0,.125), 0 1px 3px rgba(0,0,0,.2);
        }
      "))
    ),
    tabItems(
      tabItem(tabName = "dashboard",
              fluidRow(
                valueBoxOutput("total_inversion", width = 3),
                valueBoxOutput("promedio_satisfaccion", width = 3),
                valueBoxOutput("total_proyectos", width = 3),
                valueBoxOutput("tiempo_promedio", width = 3)
              ),
              fluidRow(
                box(
                  title = "Inversión por Tipo de Iniciativa",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 6,
                  plotlyOutput("grafico_inversion")
                ),
                box(
                  title = "Tendencia de Satisfacción del Cliente",
                  status = "info",
                  solidHeader = TRUE,
                  width = 6,
                  plotlyOutput("grafico_satisfaccion")
                )
              )
      ),
      tabItem(tabName = "inversion",
              fluidRow(
                box(
                  width = 12,
                  title = "Análisis Detallado de Inversiones",
                  status = "primary",
                  solidHeader = TRUE,
                  plotlyOutput("grafico_inversion_detallado", height = "600px")
                )
              )
      ),
      tabItem(tabName = "satisfaccion",
              fluidRow(
                box(
                  width = 12,
                  title = "Matriz de Satisfacción por Mercado y Tiempo",
                  status = "warning",
                  solidHeader = TRUE,
                  plotlyOutput("grafico_satisfaccion_mercado", height = "600px")
                )
              )
      ),
      tabItem(tabName = "mercado",
              fluidRow(
                box(
                  width = 6,
                  title = "Ingresos Incrementales por Mercado",
                  status = "success",
                  solidHeader = TRUE,
                  plotlyOutput("grafico_ingresos")
                ),
                box(
                  width = 6,
                  title = "Tiempo de Respuesta por Mercado",
                  status = "danger",
                  solidHeader = TRUE,
                  plotlyOutput("grafico_tiempo_respuesta")
                )
              )
      ),
      tabItem(tabName = "datos",
              fluidRow(
                box(
                  width = 12,
                  title = "Datos Detallados",
                  status = "primary",
                  solidHeader = TRUE,
                  DTOutput("tabla_datos")
                )
              )
      )
    )
  )
)
