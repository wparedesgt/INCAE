# ui.R
ui <- dashboardPage(
  header = dashboardHeader(
    title = "Dashboard Ejecutivo Deutsche Bahn"
  ),
  sidebar = dashboardSidebar(
    sidebarMenu(
      menuItem("Vista General", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Análisis de Ventas", tabName = "ventas", icon = icon("chart-line")),
      menuItem("Proyecciones", tabName = "forecast", icon = icon("forecast")),
      menuItem("Métricas de Rendimiento", tabName = "kpis", icon = icon("gauge"))
    )
  ),
  body = dashboardBody(
    tags$head(
      tags$style(HTML("
        .small-box {height: 100px}
        .content-wrapper {background-color: #f4f6f9}
      "))
    ),
    tabItems(
      tabItem(
        tabName = "dashboard",
        fluidRow(
          valueBoxOutput("vbox_ventas_total", width = 3),
          valueBoxOutput("vbox_satisfaccion", width = 3),
          valueBoxOutput("vbox_proyectos", width = 3),
          valueBoxOutput("vbox_innovacion", width = 3)
        ),
        fluidRow(
          box(
            title = "Evolución de Ventas por Canal",
            status = "primary",
            solidHeader = TRUE,
            highchartOutput("ventas_trend", height = 350)
          ),
          box(
            title = "Satisfacción vs Capacitación",
            status = "info",
            solidHeader = TRUE,
            plotlyOutput("satisfaction_training", height = 350)
          )
        ),
        fluidRow(
          box(
            title = "Distribución de Proyectos",
            status = "warning",
            solidHeader = TRUE,
            plotlyOutput("projects_dist", height = 300)
          ),
          box(
            title = "Índice de Innovación",
            status = "success",
            solidHeader = TRUE,
            plotlyOutput("innovation_index", height = 300)
          )
        )
      ),
      tabItem(
        tabName = "ventas",
        fluidRow(
          box(
            title = "Análisis Comparativo de Ventas",
            status = "primary",
            solidHeader = TRUE,
            highchartOutput("ventas_comparison")
          ),
          box(
            title = "Matriz de Correlación",
            status = "info",
            solidHeader = TRUE,
            plotlyOutput("correlation_matrix")
          )
        )
      ),
      tabItem(
        tabName = "forecast",
        fluidRow(
          box(
            title = "Pronóstico de Ventas",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            highchartOutput("sales_forecast")
          )
        ),
        fluidRow(
          box(
            title = "Métricas de Precisión del Modelo",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            DTOutput("forecast_metrics")
          )
        )
      ),
      tabItem(
        tabName = "kpis",
        fluidRow(
          box(
            title = "Tablero de KPIs",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            DTOutput("kpi_table")
          )
        )
      ), 
      
      tabItem(
        tabName = "forecast",
        fluidRow(
          box(
            title = "Pronóstico de Ventas a 12 Meses",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            highchartOutput("sales_forecast_extended")
          )
        ),
        fluidRow(
          box(
            title = "Bandas de Confianza",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("confidence_bands")
          ),
          box(
            title = "Diagnóstico del Modelo",
            status = "warning",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("model_diagnostics")
          )
        ),
        fluidRow(
          box(
            title = "Métricas de Precisión del Modelo",
            status = "success",
            solidHeader = TRUE,
            width = 12,
            DTOutput("forecast_metrics_extended")
          )
        )
      )
      
    )
  )
)

