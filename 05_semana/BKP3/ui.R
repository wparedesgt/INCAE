
# UI
ui <- dashboardPage(
  header = dashboardHeader(
    title = "Dashboard Ejecutivo Deutsche Bahn"
  ),
  sidebar = dashboardSidebar(
    sidebarMenu(
      menuItem("Vista General", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Análisis ARIMA", tabName = "arima", icon = icon("chart-line")),
      menuItem("Pronósticos", tabName = "forecast", icon = icon("forward")),
      menuItem("Métricas", tabName = "metrics", icon = icon("gauge"))
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
      # Vista General
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
            width = 12,
            highchartOutput("ventas_trend")
          )
        ),
        fluidRow(
          box(
            title = "Matriz de Correlación",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("correlation_matrix")
          ),
          box(
            title = "Distribución de Ventas",
            status = "warning",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("ventas_dist")
          )
        )
      ),
      
      # Análisis ARIMA
      tabItem(
        tabName = "arima",
        fluidRow(
          box(
            title = "Diagnóstico ARIMA - Canal Digital",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("arima_diag_digital")
          ),
          box(
            title = "Diagnóstico ARIMA - Canal Tradicional",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("arima_diag_trad")
          )
        ),
        fluidRow(
          box(
            title = "Métricas del Modelo",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            DTOutput("arima_metrics")
          )
        )
      ),
      
      # Pronósticos
      tabItem(
        tabName = "forecast",
        fluidRow(
          box(
            title = "Pronóstico de Ventas a 12 Meses",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            highchartOutput("sales_forecast")
          )
        ),
        fluidRow(
          box(
            title = "Intervalos de Confianza",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("confidence_intervals")
          ),
          box(
            title = "Precisión del Pronóstico",
            status = "warning",
            solidHeader = TRUE,
            width = 6,
            DTOutput("forecast_accuracy")
          )
        )
      ),
      
      # Métricas
      tabItem(
        tabName = "metrics",
        fluidRow(
          box(
            title = "KPIs por Canal",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            DTOutput("kpi_table")
          )
        ),
        fluidRow(
          box(
            title = "Tendencias Temporales",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("temporal_trends")
          )
        )
      )
    )
  )
)

