# ui.R
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Smart Manufacturing Analytics"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("KPIs", tabName = "kpis", icon = icon("chart-line")),
      menuItem("Análisis Detallado", tabName = "analisis", icon = icon("magnifying-glass-chart")),
      menuItem("Datos", tabName = "datos", icon = icon("table"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .small-box {margin-bottom: 15px;}
        .content-wrapper {background-color: #f4f6f9;}
        .box {border-radius: 3px; box-shadow: 0 2px 5px rgba(0,0,0,0.1);}
        .analysis-text {
          padding: 15px;
          background-color: #f8f9fa;
          border-left: 4px solid #007bff;
          margin-top: 10px;
        }
      "))
    ),
    
    tabItems(
      # Dashboard principal
      tabItem(
        tabName = "dashboard",
        fluidRow(
          valueBoxOutput("kpi_eficiencia", width = 3),
          valueBoxOutput("kpi_calidad", width = 3),
          valueBoxOutput("kpi_sostenibilidad", width = 3),
          valueBoxOutput("kpi_innovacion", width = 3)
        ),
        
        fluidRow(
          box(
            width = 8,
            title = "Tendencias Principales",
            status = "primary",
            solidHeader = TRUE,
            highchartOutput("trend_chart"),
            div(class = "analysis-text", textOutput("trend_analysis"))
          ),
          box(
            width = 4,
            title = "Distribución de Indicadores",
            status = "info",
            solidHeader = TRUE,
            plotlyOutput("distribution_plot"),
            div(class = "analysis-text", textOutput("distribution_analysis"))
          )
        ),
        
        fluidRow(
          box(
            width = 12,
            title = "Mapa de Calor de Rendimiento",
            status = "warning",
            solidHeader = TRUE,
            plotlyOutput("heatmap"),
            div(class = "analysis-text", textOutput("heatmap_analysis"))
          )
        )
      ),
      
      # Tab de KPIs
      tabItem(
        tabName = "kpis",
        fluidRow(
          box(
            width = 12,
            title = "Evolución de KPIs",
            status = "primary",
            solidHeader = TRUE,
            highchartOutput("kpi_evolution"),
            div(class = "analysis-text", textOutput("kpi_evolution_analysis"))
          )
        ),
        fluidRow(
          box(
            width = 6,
            title = "Desglose por Meta",
            status = "info",
            solidHeader = TRUE,
            plotlyOutput("goal_breakdown"),
            div(class = "analysis-text", textOutput("goal_breakdown_analysis"))
          ),
          box(
            width = 6,
            title = "Análisis de Tendencias",
            status = "success",
            solidHeader = TRUE,
            plotlyOutput("trend_analysis_plot"),
            div(class = "analysis-text", textOutput("trend_analysis_text"))
          )
        )
      ),
      
      # Tab de análisis
      tabItem(
        tabName = "analisis",
        fluidRow(
          box(
            width = 12,
            title = "Filtros de Análisis",
            status = "primary",
            solidHeader = TRUE,
            fluidRow(
              column(4, selectInput("meta_filter", "Meta:", choices = unique(data$Meta))),
              column(4, selectInput("indicador_filter", "Indicador:", choices = unique(data$Indicador))),
              column(4, dateRangeInput("fecha_filter", "Rango de Fechas:",
                                       start = min(data$Fecha),
                                       end = max(data$Fecha)))
            )
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = "Resultados del Análisis",
            status = "info",
            solidHeader = TRUE,
            plotlyOutput("analysis_results"),
            div(class = "analysis-text", textOutput("analysis_results_text"))
          )
        )
      ),
      
      # Tab de datos
      tabItem(
        tabName = "datos",
        fluidRow(
          box(
            width = 12,
            title = "Exploración de Datos",
            status = "primary",
            solidHeader = TRUE,
            DTOutput("data_table")
          )
        )
      )
    )
  )
)
