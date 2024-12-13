# UI
ui <- dashboardPage(
  skin = "blue",
  
  # Header
  dashboardHeader(
    title = "Dashboard de Transformación Digital",
    titleWidth = 300
  ),
  
  # Sidebar
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      menuItem("Resumen Ejecutivo", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Análisis de Datos", tabName = "data_analysis", icon = icon("database")),
      menuItem("Smart KPI", tabName = "smart_kpi", icon = icon("chart-line")),
      menuItem("Documentación", tabName = "docs", icon = icon("book"))
    )
  ),
  
  # Body
  dashboardBody(
    tabItems(
      # Primera pestaña: Dashboard
      tabItem(
        tabName = "dashboard",
        fluidRow(
          # Value boxes
          valueBoxOutput("total_initiatives_box", width = 3),
          valueBoxOutput("avg_completion_box", width = 3),
          valueBoxOutput("total_impact_box", width = 3),
          valueBoxOutput("advanced_initiatives_box", width = 3)
        ),
        fluidRow(
          # Gráficos principales
          box(
            title = "Progreso de Migración al Lago de Datos",
            status = "primary",
            solidHeader = TRUE,
            plotlyOutput("data_lake_progress"),
            width = 6
          ),
          box(
            title = "Impacto Financiero de Iniciativas",
            status = "primary",
            solidHeader = TRUE,
            plotlyOutput("financial_impact"),
            width = 6
          )
        ),
        fluidRow(
          box(
            title = "Mapa de Calor de Progreso",
            status = "primary",
            solidHeader = TRUE,
            plotlyOutput("heatmap_progress"),
            width = 12
          )
        )
      ),
      
      # Segunda pestaña: Análisis de Datos
      tabItem(
        tabName = "data_analysis",
        fluidRow(
          box(
            title = "Filtros",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            column(4, selectInput("department", "Departamento:",
                                  choices = NULL, multiple = TRUE)),
            column(4, dateRangeInput("date_range", "Rango de Fechas:",
                                     start = NULL, end = NULL)),
            column(4, selectInput("initiative_type", "Tipo de Iniciativa:",
                                  choices = NULL, multiple = TRUE))
          )
        ),
        fluidRow(
          box(
            title = "Tabla de Datos",
            status = "primary",
            solidHeader = TRUE,
            DTOutput("data_table"),
            width = 12
          )
        )
      ),
      
      # Tercera pestaña: Smart KPI
      tabItem(
        tabName = "smart_kpi",
        fluidRow(
          box(
            title = "DACS Score por Departamento",
            status = "warning",
            solidHeader = TRUE,
            plotlyOutput("dacs_plot"),
            width = 6
          ),
          box(
            title = "Monitoreo de KPIs",
            status = "warning",
            solidHeader = TRUE,
            plotlyOutput("kpi_monitoring"),
            width = 6
          )
        ),
        fluidRow(
          box(
            title = "Análisis de Componentes DACS",
            status = "warning",
            solidHeader = TRUE,
            plotlyOutput("dacs_components"),
            width = 12
          )
        )
      ),
      
      # Cuarta pestaña: Documentación
      tabItem(
        tabName = "docs",
        fluidRow(
          box(
            title = "Documentación del Dashboard",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            h3("Sobre este Dashboard"),
            p("Este dashboard está diseñado para analizar y visualizar el progreso 
              de las iniciativas de transformación digital. Integra datos de múltiples 
              fuentes y proporciona análisis avanzados mediante el Smart KPI DACS."),
            h3("Componentes Principales"),
            tags$ul(
              tags$li(strong("Resumen Ejecutivo: "), "Visión general del progreso e impacto de las iniciativas."),
              tags$li(strong("Análisis de Datos: "), "Exploración detallada de los datos con filtros personalizables."),
              tags$li(strong("Smart KPI: "), "Análisis avanzado mediante el DACS Score.")
            )
          )
        )
      )
    )
  )
)

