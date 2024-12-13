# Cargar librerías necesarias
library(tidyverse)
library(lubridate)

# Crear secuencia de fechas
dates <- seq(from = as.Date("2013-01-01"), 
             to = as.Date("2016-12-31"), 
             by = "month")

# Crear vectores base para las iniciativas
initiatives <- c(
  "PAT001", "REG001", "PRO001",  # Sensing
  "PROD001", "DS001", "TRN001",  # Seizing
  "ORG001", "DEC001", "ALL001"   # Reconfiguring
)

initiative_types <- c(
  rep("sensing", 3),
  rep("seizing", 3),
  rep("reconfiguring", 3)
)

metric_names <- c(
  "customer_patterns_identified", "compliance_score", "processes_optimized",
  "new_products_launched", "data_lake_migration", "staff_trained_analytics",
  "areas_reorganized", "data_based_decisions", "digital_alliances_established"
)

departments <- c(
  "Analytics", "Legal", "Operations",
  "Product", "IT", "HR",
  "Strategy", "Executive", "Partnership"
)

responsible_teams <- c(
  "Data Science", "Compliance", "Process Excellence",
  "Innovation", "Data Engineering", "L&D",
  "Change Management", "Management", "Strategy"
)

# Generar datos simulados
set.seed(123) # Para reproducibilidad

# Función para generar valores con tendencia creciente
generate_trend_values <- function(n, start_range, end_range) {
  base <- seq(from = start_range, to = end_range, length.out = n)
  noise <- rnorm(n, mean = 0, sd = (end_range - start_range) * 0.05)
  pmax(pmin(base + noise, end_range), start_range)
}

# Crear dataframe base
n_months <- length(dates)
n_initiatives <- length(initiatives)

axa_data <- expand.grid(
  date = dates,
  initiative_id = initiatives,
  stringsAsFactors = FALSE
)

# Añadir columnas adicionales
axa_data <- axa_data %>%
  arrange(date, initiative_id) %>%
  group_by(initiative_id) %>%
  mutate(
    initiative_type = initiative_types[match(initiative_id, initiatives)],
    metric_name = metric_names[match(initiative_id, initiatives)],
    department = departments[match(initiative_id, initiatives)],
    responsible_team = responsible_teams[match(initiative_id, initiatives)],
    
    # Generar valores objetivo y actuales
    target_value = case_when(
      initiative_type == "sensing" ~ 100,
      initiative_type == "seizing" ~ 100,
      initiative_type == "reconfiguring" ~ 100
    ),
    
    metric_value = case_when(
      initiative_type == "sensing" ~ generate_trend_values(n_months, 10, 95),
      initiative_type == "seizing" ~ generate_trend_values(n_months, 5, 90),
      initiative_type == "reconfiguring" ~ generate_trend_values(n_months, 0, 85)
    ),
    
    completion_percentage = (metric_value / target_value) * 100,
    
    impact_value = case_when(
      initiative_id %in% c("PAT001", "PRO001", "PROD001", "ALL001") ~ 
        generate_trend_values(n_months, 100000, 2000000),
      TRUE ~ 0
    ),
    
    status = case_when(
      completion_percentage < 25 ~ "Not Started",
      completion_percentage < 50 ~ "In Progress",
      completion_percentage < 75 ~ "Advanced",
      TRUE ~ "Near Completion"
    )
  ) %>%
  ungroup()

# Redondear valores numéricos
axa_data <- axa_data %>%
  mutate(
    metric_value = round(metric_value, 2),
    completion_percentage = round(completion_percentage, 2),
    impact_value = round(impact_value, 0)
  )

# Exportar a CSV
write.csv(axa_data, "axa_transformation_data.csv", row.names = FALSE)

# Mostrar las primeras filas del dataset
head(axa_data)

# Resumen de la estructura del dataset
str(axa_data)

# Resumen estadístico
summary(axa_data)