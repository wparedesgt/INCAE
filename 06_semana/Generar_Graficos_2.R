# Cargar librerías necesarias
library(tidyverse)
library(lubridate)
library(scales)

# Leer los datos
data <- read.csv("axa_transformation_data.csv")
data$date <- as.Date(data$date)

# 1. Gráfico de evolución de migración al lago de datos
g1 <- data %>%
  filter(initiative_id == "DS001") %>%
  ggplot(aes(x=date, y=metric_value)) +
  geom_line(color="#2C3E50", size=1) +
  geom_point(aes(color=status), size=3) +
  scale_color_manual(values=c("Not Started"="#E74C3C", 
                              "In Progress"="#F1C40F",
                              "Advanced"="#2ECC71",
                              "Near Completion"="#3498DB")) +
  labs(title="Progreso de Migración al Lago de Datos",
       subtitle="2013-2016",
       y="Porcentaje de Completitud",
       x="Fecha") +
  theme_minimal() +
  scale_y_continuous(labels=function(x) paste0(x, "%"))

# 2. Gráfico comparativo de iniciativas relacionadas con datos
g2 <- data %>%
  filter(initiative_id %in% c("DS001", "TRN001", "REG001")) %>%
  ggplot(aes(x=date, y=metric_value, color=initiative_id)) +
  geom_line(size=1) +
  facet_wrap(~metric_name) +
  scale_color_brewer(palette="Set2") +
  labs(title="Comparación de Iniciativas de Datos",
       subtitle="Migración, Capacitación y Cumplimiento",
       y="Porcentaje de Avance",
       x="Fecha",
       color="Iniciativa") +
  theme_minimal()

# 3. Gráfico de calor del progreso por departamento
g3 <- data %>%
  filter(date >= as.Date("2016-01-01")) %>%
  ggplot(aes(x=date, y=department, fill=completion_percentage)) +
  geom_tile() +
  scale_fill_gradient2(low="#FFF3E0", 
                       mid="#FB8C00", 
                       high="#E65100",
                       midpoint=50) +
  labs(title="Mapa de Calor de Progreso por Departamento",
       subtitle="2016",
       fill="% Completitud") +
  theme_minimal()

# 4. Gráfico de impacto financiero de iniciativas de datos
g4 <- data %>%
  filter(impact_value > 0, 
         initiative_id %in% c("PAT001", "PROD001")) %>%
  ggplot(aes(x=date, y=impact_value/1000000, color=initiative_id)) +
  geom_line(size=1) +
  geom_smooth(method="loess", se=FALSE, linetype="dashed") +
  scale_color_brewer(palette="Set1") +
  labs(title="Impacto Financiero de Iniciativas Basadas en Datos",
       subtitle="En millones",
       y="Impacto Financiero (M)",
       x="Fecha",
       color="Iniciativa") +
  theme_minimal() +
  scale_y_continuous(labels=dollar_format())

# Organizar los gráficos
library(gridExtra)
grid.arrange(g1, g2, g3, g4, ncol=2)
