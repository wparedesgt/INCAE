# Cargar librerías necesarias
library(tidyverse)
library(survival)
library(prophet)
library(caret)

# Función para calcular DACS
calculate_dacs <- function(customer_data, transactions_data, interactions_data) {
  
  # 1. Componente de Valor Base
  base_value <- transactions_data %>%
    group_by(customer_id) %>%
    summarise(
      total_revenue = sum(amount),
      avg_transaction = mean(amount),
      frequency = n(),
      recency = as.numeric(difftime(max(transaction_date), min(transaction_date), units = "days"))
    ) %>%
    mutate(
      base_score = scale(total_revenue) * 0.4 +
        scale(frequency) * 0.3 +
        scale(avg_transaction) * 0.3
    )
  
  # 2. Componente de Riesgo de Abandono
  churn_model <- survival::coxph(
    Surv(tenure, churn) ~ recency + frequency + monetary,
    data = customer_data
  )
  
  survival_scores <- predict(churn_model, newdata = customer_data, type = "risk")
  
  # 3. Componente de Potencial de Crecimiento
  growth_model <- prophet::prophet(
    df = historical_value,
    yearly.seasonality = TRUE,
    weekly.seasonality = TRUE
  )
  
  future_potential <- predict(growth_model, future_df)
  
  # 4. Componente de Engagement
  engagement_score <- interactions_data %>%
    group_by(customer_id) %>%
    summarise(
      channel_diversity = n_distinct(channel),
      response_rate = mean(response_flag),
      engagement_level = mean(engagement_metric)
    ) %>%
    mutate(
      engagement_score = scale(channel_diversity) * 0.3 +
        scale(response_rate) * 0.3 +
        scale(engagement_level) * 0.4
    )
  
  # 5. Cálculo del DACS final
  dacs <- customer_data %>%
    left_join(base_value, by = "customer_id") %>%
    left_join(engagement_score, by = "customer_id") %>%
    mutate(
      survival_score = scale(survival_scores),
      growth_potential = scale(future_potential$yhat),
      
      dacs_score = base_score * 0.3 +
        (1 - survival_score) * 0.25 +
        growth_potential * 0.25 +
        engagement_score * 0.2,
      
      confidence_level = calculate_confidence_interval(dacs_score)
    )
  
  # 6. Validación cruzada temporal
  train_control <- trainControl(
    method = "timeslice",
    initialWindow = 365,
    horizon = 30,
    fixedWindow = TRUE
  )
  
  cv_results <- train(
    dacs_score ~ .,
    data = dacs,
    method = "xgboost",
    trControl = train_control
  )
  
  return(list(
    dacs_scores = dacs,
    model_performance = cv_results,
    confidence_intervals = confidence_level
  ))
}

# Función para monitoreo continuo
monitor_dacs <- function(dacs_results, threshold = 0.1) {
  dacs_results$dacs_scores %>%
    group_by(customer_segment) %>%
    summarise(
      avg_score = mean(dacs_score),
      score_volatility = sd(dacs_score),
      alert_flag = score_volatility > threshold
    )
}

