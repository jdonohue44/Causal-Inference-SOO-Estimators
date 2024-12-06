# Estimator 0: Unadjusted means
estimator_0 <- function(data) {
  model <- lm(y ~ d, data = data)
  coef_summary <- summary(model)$coefficients
  estimate <- coef_summary["dTRUE", "Estimate"]
  
  return(estimate)
}

# Estimator 1.1: Regression adjustment with full control variables
estimator_1_1 <- function(data) {
  model <- lm(y ~ d + x1 + x2 + x3, data = data)
  coef_summary <- summary(model)$coefficients
  estimate <- coef_summary["dTRUE", "Estimate"]
  
  return(estimate)
}

# Estimator 1.2: Regression adjustment with partial control variables
estimator_1_2 <- function(data) {
  model <- lm(y ~ d + x1, data = data)
  coef_summary <- summary(model)$coefficients
  estimate <- coef_summary["dTRUE", "Estimate"]
  
  return(estimate)
}

# Estimator 2.1: Conditioning on propensity score with full control variables
estimator_2_1 <- function(data) {
  # Estimate propensity scores using full control variables
  ps_model <- glm(d ~ x1 + x2 + x3, data = data, family = binomial())
  data$ps <- predict(ps_model, type = "response")
  
  # Estimate treatment effect
  outcome_model <- lm(y ~ d + ps, data = data)
  coef_summary <- summary(outcome_model)$coefficients
  estimate <- coef_summary["dTRUE", "Estimate"]
  
  return(estimate)
}

# Estimator 2.2: Conditioning on propensity score with partial control variables
estimator_2_2 <- function(data) {
  # Estimate propensity scores using partial control variables
  ps_model <- glm(d ~ x1, data = data, family = binomial())
  data$ps <- predict(ps_model, type = "response")
  
  # Estimate treatment effect
  outcome_model <- lm(y ~ d + ps, data = data)
  coef_summary <- summary(outcome_model)$coefficients
  estimate <- coef_summary["dTRUE", "Estimate"]
  
  return(estimate)
}

# Estimator 3.1: Re-weighting based on propensity score with full control variables
estimator_3_1 <- function(data) {
  # Estimate propensity scores using full control variables
  ps_model <- glm(d ~ x1 + x2 + x3, data = data, family = binomial())
  data$ps <- predict(ps_model, type = "response")
  
  # Calculate weights
  data$weight <- ifelse(data$d, 1/data$ps, 1/(1-data$ps))
  
  # Calculate treatment effect
  treated_sum <- sum(data$d * data$y * data$weight)
  control_sum <- sum((1 - data$d) * data$y * data$weight)
  N <- nrow(data)
  
  estimate <- (1/N) * (treated_sum - control_sum)
  return(estimate)
}

# Estimator 3.2: Re-weighting based on propensity score with partial control variables
estimator_3_2 <- function(data) {
  # Estimate propensity scores using partial control variables
  ps_model <- glm(d ~ x1, data = data, family = binomial())
  data$ps <- predict(ps_model, type = "response")
  
  # Calculate weights
  data$weight <- ifelse(data$d, 1/data$ps, 1/(1-data$ps))
  
  # Calculate treatment effect
  treated_sum <- sum(data$d * data$y * data$weight)
  control_sum <- sum((1 - data$d) * data$y * data$weight)
  N <- nrow(data)
  
  estimate <- (1/N) * (treated_sum - control_sum)
  return(estimate)
}

# Estimator 4.1: Blocking based on propensity score with full control variables
estimator_4_1 <- function(data) {
  # Estimate propensity scores using full control variables
  ps_model <- glm(d ~ x1 + x2 + x3, data = data, family = binomial())
  data$ps <- predict(ps_model, type = "response")
  
  # Apply overlap restriction
  p_max_trt <- max(data$ps[data$d == TRUE])
  p_min_trt <- min(data$ps[data$d == TRUE])
  p_max_ctl <- max(data$ps[data$d == FALSE])
  p_min_ctl <- min(data$ps[data$d == FALSE])
  
  data <- data %>%
    filter(ps <= min(p_max_trt, p_max_ctl), ps >= max(p_min_trt, p_min_ctl))
  
  # Create blocks
  data$block <- cut(data$ps, breaks = seq(0, 1, by = 0.05), include.lowest = TRUE)
  
  # Calculate treatment effect for each block
  block_effects <- data %>%
    group_by(block) %>%
    summarize(
      n_k = n(),
      tau_k = mean(y[d == TRUE]) - mean(y[d == FALSE])
    ) %>%
    filter(!is.na(tau_k))
  
  # Calculate overall ATE
  N <- sum(block_effects$n_k)
  estimate <- sum(block_effects$n_k / N * block_effects$tau_k)
  return(estimate)
}

# Estimator 4.2: Blocking based on propensity score with partial control variables
estimator_4_2 <- function(data) {
  # Estimate propensity scores using partial control variables
  ps_model <- glm(d ~ x1, data = data, family = binomial())
  data$ps <- predict(ps_model, type = "response")
  
  # Apply overlap restriction
  p_max_trt <- max(data$ps[data$d == TRUE])
  p_min_trt <- min(data$ps[data$d == TRUE])
  p_max_ctl <- max(data$ps[data$d == FALSE])
  p_min_ctl <- min(data$ps[data$d == FALSE])
  
  data <- data %>%
    filter(ps <= min(p_max_trt, p_max_ctl), ps >= max(p_min_trt, p_min_ctl))
  
  # Create blocks
  data$block <- cut(data$ps, breaks = seq(0, 1, by = 0.05), include.lowest = TRUE)
  
  # Calculate treatment effect for each block
  block_effects <- data %>%
    group_by(block) %>%
    summarize(
      n_k = n(),
      tau_k = mean(y[d == TRUE]) - mean(y[d == FALSE])
    ) %>%
    filter(!is.na(tau_k))
  
  # Calculate overall ATE
  N <- sum(block_effects$n_k)
  estimate <- sum(block_effects$n_k / N * block_effects$tau_k)
  return(estimate)
}

# Estimator 5.1: doubly robust estimator with full control variables
estimator_5_1 <- function(data) {
  # Estimate propensity scores using full control variables
  ps_model <- glm(d ~ x1 + x2 + x3, data = data, family = binomial())
  data$ps <- predict(ps_model, type = "response")
  
  # Calculate weights
  data$w <- with(data, sqrt(d/ps + (1-d)/(1-ps)))
  
  # Fit weighted regression model
  outcome_model <- lm(y ~ d + x1 + x2 + x3, data = data, weights = w)
  
  # Extract treatment effect estimate and standard error
  coef_summary <- summary(outcome_model)$coefficients
  estimate <- coef_summary["dTRUE", "Estimate"]
  return(estimate)
}

# Estimator 5.2: doubly robust estimator with partial control variables
estimator_5_2 <- function(data) {
  # Estimate propensity scores using partial control variables
  ps_model <- glm(d ~ x1, data = data, family = binomial())
  data$ps <- predict(ps_model, type = "response")
  
  # Calculate weights
  data$w <- with(data, sqrt(d/ps + (1-d)/(1-ps)))
  
  # Fit weighted regression model (note: still using all control variables)
  outcome_model <- lm(y ~ d + x1 + x2 + x3, data = data, weights = w)
  
  # Extract treatment effect estimate and standard error
  coef_summary <- summary(outcome_model)$coefficients
  estimate <- coef_summary["dTRUE", "Estimate"]
  return(estimate)
}
