# Helper function to generate propensity scores
generate_propensity_scores <- function(data, full = TRUE) {
  if (full) {
    model <- glm(d ~ x1 + x2 + x3, data = data, family = binomial())
  } else {
    model <- glm(d ~ x1, data = data, family = binomial())
  }
  return(predict(model, type = "response"))
}

# Estimator 0: Unadjusted means
estimator_0 <- function(data) {
  mean_treated <- mean(data$y[data$d == 1])
  mean_control <- mean(data$y[data$d == 0])
  ate <- mean_treated - mean_control
  return(ate)
}

# Estimator 1.1: Regression adjustment with full control variables
estimator_1_1 <- function(data) {
  ate<-0
  return(ate)
}

# Estimator 1.2: Regression adjustment with partial control variables
estimator_1_2 <- function(data) {
  ate<-0
  return(ate)
}

# Estimator 2.1: Conditioning on propensity score with full control variables
estimator_2_1 <- function(data) {
  ate<-0
  return(ate)
}

# Estimator 2.2: Conditioning on propensity score with partial control variables
estimator_2_2 <- function(data) {
  ate<-0
  return(ate)
}

# Estimator 3.1: Re-weighting based on propensity score with full control variables
estimator_3_1 <- function(data) {
  ate<-0
  return(ate)
}

# Estimator 3.2: Re-weighting based on propensity score with partial control variables
estimator_3_2 <- function(data) {
  ate<-0
  return(ate)
}

# Estimator 4.1: Blocking based on propensity score with full control variables
estimator_4_1 <- function(data) {
  ate<-0
  return(ate)
}

# Estimator 4.2: Blocking based on propensity score with partial control variables
estimator_4_2 <- function(data) {
  ate<-0
  return(ate)
}

# Estimator 5.1: doubly robust estimator with full control variables
estimator_5_1 <- function(data) {
  ate<-0
  return(ate)
}

# Estimator 5.2: doubly robust estimator with partial control variables
estimator_5_2 <- function(data) {
  ate<-0
  return(ate)
}
