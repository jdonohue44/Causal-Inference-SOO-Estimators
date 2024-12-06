# Causal Inference Selection of Observables (SOO) Estimators

## Introduction
In this exercise, I implement various estimators under the selection on observables (SOO) design using a simulated data set. The results emphasize the importance of meeting SOO assumptions for observational causal inference. When the assumption is violated, we observe biased and less precise estimates of the ATE. However, more sophisticated methods like doubly robust estimation can help mitigate some of these issues, providing more reliable estimates even when the model is not perfectly specified. This underscores the importance of careful consideration of potential confounders and the use of appropriate statistical techniques in observational causal inference studies.

## Estimators
- Estimator 0: Unadjusted means
- Estimator 1.1: Regression adjustment with full control variables
- Estimator 1.2: Regression adjustment with partial control variables
- Estimator 2.1: Conditioning on propensity score with full control variables
- Estimator 2.2: Conditioning on propensity score with partial control variables
- Estimator 3.1: Re-weighting based on propensity score with full control variables
- Estimator 3.2: Re-weighting based on propensity score with partial control variables
- Estimator 4.1: Blocking based on propensity score with full control variables
- Estimator 4.2: Blocking based on propensity score with partial control variables
- Estimator 5.1: Doubly robust estimator with full control variables
- Estimator 5.2: Doubly robust estimator with partial control variables
