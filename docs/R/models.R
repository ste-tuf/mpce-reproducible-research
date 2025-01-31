#' Build a generalized linear model (glm)
#'
#' This function builds a generalized linear model (glm) using the specified outcome, exposure, and dataset.
#'
#' @param outcome The outcome variable.
#' @param exposure The exposure variable.
#' @param dataset The dataset containing the variables.
#'
#' @return A glm object if successful, otherwise NULL.
build_model <- function(outcome, exposure, dataset) {
  formula <- paste0(
    outcome, " ~ ",
    exposure, " + age_screening + gender"
  ) |> as.formula()

  try(
    glm(formula, data = dataset)
  )
}

#' Extract model results
#'
#' This function extracts coefficients, confidence intervals, and AIC (Akaike Information Criterion) from a glm model.
#'
#' @param model A glm model object.
#'
#' @return A dataframe containing model results.
#'
extract_model_result <- function(model) {
  # Get coefficients
  coefs <- coef(summary(model)) |> as.data.frame()

  # Confidence interval
  ci <- confint(model) |> as.data.frame()

  # AIC
  aic <- model$aic

  # Return a dataframe
  cbind(
    coefs,
    ci,
    aic
  ) |> rownames_to_column(var = "term")
}
