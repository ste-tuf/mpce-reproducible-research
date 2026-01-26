#' Compute descriptive statistics for a variable
#'
#' This function computes descriptive statistics for a given variable.
#'
#' @param variable A vector of numeric, factor, or logical values.
#'
#' @return A dataframe containing descriptive statistics.
#'
#' @examples
#' compute_descriptive_stats(c(1, 2, 3, 4, 5))
#' compute_descriptive_stats(factor(c("A", "B", "A", "C", "B")))
compute_descriptive_stats <- function(variable) {
  if (is.numeric(variable)) {
    statistics <- compute_numeric(variable)
  }

  if (is.factor(variable) || is.logical(variable)) {
    statistics <- compute_table(variable)
  }

  return(statistics)
}

#' Compute frequency table for a factor or logical variable
#'
#' This function returns a frequency table for a given factor or logical variable.
#'
#' @param variable A factor or logical vector.
#'
#' @return A dataframe containing the frequency table.
#'
#' @examples
#' compute_table(factor(c("A", "B", "A", "C", "B")))
compute_table <- function(variable) {
  # Return frequency table as a dataframe
  return(as.data.frame(table(variable, useNA = "always", dnn = "level")))
}

#' Compute numeric summary statistics for a numeric variable
#'
#' This function computes summary statistics for a numeric variable,
#' including mean, standard deviation, and quantiles.
#'
#' @param variable A numeric vector.
#'
#' @return A dataframe containing summary statistics.
#'
#' @examples
#' compute_numeric(c(1, 2, 3, 4, 5))
compute_numeric <- function(variable) {
  mean_value <- mean(variable, na.rm = TRUE)
  sd_value <- sd(variable, na.rm = TRUE)
  quantiles <- quantile(variable, na.rm = TRUE)

  # Return statistics as a dataframe
  return(
    cbind(
      data.frame(
        "mean" = mean_value,
        "sd" = sd_value
      ),
      t(quantiles)
    )
  )
}

#' Create a descriptive graph for a variable
#'
#' This function generates a histogram for numeric variables and a barplot for factor or logical variables.
#'
#' @param variable A numeric, factor, or logical vector.
#'
#' @return A ggplot object.
#'
#' @export
#'
#' @examples
#' compute_descriptive_graph(c(1, 2, 3, 4, 5))
compute_descriptive_graph <- function(variable) {
  # Histogram
  if (is.numeric(variable)) {
    p <- ggplot2::ggplot(mapping = aes(x = variable)) +
      ggplot2::geom_histogram()
  }

  # Barplot
  if (is.factor(variable) || is.logical(variable)) {
    p <- ggplot2::ggplot(mapping = aes(x = variable)) +
      ggplot2::geom_bar()
  }

  return(p)
}
