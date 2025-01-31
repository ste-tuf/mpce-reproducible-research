gt_models <- function(results_clean) {
  library(gt)
  # 1. Initialize the gt table object
  gt(
    data = results_clean,
    rowname_col = "Outcome",
    groupname_col = "outcomes"
  ) |>
    # 2. Format numbers
    fmt_number(
      columns = c("Estimate", "2.5 %", "97.5 %"),
      decimals = 3
    ) |>
    fmt(
      columns = "Pr(>|t|)",
      fns = \(x) format.pval(x, digits = 3)
    ) |>
    # 3. Merge and hide columns
    cols_merge(
      columns = c("2.5 %", "97.5 %"),
      pattern = "{1} - {2}"
    ) |>
    cols_hide(
      columns = c("Std. Error", "t value", "term")
    ) |>
    cols_label("2.5 %" = "95% CI") |>
    # 4. Add table title and subtitle
    tab_header(
      title = "Models results"
    )
}
