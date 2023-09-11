#' @title efx_to_efx
#' 
#' @description
#' Automates the data wrangling required to compare country effects from 
#' different models (see details).
#' 
#' @param baseline A data frame containing the summary of the posterior 
#' distribution of country effects from the baseline model.
#' @param baseline_obs A data frame containing the observed data used to fit 
#' the the baseline model.
#' @param baseline_label A string to label rows summarizing the baseline model.
#' @param comparisons A single data frame or list of data frames containing 
#' the summary of the posterior distribution of country effects from the 
#' comparison models.
#' @param comparison_obs A data frame containing the observed data used to fit 
#' comparison models.
#' @param comparison_labels A string or vector of strings to label rows 
#' summarizing the comparison model(s).
#' 
#' @details
#' Many plots in the paper compare climate concern polarization trends with 
#' something else, such as economic partisan polarization or the economic 
#' ideological constraint of climate-related attitudes. Joining summaries of 
#' the posterior distributions from different models involves a lot of 
#' repetitive data wrangling, which is automated by the \code{efx_to_efx} 
#' function.
#'
#' @export
#'
#' @importFrom dplyr mutate bind_rows select filter across
#' @importFrom purrr map2_df

efx_to_efx <- function(
  baseline = mp_grhseff2_efx,
  baseline_obs = multiparty_pol,
  baseline_label = "baseline",
  comparisons,
  comparison_obs = multiparty_pol,
  comparison_labels = "comparison"
) {
  
  # Check if comparisons is a data frame. If so, convert it into a list.
  if (is.data.frame(comparisons)) {
    comparisons <- list(comparisons)
  }

  output <- baseline |>
    # Filter to country effects
    filter(eff_type == "ccode") |>
    # Convert country codes to country names
    ccode_to_cntry(eff_group) |>
    # Add column with the median fitted trend for each country when it is 
    # present in a survey wave
    add_point_when_nonmissing(
      observed_data = baseline_obs,
      group = cntry,
      time_counter = x
    ) |>
    # Add column to distinguish baseline from comparison trends
    mutate(pol_type = baseline_label) |>
    # Join rows from the summary of comparison trends
    bind_rows(
      map2_df(comparisons, comparison_labels, ~.x |>
        # The plot is ordered by baseline slopes.
        # Remove the order column from comparison
        select(-order) |>
        # Filter to country effects
        filter(eff_type == "ccode") |>
        # Create column of country names using country codes included in the 
        # model
        ccode_to_cntry(eff_group) |>
        # Add column with the median fitted trend for each country when it is 
        # present in a survey wave
        add_point_when_nonmissing(
          observed_data = comparison_obs,
          group_col = cntry,
          time_counter = x
        ) |>
        # Add column to distinguish comparison from baseline trends
        mutate(pol_type = .y)
      )
    ) |>
    # Order country names by the size of their median slope in the climate 
    # partisan polarization model
    mutate(
      across(
        cntry, ~factor(
          .x, levels = unique(cntry[order(order)])
        )
      )
    )
}