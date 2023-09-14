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
#' @param baseline_x The variable used to model time in the baseline, passed
#' to the \code{time_counter} argument of 
#' \code{\link{add_point_when_nonmissing}}.
#' @param baseline_label A string to label rows summarizing the baseline model.
#' @param comparisons A single data frame or list of data frames containing 
#' the summary of the posterior distribution of country effects from the 
#' comparison models.
#' @param comparison_obs A data frame containing the observed data used to fit 
#' comparison models.
#' @param comparison_x The variable used to model time in the comparison, passed
#' to the \code{time_counter} argument of 
#' \code{\link{add_point_when_nonmissing}}.
#' @param comparison_labels A string or vector of strings to label rows 
#' summarizing the comparison model(s).
#' @param add_stars_on A variable indicating which country-slopes have a 
#' credible interval above zero (see details).
#' 
#' @details
#' Many plots in the paper compare climate concern polarization trends with 
#' something else, such as economic partisan polarization or the economic 
#' ideological constraint of climate-related attitudes. Joining summaries of 
#' the posterior distributions from different models involves a lot of 
#' repetitive data wrangling, which is automated by the \code{efx_to_efx} 
#' function.
#' 
#' The output is ordered by the median slope for countries from the baseline 
#' model, which is the climate danger partisan polarization model by default.
#' You can create a plot with a different order of countries by supplying a
#' different posterior summary to the \code{baseline} argument.
#' 
#' The \code{\link{summarise_trends}} function adds dummy variables to
#' the posterior summary of a model, indicating which groups have 90% and
#' 95% credible intervals above zero. Providing one of these variables to
#' the \code{add_stars_on} argument of \code{efx_to_obs} will paste an
#' asterisk to country names showing that the credible interval of their
#' slope is positive in the baseline model.
#'
#' @export
#'
#' @importFrom dplyr mutate bind_rows select filter across arrange pull if_else
#' @importFrom purrr map2_df

efx_to_efx <- function(
  baseline = mp_grhseff2_efx,
  baseline_obs = multiparty_pol,
  baseline_x = x,
  baseline_label = "baseline",
  comparisons,
  comparison_obs = multiparty_pol,
  comparison_x = x,
  comparison_labels = "comparison",
  add_stars_on = NULL
) {
  
  # Check if comparisons is a data frame. If so, convert it into a list.
  if (is.data.frame(comparisons)) {
    comparisons <- list(comparisons)
  }

  # Defuse column passed to add_stars_on
  add_stars_on <- substitute(add_stars_on)

  output <- baseline |>
    # Filter to country effects
    filter(eff_type == "ccode") |>
    # Convert country codes to country names
    mutate(cntry = ccode_to_cntry(eff_group)) |>
    # Add column with the median fitted trend for each country when it is 
    # present in a survey wave
    add_point_when_nonmissing(
      observed_data = baseline_obs,
      group = cntry,
      time_counter = {{ baseline_x }}
    ) |>
    # Add column to distinguish baseline from comparison trends
    mutate(pol_type = baseline_label) |>
    # Join rows from the summary of comparison trends
    bind_rows(
      map2_df(comparisons, comparison_labels, ~.x |>
        # The plot is ordered by baseline slopes.
        # Remove the order column from comparison
        select(-order, -{{ add_stars_on }}) |>
        # Filter to country effects
        filter(eff_type == "ccode") |>
        # Create column of country names using country codes included in the 
        # model
        mutate(cntry = ccode_to_cntry(eff_group)) |>
        # Add column with the median fitted trend for each country when it is 
        # present in a survey wave
        add_point_when_nonmissing(
          observed_data = comparison_obs,
          group_col = cntry,
          time_counter = {{ comparison_x }}
        ) |>
        # Add column to distinguish comparison from baseline trends
        mutate(pol_type = .y)
      )
    )

    if (!is.null(add_stars_on)) {
      # Identify countries with positive slope in the baseline model
      pos_slope_cases <- baseline |>
        filter(eff_type == "ccode", {{ add_stars_on }}) |>
        pull(eff_group) |>
        unique()
        
      # Paste asterisk to country names when their 95% credible interval is 
      # above zero
      output <- output |>
        mutate(
          cntry = if_else(
            eff_group %in% pos_slope_cases, paste0(cntry, "*"), cntry
            )
          )
    }

    # Order country names by the size of their median slope in the climate 
    # partisan polarization model
    output <- output |> 
      mutate(
        across(
          cntry, ~factor(
            .x, levels = unique(cntry[order(order)])
            )
          )
        ) |>
      arrange(order)

}
