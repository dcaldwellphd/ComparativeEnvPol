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
  filter_baseline_obs_to = NULL,
  baseline_label = "baseline",
  comparisons,
  comparison_obs = multiparty_pol,
  filter_comparison_obs_to = NULL,
  comparison_labels = "comparison",
  group = "ccode",
  add_stars_on = NULL
) {
  
  # Check if comparisons is a data frame. If so, convert it into a list.
  if (is.data.frame(comparisons)) {
    comparisons <- list(comparisons)
  }

  group_sym <- rlang::sym(group)
  add_stars_on <- substitute(add_stars_on)

  output <- baseline |>
    efx_to_obs(
      observed_data = baseline_obs,
      group = group,
      filter_obs_to = filter_baseline_obs_to,
      add_stars_on = NULL,
      apply_order = FALSE
    ) |> 
    # Add column to distinguish baseline from comparison trends
    mutate(pol_type = baseline_label) |> 
    # Join rows from the summary of comparison trends
    bind_rows(
      map2_df(comparisons, comparison_labels, ~.x |>
        # The plot is ordered by baseline slopes.
        # Remove the order column from comparison
        select(-order, -{{ add_stars_on }}) |>
        efx_to_obs(
          observed_data = comparison_obs,
          group = group,
          filter_obs_to = filter_comparison_obs_to,
          add_stars_on = NULL,
          apply_order = FALSE
        ) |>
        # Add column to distinguish comparison from baseline trends
        mutate(pol_type = .y)
      )
    )

    if (!is.null(add_stars_on)) {
      # Identify groups with positive slope in the baseline model
      pos_slope_cases <- baseline |>
        filter(eff_type == group, {{ add_stars_on }}) |>
        pull(eff_group) |>
        unique()

        if (group == "ccode") {
          output <- output |>
            mutate(
              cntry = if_else(
                ccode %in% pos_slope_cases, paste0(cntry, "*"), cntry
              )
            )
        } else {
          output <- output  |> 
            mutate(
              !!group_sym := if_else(
                !!group_sym %in% pos_slope_cases, paste0(!!group_sym, "*"), !!group_sym
              )
            )
        }
      }
      
      if (group == "ccode") {
        output <- output |>
            mutate(
             across(
               cntry, ~factor(
                 .x, levels = unique(cntry[order(order)])
               )
            )
          )
      } else {
        output <- output |>
            mutate(
              across(
                !!group_sym, ~factor(
                  .x, levels = unique(.x[order(order)])
                )
            )
          )
      }

    return(output)

  }
