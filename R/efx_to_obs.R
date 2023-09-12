#' @title efx_to_obs
#' 
#' @description
#' Automates the data wrangling required to compare country effects from a 
#' model with the observed data used to fit it.
#' 
#' @param obs_data A data frame containing the observed data used to fit 
#' the model.
#' @param baseline A data frame containing the summary of the posterior 
#' distribution of country effects from the model.
#' @param filter_obs_to An optional string specifying a filter 
#' condition for the \code{obs_data} object.
#' @param add_stars_on A variable indicating which country-slopes have a 
#' credible interval above zero (see details).
#' 
#' @details
#' The \code{\link{summarise_trends}} function adds dummy variables to
#' the posterior summary of a model, indicating which groups have 90% and
#' 95% credible intervals above zero. Providing one of these variables to
#' the \code{add_stars_on} argument of \code{efx_to_obs} will paste an
#' asterisk to country names showing that the credible interval of their
#' slope is positive.
#'
#' @export
#'
#' @importFrom dplyr filter full_join mutate select across if_else
#' @importFrom tidyselect contains
#' @importFrom rlang parse_expr

efx_to_obs <- function(
  obs_data = multiparty_pol,
  efx_data = mp_grhseff2_efx,
  filter_obs_to = "att_item == 'grhseff2'",
  add_stars_on = slpe_pos95
) {

  # Defuse column passed to add_stars_on
  add_stars_on <- substitute(add_stars_on)
  # Parse the filter_obs_to expression
  obs_filter <- rlang::parse_expr(filter_obs_to)

  if (!is.null(filter_obs_to)) {
    obs_data <- obs_data |>
      filter(!!obs_filter)
  }

  output <- obs_data |>
    # Add posterior summary to observed data
    full_join(
      efx_data |>
      ccode_to_cntry(eff_group) |>
      mutate(
        # Create joining year and ndecade columns
        year = 1993 + x * 10,
        ndecade = x
      ) |>
      select(
        -contains("eff_"), -x
      ),
      by = c(
        "cntry", "year", "ndecade"
      )
    ) |>
    # Add column with the median fitted trend for each country when it is 
    # present in a survey wave
    add_point_when_nonmissing(
      observed_data = obs_data,
      group_col = cntry
    )

  if (!is.null(add_stars_on)) {
    # Paste asterisk to country names when their 95% credible interval is 
    # above zero
    output <- output |> 
    mutate(
      cntry = if_else(
        {{ add_stars_on }}, paste0(cntry, "*"), cntry
      )
    )
  }

    # Order country names by the size of their median slope
    # It is important to do this after adding asterisks to country names
    output <- output |>
     mutate(
      across(
        cntry, ~factor(
          .x, levels = unique(cntry[order(order)])
        )
      )
    )

  return(output)
}
