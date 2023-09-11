#' @title efx_to_obs
#' 
#' @description
#' Automates the data wrangling required to compare country effects from a model with the observed data used to fit it.
#' 
#' @param obs_data A data frame containing the observed data used to fit the model.
#' @param baseline A data frame containing the summary of the posterior distribution of country effects from the model.
#' @param filter_obs_to An optional string to filter the observed data to a selection of attitude items.
#'
#' @export
#'
#' @importFrom dplyr filter full_join mutate select across if_else
#' @importFrom tidyselect contains

efx_to_obs <- function(
  obs_data = multiparty_pol,
  efx_data = mp_grhseff2_efx,
  filter_obs_to = "grhseff2"
) {

  if (!is.null(filter_obs_to)) {
    obs_data <- obs_data |>
      filter(att_item == filter_obs_to)
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
    # Add column with the median fitted trend for each country when it is present in a survey wave
    add_point_when_nonmissing(
      observed_data = obs_data,
      group_col = cntry
    ) |>
    # Paste asterisk to country names when their 90% credible interval is above zero
    mutate(
      cntry = if_else(
        slpe_pos95, paste0(cntry, "*"), cntry
      )
    ) |>
    # Order country names by the size of their median slope
    # It is important to do this after adding asterisks to country names
    mutate(
      across(
        cntry, ~factor(
          .x, levels = unique(cntry[order(order)])
        )
      )
    )

  return(output)
}