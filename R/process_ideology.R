#' @title process_ideology
#'
#' @description
#' Recodes attitude items in an ISSP wave using values from a CSV file.
#'
#' @param data A wave of ISSP data.
#' @param issp_name Name of the column in the CSV file containing ISSP 
#' attitude item names for the wave.
#' @param csv The csv file used to recode the wave of ISSP data.
#'
#' @export
#'
#' @importFrom rlang enquo as_name
#' @importFrom dplyr filter pull select across left_join if_else rename mutate
#' @importFrom tidyselect any_of
#' @importFrom tidyr pivot_longer separate_wider_delim
#'
process_ideology <- function(
    data,
    issp_name,
    csv = issp_env_vars
) {

  # Defuse the argument supplied to issp_name
  # Allows it to be passed in quotes to various functions below
  issp_name_quo <- enquo(issp_name)

  # Reverses attitude scales
  reverse_scale <- function(x) {
    (max(x, na.rm = TRUE) + 1) - x
  }

  # Rescales attitude response options between 0 and 1
  rescale_0_1 <- function(x) {
    x_scaled <- x - min(x, na.rm = TRUE)
    x_scaled / max(x_scaled, na.rm = TRUE)
  }

  # A vector of strings containing the names of attitude items in the ISSP wave
  the_items <- csv |>
    filter({{issp_name}} != "") |> 
    # Attitude items not present in the ISSP wave
    pull({{issp_name}})

  output <- data |>
    select(
      id, cntry, ccode, year,
      any_of(the_items), weight
    ) |>
    # Remove conflicting value labels by converting attitude item columns
    # Prevents warnings when pivoting longer below
    mutate(across(any_of(the_items), as.numeric)) |>
    # Gather available attitude items into a single column
    pivot_longer(
      cols = any_of(the_items),
      names_to = as_name(issp_name_quo),
      values_to = "orig_att_val"
    ) |>
    # Join the csv file using the column containing the attitude item names
    left_join(csv, by = as_name(issp_name_quo)) |>
    # Split range of valid responses into min and max columns
    separate_wider_delim(
      valid_range,
      delim = ":",
      names = c("range_min", "range_max")
    ) |>
    # Create new attitudes column with values outside valid range set to missing
    mutate(
      range_min = as.numeric(as.character(range_min)),
      range_max = as.numeric(as.character(range_max)),
      orig_att_val = as.numeric(as.character(orig_att_val)),
      att_val = if_else(
        orig_att_val >= range_min & orig_att_val <= range_max, orig_att_val, NA
      )
    ) |>
    # Recode new attitudes column based on settings in the csv file
    # New values run from 0 to 1, with 1 being the most environmental attitude
    # Economic and cultural attitudes: 1 = most left-wing/socially liberal
    mutate(
      att_val = if_else(reverse == TRUE, reverse_scale(att_val), att_val),
      att_val = if_else(is_ordinal == TRUE, rescale_0_1(att_val), att_val),
      .by = nice_name
    ) |>
    # Rename column containing standardised attitude item names
    rename(att_item = nice_name) |>
    select(
      id, cntry, ccode, year,
      att_item, orig_att_val, att_val,
      include, weight
    )
}
