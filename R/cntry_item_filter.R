#' @title cntry_item_filter
#' 
#' @description
#' Subsets to specified attitude items that are asked more than once in a country.
#' 
#' @param data A data set object.
#' @param att_col A column with the names of attitude items.
#' @param cntry_col A column with the names of countries.
#' @param wave_col A column with the year of survey waves.
#' @param att_col_filter A string specifying a filter condition for the attitude items.
#'
#' @export
#'
#' @importFrom rlang enquo as_name parse_expr
#' @importFrom dplyr select distinct mutate if_else left_join filter

cntry_item_filter <- function(
    data,
    att_col,
    cntry_col,
    wave_col = year,
    att_col_filter = "att_item %in% c(
      'grhseff2', 'cutenvir', 'prenvir', 
      'taxenvir', 'privent', 'incdiff'
      )"
    ){

  # Defuse columns passed to function arguments
  # These are used to join on these columns in the output
  att_col_quo <- enquo(att_col)
  cntry_col_quo <- enquo(cntry_col)
  wave_col_quo <- enquo(wave_col)

  # Parse the att_col_filter expression
  item_filter <- rlang::parse_expr(att_col_filter)

  # Create an object with information on whether attitude items are asked more than once in a country
  n_cases_filter <- data |>
    select({{att_col}}, {{cntry_col}}, {{wave_col}}) |>
    distinct() |>
    mutate(
      n_morethan1 = if_else(cumsum(n()) > 1, TRUE, FALSE),
      .by = c({{att_col}}, {{cntry_col}})
    ) |>
    distinct(
      {{att_col}}, {{cntry_col}}, {{wave_col}}, n_morethan1
    )

  # Use the object to subset the data
  output <- data |>
    left_join(
      n_cases_filter,
      by = c(
        as_name(att_col_quo),
        as_name(cntry_col_quo),
        as_name(wave_col_quo)
      )
    ) |>
    filter(!!item_filter & n_morethan1 == TRUE) |>
    select(-n_morethan1)

  return(output)

}