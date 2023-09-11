#' @title add_point_when_nonmissing
#' 
#' @description
#' Adds a column containing values along the median regression line for each group when it is present in a survey wave and NA otherwise.
#'
#' @param mod_summary A model summary created by \code{summarise_trends} or a related function.
#' @param observed_data The data used to fit the model summarised in mod_summary.
#' @param group_col The grouping variable to add points by.
#' @param year_col The column containing ISSP waves.
#' @param time_counter The column counting time.
#' 
#' @export
#' 
#' @importFrom dplyr select distinct summarise mutate left_join if_else
#' @importFrom stringr str_c str_detect
#' @importFrom rlang enquo as_name

add_point_when_nonmissing <- function(
    mod_summary,
    observed_data,
    group_col,
    year_col = year,
    time_counter = ndecade
){

  # Defuse group_col argument so it can be passed to joining statement below
  group_col_quo <- enquo(group_col)
  
  # Create df with dummy variables indicating whether groups are present in each wave
  in_wave <- observed_data |> 
    select({{group_col}}, {{year_col}}) |> 
    distinct() |>
    # Collapse year column into a single string of waves in which a group is present
    # e.g., "1993 2000 2010 2020", or "1993 2010", etc.
    summarise(
      years = str_c({{year_col}}, collapse = " "), 
      .by = {{group_col}}
    ) |>
    # Create dummy variables for each wave
    mutate(
      in93 = if_else(str_detect(years, "1993"), 1, 0),
      in00 = if_else(str_detect(years, "2000"), 1, 0),
      in10 = if_else(str_detect(years, "2010"), 1, 0),
      in20 = if_else(str_detect(years, "2020"), 1, 0)
    ) |> 
    select(-years)
  
  # Use the dummy variables to add a column with the median value of groups when they are present in a wave
  output <- mod_summary |>
    left_join(in_wave, by = as_name(group_col_quo)) |> 
    mutate(
      point_when_nonmissing = case_when(
        {{time_counter}} == 0 & in93 == 1 ~ mid,
        {{time_counter}} == 0.7 & in00 == 1 ~ mid,
        {{time_counter}} == 1.7 & in10 == 1 ~ mid,
        {{time_counter}} == 2.7 & in20 == 1 ~ mid,
        TRUE ~ NA
      )
    ) |> 
    select(-in93, -in00, -in10, -in20)
  
  return(output)
  
}