#' @title count_ndecade
#' 
#' @description
#' Adds a variable counting the number of decades from a specified year.
#' 
#' @param data A data set object.
#' @param year_col A column containing survey year.
#' @param from A numeric value to count time from.
#'
#' @export
#' 
#' @importFrom dplyr mutate
#' 
count_ndecade <- function(
  data,
  year_col,
  from = 1993
  ) {
  
  output <- data |> 
    mutate(
      ndecade = as.numeric(as.character({{year_col}})),
      ndecade = ({{year_col}} - from) / 10
    )

  return(output)
}