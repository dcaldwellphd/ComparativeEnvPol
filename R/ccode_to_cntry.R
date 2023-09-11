#' @title ccode_to_cntry
#' 
#' @description
#' Converts a column of two-character ISO country codes to country names
#' 
#' @param data A data set object.
#' @param input_col A column containing two-character ISO country codes.
#' @param output_col The name to give the resulting column of country names.
#' @param custom_match A set of custom values mapping two-character ISO country codes to country names (see \code{\link[countrycode]{countrycode}}).
#' 
#' @export
#' 
#' @importFrom dplyr mutate
#' @importFrom countrycode countrycode

ccode_to_cntry <- function(
    data,
    input_col,
    output_col = cntry,
    custom_match = c("GB" = "Great Britain")
) {

  output <- data |> 
    mutate(
      {{output_col}} := countrycode::countrycode(
        sourcevar = {{input_col}},
        origin = "iso2c",
        destination = "country.name",
        nomatch = NULL,
        # Hard code GB to Great Britain, rather than United Kingdom
        custom_match = custom_match
      )
    )
}
