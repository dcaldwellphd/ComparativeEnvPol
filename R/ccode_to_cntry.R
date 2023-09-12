#' @title ccode_to_cntry
#' 
#' @description
#' Converts a vector of two-character ISO country codes to country names
#' 
#' @param x A vector of two-character ISO country codes.
#' @param custom_match A set of custom values mapping two-character ISO 
#' country codes to country names (see \code{\link[countrycode]{countrycode}}).
#' 
#' @export
#' 
#' @importFrom countrycode countrycode

ccode_to_cntry <- function(
    x,
    custom_match = c("GB" = "Great Britain")
) {

  countrycode::countrycode(
    x,
    origin = "iso2c",
    destination = "country.name",
    custom_match = custom_match
    )
}
