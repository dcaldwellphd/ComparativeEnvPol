#' @title prepare_wave
#'
#' @description
#' Performs a number of miscellaneous operations on a wave of ISSP data, establishing consistent country codes and names, creating an id and year column for keeping track of observations when joining waves together, and applying a consistent name and numeric class to the column containing post-stratification weights.
#' 
#' @param data A wave of ISSP data.
#' @param cntry_col The column with country names.
#' @param year_value A numeric value to add as \code{year} to the output.
#' @param weight_col The column with post-stratification weights.
#' @param rm_cntry Countries to remove from the wave (see details).
#' @param custom_ccode_to_cntry A set of custom values mapping country names to country codes (see details).
#' @param custom_cntry_to_ccode A set of custom values mapping country codes to country names (see details).
#' 
#' @details
#' To remove countries from a wave of ISSP data, provide \code{rm_cntry} with a pattern to match based on country names used by the ISSP in that wave. For example, default searches for "Northern Ireland" and removes corresponding rows. Remove multiple countries using a pattern like "Nothern Ireland|China".
#' 
#' The function uses \code{countrycode} to assign consistent two character ISO codes based on country names used by the ISSP. However, some ISSP values require a custom mapping, such as the labels for East and West Germany before reunification.
#' 
#' Similarly, the function uses \code{countrycode} on the newly created ISO codes to return country names that may be more consistent than values used by the ISSP across waves. The \code{custom_cntry_to_ccode} makes it possible to specify a custom mapping for these country names. For example, the default hard codes "GB" to "Great Britain" (rather than "United Kingdom"), reflecting the fact that Northern Ireland is excluded by default.
#' 
#' @seealso \code{\link[countrycode]{countrycode}}
#' 
#' @export
#' 
#' @importFrom dplyr mutate
#' @importFrom stringr str_detect
#' @importFrom countrycode countrycode
#' @importFrom tibble rownames_to_column
#' @importFrom haven as_factor

prepare_wave <- function(
    data,
    cntry_col,
    year_value,
    weight_col,
    rm_cntry = "Northern Ireland",
    custom_ccode_to_cntry = c(
        "Germany-East - D-E" = "DE",
        "Germany-West - D-W" = "DE",
        "D-E-Germany-East" = "DE",
        "D-W-Germany-West" = "DE"
    ),
    custom_cntry_to_ccode = c(
        "GB" = "Great Britain"
    )
) {

    input <- mutate(
        data,
        fcntry = as_factor({{cntry_col}})
    )
    
    if (!is.null(rm_cntry)) {
        input <- filter(
            input,
            str_detect(fcntry, rm_cntry, negate = TRUE)
        )
    }
    
    output <- input |> 
        # Create columns of consistent country codes and names
        mutate(
            ccode = countrycode::countrycode(
                sourcevar = fcntry,
                origin = "country.name",
                destination = "iso2c",
                nomatch = NULL,
                custom_match = custom_ccode_to_cntry
            ),
            cntry = countrycode::countrycode(
                sourcevar = ccode,
                origin = "iso2c",
                destination = "country.name",
                nomatch = NULL,
                # Since we exclude Northern Ireland, hard code United Kingdom as Great Britain
                custom_match = custom_cntry_to_ccode
            )
        ) |>
        # Create id column with values that are unique to the wave
        rownames_to_column("id") |>
        mutate(
            # Recode weight column to be consistent across waves
            weight = {{weight_col}},
            weight = as.numeric(weight),
            # Add year column
            year = year_value
        )

    return(output)
}
