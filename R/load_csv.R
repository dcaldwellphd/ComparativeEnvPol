#' @title load_csv
#' 
#' @description
#' \code{ComparativeEnvPol} uses CSV files to recode attitude and party 
#' support items, which are bundled in its \code{inst/extdata} directory. 
#' This function makes them easy to access.
#' 
#' @param csv Name of the CSV file
#'
#' @export
#' 
load_csv <- function(csv) {
  read.csv(
    system.file(
      "extdata", 
      csv, 
      package = "ComparativeEnvPol", 
      mustWork = TRUE
      )
    )
}