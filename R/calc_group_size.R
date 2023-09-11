#' @title calc_group_size
#' 
#' @description
#' Calculates the survey weighted count and proportion of observations on a variable.
#' 
#' @param data A data set object.
#' @param group The variable on which to calculate counts and proportions.
#' @param by Optional groups to nest observations by (e.g., attitude item, survey wave, country).
#' @param ids Variables specifying cluster ids from largest level to smallest level (leaving the argument empty, NULL, 1, or 0 indicate no clusters).
#' @param probs Variables specifying cluster sampling probabilities.
#' @param strata Variables specifying strata.
#' @param fpc Variables specifying a finite population correct (see \code{\link[survey]{svydesign}})
#' @param weights Variables specifying weights (inverse of probability).
#' @param nest If \code{TRUE}, relabel cluster ids to enforce nesting within strata.
#' 
#' @export
#' 
#' @importFrom dplyr select filter nest_by across rename mutate
#' @importFrom tidyr drop_na pivot_longer unnest_wider
#' @importFrom survey svytable
#' @importFrom tidyselect any_of
#' @importFrom rlang .data

calc_group_size <- function(
  data,
  group,
  by = NULL,
  ids = NULL,
  probs = NULL,
  strata = NULL,
  fpc = NULL,
  weights = NULL,
  nest = FALSE
) {

   weights <- substitute(weights)
   group <- substitute(group)

  # Convert unquoted column names in 'by' to character strings
  by_sub <- substitute(by)
  if (is.null(by_sub) || is.symbol(by_sub)) {
    by <- deparse(by_sub)
  } else {
    by <- sapply(as.list(by_sub)[-1L], deparse)
  }

  calc_group <- function(
    data,
    col = group
    ) {
    fmla <- as.formula(paste0("~", col))
    group_table <- survey::svytable(
      fmla, design = data
      )
    }

  input <- data |>
    select(
      {{ group }},
      any_of(by),
      {{ ids }},
      {{ probs }},
      {{ strata }},
      {{ fpc }},
      {{ weights }}
    ) |>
    # Filtering complete cases
    drop_na({{ group }})

  # Subsetting to weighted sample
  if (!is.null(weights)) {
    input <- input |>
      drop_na({{ weights }}) |>
      filter(.data[[weights]] != 0)
  }

  # Nesting by variables supplied to by argument
  nested_data <- nest_by(input, across(any_of(by)))
  # Setting up survey design for each level
  design_list <- lapply(
    nested_data$data,
    function(nest_level) {
      survey_design <- as_survey_design(
        .data = nest_level,
        ids = {{ ids }},
        probs = {{ probs }},
        strata = {{ strata }},
        fpc = {{ fpc }},
        weights = {{ weights }},
        nest = nest
        )
      return(survey_design)
      }
    )

  nested_data$design_list <- design_list

  # Calculating distributional value for each group
  group_list <- lapply(
    nested_data$design_list,
    function(survey_design) {
      group_val <- calc_group(
        survey_design
      )
    return(group_val)
    }
  )

  nested_data$group_list <- group_list

  # Discaring nested data and design lists
  nested_data <- select(
    nested_data,
    -data,
    -design_list
    )

  output <- nested_data |>
    unnest_wider(group_list) |> 
    pivot_longer(
      cols = -c(any_of(by)),
      names_to = paste0(group),
      values_to = "total"
      ) |>
    drop_na(total) |>
    mutate(
      total = as.numeric(total),
      prop = total / sum(total),
      .by = any_of(by)
    ) 

  return(output)
}