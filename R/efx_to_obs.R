#' @title efx_to_obs
#' 
#' @description
#' Joins the summary of a set of group effects from a multilevel model 
#' to the observed data used to fit it.
#' 
#' @param model_summary A model summary created by 
#' \code{summarise_trends} or a related function.
#' @param observed_data The data used to fit the model summarised in 
#' \code{model_summary}.
#' @param group A string identifying the grouping variable being 
#' summarised from the model.
#' @param filter_obs_to An optional string specifying a filter condition for 
#' the \code{observed_data} object. It is only really necessary when 
#' plotting points, in which case it prevents data that that isn't part
#' of the model from being plotted.
#' @param add_stars_on A variable indicating which group-slopes have a 
#' credible interval above zero (see details).
#' 
#' @details
#' The \code{\link{summarise_trends}} function adds dummy variables to
#' the posterior summary of a model, indicating which groups have 90% and
#' 95% credible intervals above zero. Providing one of these variables to
#' the \code{add_stars_on} argument of \code{efx_to_obs} will paste an
#' asterisk to group names showing that the credible interval of their
#' slope is positive.
#' 
#' @export
#' 
#' @importFrom dplyr rename mutate left_join filter if_else across
#' @importFrom rlang sym parse_expr

efx_to_obs <- function(
    model_summary,
    observed_data,
    group = "ccode",
    filter_obs_to = "att_item == 'grhseff2'",
    add_stars_on = slope_pos95,
    apply_order = TRUE 
  ){

  add_stars_on <- substitute(add_stars_on)
  group_sym <- rlang::sym(group)

  if (!is.null(filter_obs_to)) {
    obs_filter <- rlang::parse_expr(filter_obs_to)
    observed_data <- observed_data |>
      filter(!!obs_filter)
  }

  input <- model_summary |>
    filter(eff_type == group) |> 
    rename(!!group_sym := eff_group)

  if("ndecade" %in% names(input)){
    input <- input  |> 
      mutate(year = 1993 + ndecade * 10)
  } else if ("year" %in% names(input)) {
    input <- input |>
      mutate(ndecade = (year - 1993) / 10)
  } else {
    stop("No variable called 'ndecade' or 'year' in model summary.")
  }

  output <- suppressMessages(
    input |> left_join(observed_data)
  )

  if (group == "ccode") {
    output <- output |>
      mutate(cntry = ccode_to_cntry(!!group_sym))
  }

  if (!is.null(add_stars_on)) {
    if (group == "ccode") {
      output <- output |> 
        mutate(
          cntry = if_else(
            {{ add_stars_on }}, paste0(cntry, "*"), cntry
          )
        )
    } else {
      output <- output |> 
        mutate(
          !!group_sym := if_else(
            {{ add_stars_on }}, paste0(!!group_sym, "*"), !!group_sym
          )
        )
    }
  }

  if (apply_order) {
    if (group == "ccode") {
      output <- output |>
       mutate(
        across(
          cntry, ~factor(
            .x, levels = unique(cntry[order(order)])
          )
        )
      )
    } else {
      output <- output |>
       mutate(
        across(
          !!group_sym, ~factor(
            .x, levels = unique(.x[order(order)])
          )
        )
      )
    }
  }
  

  return(output)
  
}
