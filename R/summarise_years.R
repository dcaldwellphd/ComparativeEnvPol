#' @title summarise_years
#' 
#' @description
#' Takes a multilevel \code{stanreg} object as input and summarises the
#' posterior distribution of year effects for each group in the
#' model (see details).
#' 
#' @param mod A stanreg object.
#' @param ci_width The width of the credible interval to be reported.
#' 
#' @details
#' The supplementary information for the paper considers nonlinear
#' changes in partisan polarization using a multilevel model with
#' dummy variables for each year. This function summarises the change
#' in partisan polarization for each group in 2000, 2010, and 2020
#' relative to 1993. For models where polarization is predicted using
#' a scalar variable counting time in decades, see 
#' \code{\link{summarise_decades}}.
#' 
#' @export
#' 
#' @importFrom tidybayes spread_draws median_qi
#' @importFrom dplyr mutate case_when rename distinct group_by ungroup select arrange desc left_join
#' @importFrom tidyr pivot_wider pivot_longer separate_wider_delim
#' 
summarise_years <- function(mod, ci_width = 0.95){
  
  posterior <- mod |> 
    tidybayes::spread_draws(
        `(Intercept)`,
        yearf2000,
        yearf2010,
        yearf2020,
        b[term, group]
        ) |> 
    mutate(
        term = case_when(
            term == "(Intercept)" ~ "r93",
            term == "yearf2000" ~ "r00",
            term == "yearf2010" ~ "r10",
            term == "yearf2020" ~ "r20"
            )
        ) |>
    rename(
        f93 = `(Intercept)`,
        f00 = yearf2000,
        f10 = yearf2010,
        f20 = yearf2020
        ) |> 
    pivot_wider(names_from = term, values_from = b) |> 
    mutate(
        c93 = f93 + r93,
        c00 = c93 + f00 + r00,
        c10 = c93 + f10 + r10,
        c20 = c93 + f20 + r20
        ) |> 
    distinct(
        .chain, .iteration, .draw,
        group, c93, c00, c10, c20
        ) |> 
    pivot_longer(
        cols = c(c93, c00, c10, c20),
        names_to = "year",
        values_to = "effect"
        ) |> 
    dplyr::group_by(group, year) |> 
    median_qi(effect, .width = ci_width) |> 
    dplyr::ungroup() |> 
    mutate(
        year = case_when(
            year == "c93" ~ 1993,
            year == "c00" ~ 2000,
            year == "c10" ~ 2010,
            TRUE ~ 2020
            )
        ) |>
    separate_wider_delim(
        cols = group,
        delim = ":",
        names = c("eff_type", "eff_group")
        ) |> 
    rename(
        lower = .lower,
        upper = .upper
        ) |> 
    select(-.width, -.point, -.interval)
  
  
  group_order <- posterior |>
    mutate(
        roc = max(effect) - min(effect), .by = eff_group
        ) |> 
    distinct(eff_type, eff_group, roc) |> 
    arrange(desc(roc)) |> 
    mutate(
        order = row_number(),
        .by = eff_type
        ) |> 
    select(-roc)
    
  output <- left_join(
    posterior, 
    group_order, 
    by = c("eff_type", "eff_group")
  )
  
  return(output)
  
}
