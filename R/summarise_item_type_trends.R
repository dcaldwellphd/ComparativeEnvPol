#' @title summarise_item_type_trends
#' 
#' @description
#' This is a special case of \code{\link{summarise_trends}} for summarising trends separated by item type (see details).
#' 
#' @param mod A stanreg object.
#' @param x A string containing the name of the variable counting time.
#' @param x_vals A vector containing values of x at which to calculate the median and credible interval.
#' @param z1 A string containing the name of the first dummy variable.
#' @param z2 A string containing the name of the second dummy variable.
#' @param cntry_var A string containing the name of the variable used to model country effects.
#' 
#' 
#' @details
#' This function is specifically meant for a three-way grouping of attitude items based on topic-expression (environment-conative, environment-cognitive/affective, climate-cognitive/affective).
#' 
#' @export
#' 
#' @importFrom tidybayes spread_draws
#' @importFrom dplyr ungroup mutate if_else rename distinct select summarise left_join arrange desc reframe group_by
#' @importFrom tidyr pivot_wider separate_wider_delim
#' @importFrom stringr str_replace
#' @importFrom rlang sym
#' 
summarise_item_type_trends <- function(
    mod,
    x = "ndecade",
    x_vals = c(0.0, 0.7, 1.7, 2.7),
    z1 = "is_env_cogaffTRUE",
    z2 = "is_climate_cogaffTRUE",
    cntry_var = "ccode"
) {
    
    # Define a vector of probabilities for quantile calculation
    probs <- c(0.005, 0.025, 0.05, 0.5, 0.95, 0.975, 0.995)
  
    # Create a data frame of all combinations of x vals and item type
    predvar_combos <- expand.grid(
        x = x_vals,
        itemtype_fac = c(
            "Environment-Conative", 
            "Environment-Cognitive/Affective",
            "Climate-Cognitive/Affective"
        )
    ) |>
    # Add columns for year and dummy variables for each item type
    mutate(
        year = 1993 + 10 * x,
        is_env_conative = as.numeric(
            itemtype_fac == "Environment-Conative"
        ),
        is_env_cogaff = as.numeric(
            itemtype_fac == "Environment-Cognitive/Affective"
        ),
        is_climate_cogaff = as.numeric(
            itemtype_fac == "Climate-Cognitive/Affective"
        )
    )
    
    # Extract draws from the posterior distribution
    posterior <- mod |> 
    tidybayes::spread_draws(
        `(Intercept)`, 
        !!sym(x),
        !!sym(z1),
        !!sym(z2),
        !!sym(paste0(x, ":", z1)),
        !!sym(paste0(x, ":", z2)),
        b[term, group]
    ) |>
    # Rename terms for clarity
    mutate(
        term = case_when(
            term == "(Intercept)" ~ "r_a",
            term == x ~ "r_b_x",
            term == z1 ~ "r_b_z1",
            term == z2 ~ "r_b_z2",
            term == paste0(x, ":", z1) ~ "r_b_xz1",
            TRUE ~ "r_b_xz2"
        )
    ) |> 
    # Pivot the data frame to a wider format
    pivot_wider(names_from = term, values_from = b) |> 
    # Rename global variables for clarity
    rename(
        glbl_a = `(Intercept)`,
        glbl_b_x = !!sym(x),
        glbl_b_z1 = !!sym(z1),
        glbl_b_z2 = !!sym(z2),
        glbl_b_xz1 = !!sym(paste0(x,":",z1)),
        glbl_b_xz2 = !!sym(paste0(x,":",z2))
    ) |>
    # Subset to country effects
    filter(str_detect(group, cntry_var))
    
    # Initialize a list to store the posterior predicted value of avg R2 for each country in each year for each type of the three types of item
    fitted_pol_posterior_list <- vector(
        "list", 
        length = nrow(predvar_combos)
    )
    
    # Loop over each row in predvar_combos
    for(i in 1:nrow(predvar_combos)){
      
        # Compute the posterior predicted value and store it in the list
        fitted_pol_posterior_list[[i]] <- data.frame(
            x = predvar_combos$x[i],
            itemtype_fac = predvar_combos$itemtype_fac[i],
            year = predvar_combos$year[i],
            draw = posterior$.draw,
            group = posterior$group,
            pol_val = posterior$glbl_a + 
            posterior$glbl_b_x*predvar_combos$x[i] +
            posterior$glbl_b_z1*predvar_combos$is_env_cogaff[i] +
            posterior$glbl_b_z2*predvar_combos$is_climate_cogaff[i] +
            posterior$glbl_b_xz1*predvar_combos$x[i]*predvar_combos$is_env_cogaff[i] +
            posterior$glbl_b_xz2*predvar_combos$x[i]*predvar_combos$is_climate_cogaff[i] +
            posterior$r_a +
            posterior$r_b_x*predvar_combos$x[i] +
            posterior$r_b_z1*predvar_combos$is_env_cogaff[i] +
            posterior$r_b_z2*predvar_combos$is_climate_cogaff[i] +
            posterior$r_b_xz1*predvar_combos$x[i]*predvar_combos$is_env_cogaff[i] +
            posterior$r_b_xz2*predvar_combos$x[i]*predvar_combos$is_climate_cogaff[i] 
        )
    }
    
    # Combine all data frames in the list into one data frame
    fitted_pol_posterior <- do.call("rbind", fitted_pol_posterior_list)
   
    # Group by each country-year-itemtype combo and summarize posterior for this group with mean and quantiles
    output <-  fitted_pol_posterior |>
    group_by(group, x, year, itemtype_fac) |>
    reframe(
        pol_val = quantile(pol_val, probs), 
        q = probs,
    ) |>
    # Pivot the data frame to a wider format
    pivot_wider(
        names_from = q, 
        values_from = pol_val, 
        names_prefix = "q"
    ) |>
    # Rename columns for clarity
    rename(
        lower99 = q0.005,
        lower95 = q0.025,
        lower90 = q0.05,
        mid = q0.5,
        upper90 = q0.95,
        upper95 = q0.975,
        upper99 = q0.995
    )
  
    # Order groups by size of the slope in baseline itemtype
    slope_order <- posterior |>
    # Unique draws of the baseline slope for each group
    distinct(
        .chain, .iteration, .draw,
        group, r_b_x
    ) |>
    # Calculate median slope for each group
    group_by(group) |>
    summarise(slope = median(r_b_x)) |> 
    arrange(desc(slope)) |>
    mutate(order = seq(1, length(slope), 1)) |> 
    select(group, order)

    # Add the order of group slopes to the output
    output <- output |> 
    left_join(
        slope_order,
        by = "group"
    ) |>
    # Separate the country column into two columns containing the type of group (country, attitude item, etc.) and the group itself
    separate_wider_delim(
        cols = group,  
        delim = ":", 
        names = c("eff_type", "eff_group")
    )
      
    return(output)  
      
}