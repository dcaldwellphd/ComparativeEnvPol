#' @title summarise_trends
#' 
#' @description
#' Takes a multilevel \code{stanreg} object as input and summarises the
#' posterior distribution of linear time trends for each group in the
#' model (see details).
#' 
#' @param mod A stanreg object.
#' @param x A string containing the name of the variable counting time.
#' @param x_vals A vector containing values of x at which to calculate the 
#' median and credible interval.
#' @param control_var A string containing the name of a control variable column.
#' @param control_val A numeric value for fitting group-level trends 
#' (e.g., the overall mean in the data).
#' 
#' @details
#' The main approach to modelling polarization in the paper is to predict
#' some statistical outcome using a multilevel model with the number of
#' decades from 1993 as a scalar independent variable. This function
#' summarises the rergession line of each group along values of this 
#' independent variable. For models where polarization is predicted 
#' using dummy variables for each year, see \code{\link{summarise_years}}.
#' 
#' @export
#' 
#' @importFrom tidybayes spread_draws
#' @importFrom dplyr ungroup mutate if_else rename distinct select summarise left_join arrange desc
#' @importFrom tidyr pivot_wider separate_wider_delim
#' @importFrom stringr str_replace
#' @importFrom rlang sym
#' 
summarise_trends <- function(
    mod,
    x = "ndecade",
    x_vals = c(0.0, 0.7, 1.7, 2.7),
    control_var = NULL,
    control_val = NULL
) {

    # Create a vector of probabilities to extract from the posterior
    probs <- c(0.005, 0.025, 0.05, 0.5, 0.95, 0.975, 0.995)

    x_sym <- rlang::sym(x)

    # If not controlling for changes in the distribution of attitudes
    if (is.null(control_var)) {

        posterior <- mod |>
            # Create df with draws of the global intercept and slope and 
            # random effects for each group
            tidybayes::spread_draws(
                `(Intercept)`,
                !!x_sym,
                # Names of the columns containing parameter-value keys for 
                # the random effects
                b[term, group]
                ) |>
            # Remove grouped structure from the output of tidybayes
            ungroup() |>
            # Rename values in the column of random effect parameter names
            mutate(
                term = if_else(
                    term == "(Intercept)", "r_a", term
                    ),
                term = if_else(
                    term == x, "r_b", term
                    )
                ) |> 
            # Create separate columns for random intercepts and slopes of 
            # each group. b is the column containing the random effects
            pivot_wider(names_from = term, values_from = b) |>
            # Rename the columns containing draws of the global intercept 
            # and slope
            rename(
                glbl_a = `(Intercept)`,
                glbl_b = !!x_sym
                ) |>
            # Create columns containing the sum of the global and random 
            # intercepts and slopes
            mutate(
                c_a = glbl_a + r_a,
                c_b = glbl_b + r_b
                ) |>
            distinct(
                # Keys identifying unique draws
                .chain, .iteration, .draw,
                # Columns identifying groups and their estimated effects
                group, c_a, c_b
                )

        # Create df with columns containing the draws for each group
        group_sims <- posterior  |>
            pivot_wider(
                names_from = group,
                values_from = c(c_a, c_b)
                ) |>
            # We no longer need keys identifying unique draws
            select(-.chain, -.iteration, -.draw)

        # Create two data frames, one containing the draws of the intercepts 
        # for each group and one containing the draws of the slopes for 
        # each group
        ints <- select(group_sims, contains("_a_"))
        slps <- select(group_sims, contains("_b_"))

        # Loop over the values of x at which to calculate the median and 
        # credible interval
        output <- NULL
        for (x in x_vals){
            # The regression line for each group
            r_line <- ints + slps * x
            # Create df with the median and credible interval of the 
            # regression line for each group
            post_sum <- data.frame(
                t(
                    apply(
                        r_line, 2, function (x) quantile(x, probs)
                    )
                )
            )
            # Add a column containing names for groups
            post_sum$group <- colnames(r_line)
            # Add a column containing the value of x
            post_sum$x <- x
            # Combine medians and credible intervals in data frame
            output <- rbind(output, post_sum)

        }
        # If controlling for changes in the distribution of attitudes
        # Incorporates the control variable into the steps above
    } else {
        posterior <- mod |>
            tidybayes::spread_draws(
                `(Intercept)`,
                !!x_sym,
                # Column of draws for fixed effects of the control variable
                !!sym(control_var),
                b[term, group]
                ) |>
            ungroup() |>
            mutate(
                term = if_else(
                    term == "(Intercept)", "r_a", term
                    ),
                term = if_else(
                    term == x, "r_b", term
                    )
                ) |>
            pivot_wider(names_from = term, values_from = b) |>
            rename(
                glbl_a = `(Intercept)`,
                glbl_b = !!x_sym,
                # Renaming whatever control variable is supplied
                contr_col = !!sym(control_var)
                ) |>
            mutate(
                c_a = glbl_a + r_a,
                c_b = glbl_b + r_b
                ) |>
            distinct(
                .chain, .iteration, .draw,
                group, c_a, c_b, contr_col
                )

        group_sims <- posterior  |>
            pivot_wider(
                names_from = group,
                values_from = c(c_a, c_b, contr_col)
                ) |>
            select(-.chain, -.iteration, -.draw)

        ints <- select(group_sims, contains("_a_"))
        slps <- select(group_sims, contains("_b_"))
        # Data frame containing draws of the control variable for each group
        cntrls <- select(group_sims, contains("contr_col"))

        output <- NULL
        for (x_val in x_vals){
            # The regression line for each group now includes the control 
            # variable, with values fit to the whatever is supplied as 
            # control_val (e.g., the overall mean in the data)
            r_line <- ints + slps * x_val + cntrls * control_val
            post_sum <- data.frame(t(apply(r_line, 2, function (y) quantile(y, probs))))
            post_sum$group <- colnames(r_line)
            post_sum$x <- x_val
            output <- rbind(output, post_sum)
        }

    }
    # Clean the names of columns and keys in the output
    output <- output |>
        # The group column currently has the name of columns from the df 
        # of intercepts, which is the first component in the construction 
        # of the regression lines for each group.
        # Strip prefixes from group names
        mutate(
            group = str_replace(
                group,
                pattern = "c_a_",
                replacement = ""
            )
        ) |>
        # Give columns containing medians and credible intervals more 
        # informative names
        rename(
            lower99 = X0.5.,
            lower95 = X2.5.,
            lower90 = X5.,
            mid = X50.,
            upper90 = X95.,
            upper95 = X97.5.,
            upper99 = X99.5.
        )

    # Create object containing the order of groups by slope size
    slope_order <- posterior |>
        # Unique draws of the slope for each group
        distinct(
            .chain, .iteration, .draw,
            group, c_b
        ) |>
        # Calculate median slope for each group
        summarise(
            slope_med = median(c_b),
            slope_mean = mean(c_b),
            .by = group
        ) |>
        # Order slopes from largest to smallest
        arrange(desc(slope_med)) |>
        # Create column containing the order of groups
        mutate(order = seq(1, length(slope_med), 1)) |>
        select(group, order, slope_med, slope_mean)

    # Create object containing dummy variables for whether the credible 
    # interval of the slope for each group encompasses 0
    ci_dummies <- posterior |>
        # Unique draws of the slope for each group
        distinct(
            .chain, .iteration, .draw,
            group, c_b
        ) |>
        # Calculate lower bounds of the credible interval for each group
        summarise(
            slope_lo90 = quantile(c_b, 0.05),
            slope_hi90 = quantile(c_b, 0.95),
            slope_lo95 = quantile(c_b, 0.025),
            slope_hi95 = quantile(c_b, 0.975),
            .by = group
        ) |>
        # Create dummy variable for whether the lower bound of the credible 
        # interval is positive
        mutate(
            slope_pos90 = slope_lo90 > 0,
            slope_pos95 = slope_lo95 > 0
        ) |>
        select(
            group,
            slope_lo90, slope_hi90,
            slope_lo95, slope_hi95,
            slope_pos90, slope_pos95
            )

    output <- output |>
        # Add column containing the order of group slopes to output
        left_join(slope_order, by = "group") |>
        # Add columns containing dummy variables for whether the credible 
        # interval of the slope for each group encompasses 0
        left_join(ci_dummies, by = "group") |>
        # Separate group column into two columns containing the type of 
        # group (country, attitude item, etc.) and the group itself
        separate_wider_delim(
            cols = group,
            delim = ":",
            names = c("eff_type", "eff_group")
        ) |> 
        rename(!!x_sym := x)

    return(output)

}
