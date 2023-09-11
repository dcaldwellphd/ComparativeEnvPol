#' @title make_plot
#' 
#' @description
#' A wrapper around \code{plot_structure}, \code{plot_layers}, and \code{plot_scales}, with default values set to common use cases in the paper.
#' 
#' 
#' @param data A data set containing the information to be plotted.
#' @param x Name of the column for the x-axis.
#' @param y Name of the column for the y-axis.
#' @param color Name of the column for an optional color aesthetic.
#' @param fill Name of the column for an optional fill aesthetic.
#' @param linetype Name of the column for an optional linetype aesthetic.
#' @param shape Name of the column for an optional shape aesthetic.
#' @param group Name of the column for an optional group aesthetic.
#' @param facet_vars A set of variables or expressions quoted by `ggplot2::vars` and defining faceting groups.
#' @param ncol An option to set the number of panels in a row when using facet_wrap.
#' @param points Name of the variable containing values of the points to draw for each group.
#' @param line Name of the variable containing values of the line to draw for each group.
#' @param ci_upper Name of the variable containing the upper bound of the credible interval of the median regression line for each group.
#' @param ci_lower Name of the variable containing the lower bound of the credible interval of the median regression line for each group.
#' @param hline A value along which to draw a horizontal line on the y-axis.
#' @param missingness_var A variable containing median values along the regression line for each group, for when you want to make the line and credible interval less promiment outside the range of available data.
#' @param breaks A character vector of breaks.
#' @param linetype_values A set of aesthetic values mapping linetypes to breaks.
#' @param fill_values A set of aesthetic values mapping fills to breaks.
#' @param group_values A set of aesthetic values mapping groups to breaks.
#' @param color_values A set of aesthetic values mapping colors to breaks.
#' @param shape_values A set of aesthetic values mapping shapes to breaks.
#' @param labels A character vector of labels for the breaks.
#' @param ndecade_to_years Logical. Do you want to display the number of decades as years on the x-axis?
#' @param ylab A string or expression to use as the y-axis label.
#' @param ylim A vector of values to use as the y-axis limits.
#' 
#' @export
#' 
#' @importFrom rlang enquo

make_plot <- function(
  data,
  x = x,
  y = mid,
  color = NULL,
  fill = pol_type,
  linetype = pol_type,
  shape = NULL,
  group = NULL,
  facet_vars = vars(cntry),
  ncol = NULL,
  points = NULL,
  line = mid,
  ci_upper = upper95,
  ci_lower = lower95,
  hline = 0,
  missingness_var = point_when_nonmissing,
  breaks = c("baseline", "comparison"),
  linetype_values = c(
      "baseline" = "solid",
      "comparison" = "dashed"
      ),
  fill_values = c(
      "baseline" = "navyblue",
      "comparison" = "pink1"
      ),
  group_values = NULL,
  color_values = NULL,
  shape_values = NULL,
  labels = NULL,
  ndecade_to_years = TRUE,
  ylab = "bquote('Adjusted ' ~ R^2)",
  ylim = c(-0.01, 0.24)
  ) {

  p <- plot_structure(
    data = data,
    x = {{x}},
    y = {{y}},
    linetype = {{linetype}},
    fill = {{fill}},
    color = {{color}},
    shape = {{shape}},
    group = {{group}},
    facet_vars = facet_vars
    )

  # Capture the expression passed to points. Otherwise, when these arguments
  # are set to NULL, lazy tidy evaluation results in the symbol "points" being
  # passed to the counterpart arguments in `plot_layers`.
  points_quo <- enquo(points)
  line_quo <- enquo(line)
  ci_upper_quo <- enquo(ci_upper)
  ci_lower_quo <- enquo(ci_lower)
  missingness_var_quo <- enquo(missingness_var)

  p <- plot_layers(
    p,
    points = !!points_quo,
    line = !!line_quo,
    ci_upper = !!ci_upper_quo,
    ci_lower = !!ci_lower_quo,
    hline = hline,
    missingness_var = !!missingness_var_quo
    )

  p <- plot_scales(
    p,
    breaks = breaks,
    linetype_values = linetype_values,
    fill_values = fill_values,
    group_values = group_values,
    color_values = color_values,
    shape_values = shape_values,
    labels = labels,
    ndecade_to_years = ndecade_to_years,
    ylab = ylab,
    ylim = ylim
    )

  return(p)

}