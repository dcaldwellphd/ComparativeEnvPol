#' @title plot_scales
#'
#' @description Takes a ggplot2 object as input and sets scales and labels for axes and geom layers
#'
#' @param p A ggplot2 object.
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
#' @import ggplot2
#' @importFrom scales pretty_breaks
#' @importFrom rlang parse_expr

plot_scales <- function(
  p,
  breaks = NULL,
  linetype_values = NULL,
  fill_values = NULL,
  group_values = NULL,
  color_values = NULL,
  shape_values = NULL,
  labels = NULL,
  ndecade_to_years = FALSE,
  ylab = NULL,
  ylim = NULL
) {

# Check if ylab is a valid expression
  ylab_valid_expr <- tryCatch(
    expr = { rlang::parse_expr(ylab); TRUE },
    error = function(e) FALSE
  )

  # If ylab is a valid expression, parse it
  if (ylab_valid_expr) {
    ylab_expr <- rlang::parse_expr(ylab)
  } else {
    # If ylab is not a valid expression, use it as a regular character string
    ylab_expr <- ylab
  }
  # Set x-axis labels
  # If ndecade_to_years is TRUE, display values of ndecade as ISSP Environment Module years
  if (ndecade_to_years){
    p <- p +
      scale_x_continuous(
        breaks = c(0.0, 0.7, 1.7, 2.7),
        labels = c(1993, 2000, 2010, 2020)
      ) +
      # Year labels fit better vertically and don't need an axis label
      theme(
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1)
      )
    # If ndecade_to_years is FALSE, use scales::pretty_breaks() to plot values appearing in the data
  } else {
    p <- p +
      scale_x_continuous(breaks = pretty_breaks()) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }

  if (!is.null(linetype_values)) {
    p <- p +
      scale_linetype_manual(
        breaks = breaks,
        values = linetype_values,
        labels = labels
      )
  }

  if (!is.null(fill_values)) {
    p <- p +
      scale_fill_manual(
        breaks = breaks,
        values = fill_values,
        labels = labels
      )
  }

  if (!is.null(group_values)) {
    p <- p +
      scale_group_manual(
        breaks = breaks,
        values = group_values,
        labels = labels
      )
  }

  if (!is.null(color_values)) {
    p <- p +
      scale_color_manual(
        breaks = breaks,
        values = color_values,
        labels = labels
      )
  }

  if (!is.null(shape_values)) {
    p <- p +
      scale_shape_manual(
        breaks = breaks,
        values = shape_values,
        labels = labels
      )
  }

  if (!is.null(ylab)) {
    p <- p +
      labs(y = eval(ylab_expr))
  }

  if (!is.null(ylim)) {
    p <- p +
      coord_cartesian(ylim = ylim)
  }

  return(p)

}
