#' @title plot_structure
#' 
#' @description
#' Creates a ggplot2 object with any specified faceting and aesthetics.
#' 
#' @param data A data set containing the information to be plotted
#' @param x Name of the column for the x-axis
#' @param y Name of the column for the y-axis
#' @param color Name of the column for an optional color aesthetic
#' @param fill Name of the column for an optional fill aesthetic
#' @param linetype Name of the column for an optional linetype aesthetic
#' @param shape Name of the column for an optional shape aesthetic
#' @param group Name of the column for an optional group aesthetic
#' @param facet_vars A set of variables or expressions quoted by `ggplot2::vars` and defining faceting groups
#' @param ncol An option to set the number of panels in a row when using facet_wrap
#'
#' @export
#'
#' @import ggplot2
#' @importFrom scales pretty_breaks
#' 
plot_structure <- function(
    data,
    x,
    y,
    color = NULL,
    fill = NULL,
    linetype = NULL,
    shape = NULL,
    group = NULL,
    facet_vars = NULL,
    ncol = NULL
) {

  # Store the parse tree for unevaluated aesthetic arguments
  # Allows is.null() to handle non-standard evaluation behaviour
  color_sub <- substitute(color)
  fill_sub <- substitute(fill)
  linetype_sub <- substitute(linetype)
  shape_sub <- substitute(shape)
  group_sub <- substitute(group)

  # Construct the main plot object
  p <- ggplot(data = data, aes(x = {{x}}, y = {{y}})) +
    scale_y_continuous(breaks = pretty_breaks()) +
      theme_bw() +
      theme(
        panel.border = element_rect(linewidth = 1, color = "black"),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 8),
        legend.key.width = unit(1.25, "cm")
      )

  # Add facet_wrap layer if specified
  if (!is.null(facet_vars)) {
    p <- p +
      facet_wrap(facet_vars, ncol = ncol) +
      # Make the border of the strip containing facet labels the same width as the border of the plot panels
      theme(
        strip.background = element_rect(
          linewidth = 1,
          color = "black"
        )
      )
  }

  # Add color aesthetic if specified
  if (!is.null(color_sub)) {
    p <- p +
      aes(color = {{color}})
  }

  # Add fill aesthetic if specified
  if (!is.null(fill_sub)) {
    p <- p +
      aes(fill = {{fill}})
  }

  # Add linetype aesthetic if specified
  if (!is.null(linetype_sub)) {
    p <- p +
      aes(linetype = {{linetype}})
  }

  # Add shape aesthetic if specified
  if (!is.null(shape_sub)) {
    p <- p +
      aes(shape = {{shape}})
  }

  # Add group aesthetic if specified
  if (!is.null(group_sub)) {
    p <- p +
      aes(group = {{group}})
  }

  return(p)

}