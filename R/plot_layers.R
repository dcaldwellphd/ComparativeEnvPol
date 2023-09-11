#' @title plot_layers
#' 
#' @description Takes a ggplot2 object as input and adds specified geom layers that can highlight missingness in the data.
#' 
#' @param p A ggplot2 object.
#' @param points Name of the variable containing values of the points to draw for each group.
#' @param line Name of the variable containing values of the line to draw for each group.
#' @param ci_upper Name of the variable containing the upper bound of the credible interval of the median regression line for each group.
#' @param ci_lower Name of the variable containing the lower bound of the credible interval of the median regression line for each group.
#' @param hline A value along which to draw a horizontal line on the y-axis.
#' @param missingness_var A variable containing median values along the regression line for each group, for when you want to make the line and credible interval less promiment outside the range of available data.
#'
#' @export
#'
#' @import ggplot2
#' @importFrom rlang enquo quo_is_null

plot_layers <- function(
  p,
  points = NULL,
  line = NULL,
  ci_upper = NULL,
  ci_lower = NULL,
  hline = NULL,
  missingness_var = NULL
) {

  points_quo <- enquo(points)
  line_quo <- enquo(line)
  ci_upper_quo <- enquo(ci_upper)
  ci_lower_quo <- enquo(ci_lower)
  missingness_var_quo <- enquo(missingness_var)

  # Draws points for each group
  if(!quo_is_null(points_quo)) {
    p <- p +
    geom_point(
      aes(y = !!points_quo),
      size = 1
    )
  }
  # Draws a line for each group
  if(!quo_is_null(line_quo)) {
    # If highlighting missingness
    if(!quo_is_null(missingness_var_quo)) {
      p <- p +
      # A lighter line covers the entire x-axis
      geom_line(
        aes(y = {{ line }}),
        alpha = 0.35
      ) +
      # A more prominent line covers the range of available data for each group
      geom_line(
        data = . %>% filter(!is.na({{ missingness_var }})),
        aes(y = {{ missingness_var }}),
        linewidth = 0.75
      )
    } else {
      p <- p +
      geom_line(
        aes(y = {{ line }}),
        linewidth = 0.75
      )
    }
  }

  # Draws two ribbons around the credible interval of the regression line for each group
  if(!quo_is_null(ci_upper_quo) & !quo_is_null(ci_lower_quo)) {
    # If highlighting missingness
    if(!quo_is_null(missingness_var_quo)) {
      p <- p +
      # A lighter ribbon covers the entire x-axis
      geom_ribbon(
        aes(
          ymin = {{ ci_lower }},
          ymax = {{ ci_upper }}
        ),
        alpha = .1
      ) +
      # A more prominent ribbon covers the range of available data for each group
      geom_ribbon(
        data = . %>% filter(!is.na({{ missingness_var }})),
        aes(
          ymin = {{ ci_lower }},
          ymax = {{ ci_upper }}
        ),
        alpha = .3
      )
    } else {
      p <- p +
      geom_ribbon(
        aes(
          ymin = {{ ci_lower }},
          ymax = {{ ci_upper }}
        ),
        alpha = .3
      )
    }
  }

  # Adds a horizontal line at zero on the y-axis
  if(!is.null(hline)) {
    p <- p +
    geom_hline(
      yintercept = hline,
      linetype = "dotted",
      color = "gray50"
    )
  }

  return(p)

}