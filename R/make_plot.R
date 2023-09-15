#' @title make_plot
#' 
#' @description
#' Creates a \code{ggplot2} object with any specified faceting and 
#' aesthetics, adds specified geom layers that can highlight 
#' missingness in the data, and sets scales and labels for axes 
#' and geom layers.
#' 
#' @param data A data set containing the information to be plotted.
#' @param x Name of the column for the x-axis.
#' @param y Name of the column for the y-axis.
#' @param color Name of the column for an optional color aesthetic.
#' @param fill Name of the column for an optional fill aesthetic.
#' @param linetype Name of the column for an optional linetype aesthetic.
#' @param shape Name of the column for an optional shape aesthetic.
#' @param group Name of the column for an optional group aesthetic.
#' @param facet_vars A set of variables or expressions quoted by 
#' \code{ggplot2::vars} and defining faceting groups.
#' @param ncol An option to set the number of panels in a row when using 
#' facet_wrap.
#' @param points Name of the variable containing values of the points to draw 
#' for each group.
#' @param line Name of the variable containing values of the line to draw 
#' for each group.
#' @param ci_upper Name of the variable containing the upper bound of the 
#' credible interval of the median regression line for each group.
#' @param ci_lower Name of the variable containing the lower bound of the 
#' credible interval of the median regression line for each group.
#' @param use_linerange Logical. Do you want to draw vertical lines along
#' the credible interval at each value of x?
#' @param hline A value along which to draw a horizontal line on the y-axis.
#' @param highlight_missingness Logical. Do you want to highlight missing
#' values in the data?
#' @param breaks A character vector of breaks.
#' @param linetype_values A set of aesthetic values mapping linetypes to breaks.
#' @param fill_values A set of aesthetic values mapping fills to breaks.
#' @param group_values A set of aesthetic values mapping groups to breaks.
#' @param color_values A set of aesthetic values mapping colors to breaks.
#' @param shape_values A set of aesthetic values mapping shapes to breaks.
#' @param labels A character vector of labels for the breaks.
#' @param ndecade_to_years Logical. Do you want to display the number of 
#' decades as years on the x-axis?
#' @param ylab A string or expression to use as the y-axis label.
#' @param ylim A vector of values to use as the y-axis limits.
#' 
#' @export
#' 
#' @import ggplot2
#' @importFrom rlang parse_expr
#' @importFrom scales pretty_breaks

 make_plot <- function(
   data,
   x = ndecade,
   y = adj_r2,
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
   use_linerange = FALSE,
   hline = 0,
   highlight_missingness = TRUE,
   breaks = c("baseline", "comparison"),
   linetype_values = c("baseline" = "solid", "comparison" = "dashed"),
   fill_values = c("baseline" = "navyblue", "comparison" = "pink1"),
   group_values = NULL,
   color_values = NULL,
   shape_values = NULL,
   labels = NULL,
   ndecade_to_years = TRUE,
   ylab = "bquote('Adjusted ' ~ R^2)",
   ylim = c(-0.01, 0.24)
   ) {

   # Store the parse tree for unevaluated aesthetic arguments
   # Allows is.null() to handle non-standard evaluation behaviour
   color_sub <- substitute(color)
   fill_sub <- substitute(fill)
   linetype_sub <- substitute(linetype)
   shape_sub <- substitute(shape)
   group_sub <- substitute(group)
   points_sub <- substitute(points)
   line_sub <- substitute(line)
   ci_upper_sub <- substitute(ci_upper)
   ci_lower_sub <- substitute(ci_lower)

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

   if (!is.null(facet_vars)) {
     p <- p +
       facet_wrap(facet_vars, ncol = ncol) + 
       # Make the border of the strip containing facet labels the same width 
       # as the border of the plot panels
       theme(
         strip.background = element_rect(
           linewidth = 1,
           color = "black"
         )
       )
   }

   if (!is.null(color_sub)) {
     p <- p +
       aes(color = {{color}}) 
   }

   if (!is.null(fill_sub)) {
     p <- p +
       aes(fill = {{fill}})
   }

   if (!is.null(linetype_sub)) {
     p <- p +
       aes(linetype = {{linetype}})
   }

   if (!is.null(shape_sub)) {
     p <- p +
       aes(shape = {{shape}})
   }

   if (!is.null(group_sub)) {
     p <- p +
       aes(group = {{group}})
   }

   if(!is.null(points_sub)) {
     if(highlight_missingness) {
         p <- p +
             geom_point(
                 data = . %>% filter(!is.na({{ y }})), 
                 aes(y = {{ points }}), 
                 size = 1
             )
     } else {
         p <- p +
             geom_point(
                 aes(y = {{ points }}),
                 size = 1
             )
     }
   }

   # Draw a line for each group if line_sub is not NULL
   if(!is.null(line_sub)) {
     # If highlighting missingness
     if(highlight_missingness) {
       p <- p +
       # A lighter line covers the entire x-axis
       geom_line(
         aes(y = {{ line }}),
         alpha = 0.35 
       ) +
       # A more prominent line covers the range of available data for each group
       geom_line(
         data = . %>% filter(!is.na({{ y }})),
         aes(y = {{ line }}),
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

   if (!is.null(ci_upper_sub) & !is.null(ci_lower_sub)) {
     if (highlight_missingness) {
         if (use_linerange) {
             p <- p +
                 geom_linerange(
                     data = . %>% filter(!is.na({{ y }})),
                     aes(ymin = {{ ci_lower }}, ymax = {{ ci_upper }}),
                     alpha = .3 
                 )
         }
       p <- p +
       geom_ribbon(
         aes(
           ymin = {{ ci_lower }},
           ymax = {{ ci_upper }}
         ),
         alpha = .1
       ) +
       geom_ribbon(
         data = . %>% filter(!is.na({{ y }})),
         aes(
           ymin = {{ ci_lower }},
           ymax = {{ ci_upper }}
         ),
         alpha = .3
       )
     } else {
         if (use_linerange) {
             p <- p +
                 geom_linerange(
                     aes(ymin = {{ ci_lower }}, ymax = {{ ci_upper }}),
                     alpha = .3
                 )
         }
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

   if(!is.null(hline)) {
     p <- p +
     geom_hline(
       yintercept = hline,
       linetype = "dotted",
       color = "gray50"
     )
   }

   # Set x-axis labels
   # If ndecade_to_years is TRUE, display values of ndecade as ISSP 
   # Environment Module years
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
   # If ndecade_to_years is FALSE, use scales::pretty_breaks() to plot 
   # values appearing in the data
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

 