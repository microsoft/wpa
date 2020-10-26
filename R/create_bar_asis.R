#' @title Create a bar chart without aggregation
#'
#' @description
#' This function creates a bar chart directly from the aggregated / summarised data.
#' Unlike `create_bar()` which performs a person-level aggregation, there is no
#' calculation for `create_bar_asis()` and the values are rendered as they are passed
#' into the function.
#'
#' @param data Plotting data as a data frame.
#' @param group_var String containing name of variable for the group.
#' @param bar_var String containing name of variable representing the value of the bars.
#' @param title Title of the plot.
#' @param subtitle Subtitle of the plot.
#' @param caption Caption of the plot.
#' @param ylab Y-axis label for the plot (group axis)
#' @param xlab X-axis label of the plot (bar axis).
#' @param percent Logical value to determine whether to show labels as percentage signs. Defaults to FALSE.
#' @param bar_colour String to specify colour to use for bars.
#' In-built accepted values include "default" (default), "alert" (red), and
#' "darkblue". Otherwise, hex codes are also accepted. You can also supply
#' RGB values via `rgb2hex()`.
#'
#' @import ggplot2
#' @import dplyr
#'
#' @export
create_bar_asis <- function(data,
                            group_var,
                            bar_var,
                            title = NULL,
                            subtitle = NULL,
                            caption = NULL,
                            ylab = group_var,
                            xlab = bar_var,
                            percent = FALSE,
                            bar_colour = "default"){

  ## Colour bar override
  if(bar_colour == "default"){

    bar_colour <- rgb2hex(0, 120, 212)

  } else if(bar_colour == "alert"){

    bar_colour <- rgb2hex(215, 59, 1)

  } else if(bar_colour == "darkblue"){

    bar_colour <- rgb2hex(0, 32, 80)

  }

  up_break <- max(data[[bar_var]], na.rm = TRUE) * 1.3

  if(percent == FALSE){
    returnPlot <-
      data %>%
      ggplot(aes(x = reorder(!!sym(group_var), !!sym(bar_var)), y = !!sym(bar_var))) +
      geom_col(fill = bar_colour) +
      geom_text(aes(label = round(!!sym(bar_var), 2)),
                hjust = -0.25,
                color = "#000000",
                fontface = "bold",
                size = 4)
  } else if(percent == TRUE){
    returnPlot <-
      data %>%
      ggplot(aes(x = reorder(!!sym(group_var), !!sym(bar_var)), y = !!sym(bar_var))) +
      geom_col(fill = bar_colour) +
      geom_text(aes(label = scales::percent(!!sym(bar_var), accuracy = 1)),
                hjust = -0.25,
                color = "#000000",
                fontface = "bold",
                size = 4)

  }

  returnPlot +
    scale_y_continuous(limits = c(0, up_break)) +
    coord_flip() +
    labs(title = title,
         subtitle = subtitle,
         caption = caption,
         y = xlab,
         x = ylab) +
    theme_wpa_basic()
}
