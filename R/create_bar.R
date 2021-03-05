# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Mean Bar Plot for any metric
#'
#' @description
#' Provides an overview analysis of a selected metric by calculating a mean per
#' metric.
#' Returns a bar plot showing the average of a selected metric by default.
#' Additional options available to return a summary table.
#'
#' @template spq-params
#' @param mingroup Numeric value setting the privacy threshold / minimum group
#'   size. Defaults to 5.
#' @param metric Character string containing the name of the metric,
#' e.g. "Collaboration_hours"
#' @param return String specifying what to return. This must be one of the
#'   following strings:
#'   - `"plot"`
#'   - `"table"`
#'
#' See `Value` for more information.
#'
#' @param bar_colour String to specify colour to use for bars.
#' In-built accepted values include `"default"` (default), `"alert"` (red), and
#' `"darkblue"`. Otherwise, hex codes are also accepted. You can also supply
#' RGB values via `rgb2hex()`.
#' @param na.rm A logical value indicating whether `NA` should be stripped
#' before the computation proceeds. Defaults to `FALSE`.
#'
#' @return
#' A different output is returned depending on the value passed to the `return` argument:
#'   - `"plot"`: ggplot object. A bar plot for the metric.
#'   - `"table"`: data frame. A summary table for the metric.
#'
#' @import dplyr
#' @import ggplot2
#' @import reshape2
#' @importFrom scales wrap_format
#' @importFrom stats reorder
#'
#' @family Visualization
#' @family Flexible
#'
#' @examples
#' # Return a ggplot bar chart
#' create_bar(sq_data, metric = "Collaboration_hours", hrvar = "LevelDesignation")
#'
#' # Change bar colour
#' create_bar(sq_data,
#'            metric = "After_hours_collaboration_hours",
#'            bar_colour = "alert")
#'
#' # Return a summary table
#' create_bar(sq_data,
#'            metric = "Collaboration_hours",
#'            hrvar = "LevelDesignation",
#'            return = "table")
#' @export
create_bar <- function(data,
                       metric,
                       hrvar = "Organization",
                       mingroup = 5,
                       return = "plot",
                       bar_colour = "default",
                       na.rm = FALSE){

  ## Check inputs
  required_variables <- c("Date",
                          metric,
                          "PersonId")

  ## Error message if variables are not present
  ## Nothing happens if all present
  data %>%
    check_inputs(requirements = required_variables)

  ## Handling NULL values passed to hrvar
  if(is.null(hrvar)){
    data <- totals_col(data)
    hrvar <- "Total"
  }

  ## Clean metric name
  clean_nm <- us_to_space(metric)

  ## Data for bar plot
  plot_data <-
    data %>%
    rename(group = !!sym(hrvar)) %>%
    group_by(PersonId, group) %>%
    summarise(!!sym(metric) := mean(!!sym(metric), na.rm = na.rm)) %>%
    ungroup() %>%
    left_join(data %>%
                rename(group = !!sym(hrvar)) %>%
                group_by(group) %>%
                summarise(Employee_Count = n_distinct(PersonId)),
              by = "group") %>%
    filter(Employee_Count >= mingroup)

  ## Colour bar override
  if(bar_colour == "default"){

    bar_colour <- "#34b1e2"

  } else if(bar_colour == "alert"){

    bar_colour <- "#FE7F4F"

  } else if(bar_colour == "darkblue"){

    bar_colour <- "#1d627e"

  }

  ## Employee count / base size table
  plot_legend <-
    plot_data %>%
    group_by(group) %>%
    summarize(Employee_Count = first(Employee_Count)) %>%
    mutate(Employee_Count = paste("n=",Employee_Count))

  ## Data for bar plot
  plot_table <-
    plot_data %>%
    group_by(group) %>%
    summarise_at(metric, ~mean(., na.rm = na.rm)) %>%
    arrange(desc(!!sym(metric)))

  ## Table for annotation
  annot_table <-
    plot_legend %>%
    dplyr::left_join(plot_table, by = "group")

  ## Location attribute for x axis
 location <- plot_table %>% select(!!sym(metric)) %>% max()

  ## Bar plot
  plot_object <-
    plot_table %>%
    ggplot(aes(x = stats::reorder(group, !!sym(metric)), y = !!sym(metric))) +
    geom_bar(stat = "identity",
             fill = bar_colour) +
    geom_text(aes(label = round(!!sym(metric), 1)),
              hjust = 1.3,
              color = "#FFFFFF",
              fontface = "bold",
              size = 4) +
    scale_y_continuous(limits = c(0, location * 1.25)) +
    annotate("text",
             x = plot_legend$group,
             y = location * 1.15,
             label = plot_legend$Employee_Count) +
    annotate("rect", xmin = 0.5, xmax = length(plot_legend$group) + 0.5, ymin = location * 1.05, ymax = location * 1.25, alpha = .2) +
    coord_flip() +
    theme_classic() +
    theme(axis.text = element_text(size=12),
          plot.title = element_text(color="grey40", face="bold", size=18),
          plot.subtitle = element_text(size=14),
          legend.position = "top",
          legend.justification  = "right",
          legend.title = element_text(size=14),
          legend.text = element_text(size=14)) +
    labs(title = clean_nm,
         subtitle = paste("Average", clean_nm, "by", camel_clean(hrvar))) +
    xlab(camel_clean(hrvar)) +
    ylab(paste("Average weekly", clean_nm)) +
    labs(caption = extract_date_range(data, return = "text"))

  summary_table <-
    plot_data %>%
    select(group, !!sym(metric)) %>%
    group_by(group) %>%
    summarise(!!sym(metric) := mean(!!sym(metric)),
              n = n())

  if(return == "table"){

    return(summary_table)

    } else if(return == "plot"){

    return(plot_object)

    } else {

    stop("Please enter a valid input for `return`.")

    }
}
