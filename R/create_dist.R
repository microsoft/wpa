# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Horizontal 100 percent stacked bar plot for any metric
#'
#' @description
#' Provides an analysis of the distribution of a selected metric.
#' Returns a stacked bar plot by default.
#' Additional options available to return a table with distribution elements.
#'
#' @template spq-params
#' @param metric String containing the name of the metric,
#' e.g. "Collaboration_hours"
#'
#' @param return String specifying what to return. This must be one of the
#'   following strings:
#'   - `"plot"`
#'   - `"table"`
#'
#' See `Value` for more information.
#'
#' @param cut A numeric vector of length three to specify the breaks for the
#'   distribution,
#' e.g. c(10, 15, 20)
#' @param dist_colours A character vector of length four to specify colour
#' codes for the stacked bars.
#' @param unit String to specify what units. This defaults to `"hours"` but can
#'   accept any custom string. See `cut_hour()` for more details.
#'
#' @return
#' A different output is returned depending on the value passed to the `return` argument:
#'   - `"plot"`: 'ggplot' object. A stacked bar plot for the metric.
#'   - `"table"`: data frame. A summary table for the metric.
#'
#' @import dplyr
#' @import ggplot2
#' @import reshape2
#' @import scales
#' @importFrom tidyr spread
#' @importFrom stats median
#' @importFrom stats sd
#'
#' @family Visualization
#' @family Flexible
#'
#' @examples
#' # Return plot
#' create_dist(sq_data, metric = "Collaboration_hours", hrvar = "Organization")
#'
#' # Return summary table
#' create_dist(sq_data, metric = "Collaboration_hours", hrvar = "Organization", return = "table")
#' @export

create_dist <- function(data,
                        metric,
                        hrvar = "Organization",
                        mingroup = 5,
                        return = "plot",
                        cut = c(15, 20, 25),
                        dist_colours = c("#facebc",
                                         "#fcf0eb",
                                         "#b4d5dd",
                                         "#bfe5ee"),
                        unit = "hours") {

  ## Check inputs
  required_variables <- c("Date",
                          metric,
                          "PersonId")

  ## Error message if variables are not present
  ## Nothing happens if all present
  data %>%
    check_inputs(requirements = required_variables)

  ## Clean metric name
  clean_nm <- us_to_space(metric)

  ## Handling NULL values passed to hrvar
  if(is.null(hrvar)){
    data <- totals_col(data)
    hrvar <- "Total"
  }

  ## Basic Data for bar plot
  plot_data <-
    data %>%
    rename(group = !!sym(hrvar)) %>%
    group_by(PersonId, group) %>%
    summarise(!!sym(metric) := mean(!!sym(metric))) %>%
    ungroup() %>%
    left_join(data %>%
                rename(group = !!sym(hrvar)) %>%
                group_by(group) %>%
                summarise(Employee_Count = n_distinct(PersonId)),
              by = "group") %>%
    filter(Employee_Count >= mingroup)

  ## Create buckets of collaboration hours
  plot_data <-
    plot_data %>%
    mutate(bucket_hours = cut_hour(!!sym(metric), cuts = cut, unit = unit))

  ## Employee count / base size table
  plot_legend <-
    plot_data %>%
    group_by(group) %>%
    summarize(Employee_Count = first(Employee_Count)) %>%
    mutate(Employee_Count = paste("n=",Employee_Count))

  ## Data for bar plot
  plot_table <-
    plot_data %>%
    group_by(group, bucket_hours) %>%
    summarize(Employees = n(),
              Employee_Count = first(Employee_Count),
              percent = Employees / Employee_Count ) %>%
    arrange(group, desc(bucket_hours))

  ## Table for annotation
  annot_table <-
    plot_legend %>%
    dplyr::left_join(plot_table, by = "group")

  ## Remove max from axis labels, and add %
max_blank <- function(x){
  as.character(
    c(
      scales::percent(
        x[1:length(x) - 1]
        ),
      "")
  )
}
 
  # paste0(x*100, "%") 

  ## Bar plot

  plot_object <-
    plot_table %>%
    ggplot(aes(x = group, y=Employees, fill = bucket_hours)) +
    geom_bar(stat = "identity", position = position_fill(reverse = TRUE)) +
    scale_y_continuous(expand = c(.01, 0), labels = max_blank, position = "right") +
    coord_flip() +
    annotate("text", x = plot_legend$group, y = 1.15, label = plot_legend$Employee_Count, size=3 ) +
	annotate("rect", xmin = 0.5, xmax = length(plot_legend$group) + 0.5, ymin = 1.05, ymax = 1.25, alpha = .2) +
	annotate(x=length(plot_legend$group) + 0.8, xend=length(plot_legend$group) + 0.8, y=0, yend=1, colour="black", lwd=0.75, geom="segment") +
    scale_fill_manual(name="",
                      values = rev(dist_colours)) +
    theme_wpa_basic() +
	theme(axis.line = element_blank(),   
		axis.ticks = element_blank(),   
		axis.title = element_blank()) +
    labs(title = clean_nm,
         subtitle = paste("Percentage of employees by", tolower(camel_clean(hrvar)))) +
    xlab(camel_clean(hrvar)) +
    labs(caption = extract_date_range(data, return = "text"))

  ## Table to return
  return_table <-
    plot_table %>%
    select(group, bucket_hours, percent) %>%
    spread(bucket_hours,  percent) %>%
    left_join(data %>%
                rename(group = !!sym(hrvar)) %>%
                group_by(group) %>%
                summarise(Employee_Count = n_distinct(PersonId)),
              by = "group")


  if(return == "table"){

    return_table %>%
      as_tibble() %>%
      return()

  } else if(return == "plot"){

    return(plot_object)

  } else {

    stop("Please enter a valid input for `return`.")

  }
}
