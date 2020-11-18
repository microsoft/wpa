# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Horizontal 100 percent stacked bar plot (General Purpose)
#'
#' @description
#' Provides an analysis of the distribution of a selected metric.
#' Returns a stacked bar plot by default.
#' Additional options available to return a table with distribution elements.
#'
#' @param data A Standard Person Query dataset in the form of a data frame.
#' @param metric Character string containing the name of the metric,
#' e.g. "Collaboration_hours"
#' @param hrvar HR Variable by which to split metrics. Accepts a character vector, defaults to "Organization" but accepts any character vector, e.g. "LevelDesignation"
#' @param mingroup Numeric value setting the privacy threshold / minimum group size, defaults to 5.
#' @param return Character vector specifying what to return, defaults to "plot".
#' Valid inputs are "plot" and "table".
#' @param cut A numeric vector of length three to specify the breaks for the distribution,
#' e.g. c(10, 15, 20)
#' @param dist_colours A character vector of length four to specify colour
#' codes for the stacked bars.
#' @param unit See `cut_hour()`.
#'
#' @import dplyr
#' @import ggplot2
#' @import reshape2
#' @import scales
#' @importFrom tidyr spread
#' @importFrom stats median
#' @importFrom stats sd
#'
#' @family General
#'
#' @examples
#' ## Return a plot
#' create_dist(sq_data, metric = "Collaboration_hours", hrvar = "Organization")
#'
#' ## Return a table
#' create_dist(sq_data, metric = "Collaboration_hours", hrvar = "Organization", return = "table")
#' @export

create_dist <- function(data,
                        metric,
                        hrvar = "Organization",
                        mingroup = 5,
                        return = "plot",
                        cut = c(15, 20, 25),
                        dist_colours = c("#FE7F4F",
                                         "#ffdfd3",
                                         "#bed6f2",
                                         "#e9f1fb"),
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
  clean_nm <- gsub(pattern = "_", replacement = " ", x = metric)

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

  ## Bar plot

  plot_object <-
    plot_table %>%
    ggplot(aes(x = group, y=Employees, fill = bucket_hours)) +
    geom_bar(stat = "identity", position = position_fill(reverse = TRUE)) +
    scale_y_continuous(labels = function(x) paste0(x*100, "%")) +
    coord_flip() +
    annotate("text", x = plot_legend$group, y = -.05, label = plot_legend$Employee_Count ) +
    scale_fill_manual(name="",
                      values = rev(dist_colours)) +
    theme_wpa_basic() +
    labs(title = clean_nm,
         subtitle = paste("Distribution of", clean_nm, "by", camel_clean(hrvar))) +
    xlab(camel_clean(hrvar)) +
    ylab("Fraction of employees") +
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

#' @rdname collaboration_dist
#' @export
collaboration_distribution <- collaboration_dist
