# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Manager 1:1 Time Trend distribution
#'
#' @description
#' Analyze Manager 1:1 Time distribution.
#' Returns a stacked bar plot of different buckets of 1:1 time.
#' Additional options available to return a table with distribution elements.
#'
#' @param data A Standard Query dataset in the form of a data frame.
#' @param hrvar HR Variable by which to split metrics. Accepts a character vector, defaults to "Organization" but accepts any character vector, e.g. "LevelDesignation"
#' @param mingroup Numeric value setting the privacy threshold / minimum group size, defaults to 5.
#' @param return Character vector specifying what to return, defaults to "plot".
#' Valid inputs are "plot" and "table".
#'
#' @import dplyr
#' @import ggplot2
#' @import reshape2
#' @import scales
#' @importFrom stats median
#' @importFrom stats sd
#'
#' @family Managerial Relations
#'
#' @examples
#' workloads_dist(sq_data, hrvar = "Organization", return = "table")
#' @export

one2one_dist <- function(data, hrvar = "Organization", mingroup = 5, return = "plot") {

  ## Date range data frame
  myPeriod <- extract_date_range(data)

  ## Basic Data for bar plot
  plot_data <-
    data %>%
    rename(group = !!sym(hrvar)) %>%
    group_by(PersonId, group) %>%
    summarise(Minutes_with_manager_1_on_1 = mean(Meeting_hours_with_manager_1_on_1 * 60)) %>%
    ungroup() %>%
    left_join(data %>%
                rename(group = !!sym(hrvar)) %>%
                group_by(group) %>%
                summarise(Employee_Count = n_distinct(PersonId)),
              by = "group") %>%
    filter(Employee_Count >= mingroup)

  ## Create buckets of 1:1 time
  plot_data <-
    plot_data %>%
    mutate(bucket_manager_1_on_1 = case_when(Minutes_with_manager_1_on_1 == 0 ~ "0 minutes",
                                             Minutes_with_manager_1_on_1 > 0 &  Minutes_with_manager_1_on_1 < 15 ~ "1 - 15 minutes",
                                             Minutes_with_manager_1_on_1 >= 15 & Minutes_with_manager_1_on_1 < 30 ~ "15 - 30 minutes",
                                             Minutes_with_manager_1_on_1 >= 30 ~ "30 min +"))


  ## Employee count / base size table
  plot_legend <-
    plot_data %>%
    group_by(group) %>%
    summarize(Employee_Count=first(Employee_Count)) %>%
    mutate(Employee_Count = paste("n=",Employee_Count))

  ## Data for bar plot
  plot_table <-
    plot_data %>%
    group_by(group, bucket_manager_1_on_1) %>%
    summarize(Employees = n(),
              Employee_Count = first(Employee_Count),
              percent = Employees / Employee_Count) %>%
    arrange(group, desc(bucket_manager_1_on_1))

  ## Table for annotation
  annot_table <-
    plot_legend %>%
    dplyr::left_join(plot_table, by = "group")

  ## Bar plot
  plot_object <-
    plot_table %>%
    ggplot(aes(x = group, y=Employees, fill = bucket_manager_1_on_1)) +
    geom_bar(stat = "identity", position = position_fill(reverse = TRUE)) +
	scale_y_continuous(labels = function(x) paste0(x*100, "%")) +
	coord_flip() +
	annotate("text", x = plot_legend$group, y = -.05, label = plot_legend$Employee_Count ) +
	scale_fill_manual(name="", values = c("#FE7F4F", "#ffdfd3", "#bed6f2", "#e9f1fb")) +
  theme_wpa_basic() +
	labs(title = "Time with Manager", subtitle = paste("Scheduled 1:1 weekly meeting minutes by", tolower(hrvar))) +
	xlab(hrvar) +
	ylab("Fraction of employees") +
	labs(caption = paste("Data from week of", myPeriod$Start, "to week of", myPeriod$End))

  ## Table to return
  return_table <-  plot_table %>% select(group, bucket_manager_1_on_1,  percent) %>% spread(bucket_manager_1_on_1,  percent)


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
