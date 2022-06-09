# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Run a summary of Key Metrics from the Standard Person Query data
#'
#' @description
#' Returns a heatmapped table by default, with options to return a table.
#'
#' @template spq-params
#' @param metrics A character vector containing the variable names to calculate
#'   averages of.
#' @param return Character vector specifying what to return, defaults to "plot".
#'   Valid inputs are "plot" and "table".
#' @param low String specifying colour code to use for low-value metrics.
#'   Arguments are passed directly to `ggplot2::scale_fill_gradient2()`.
#' @param mid String specifying colour code to use for mid-value metrics.
#'   Arguments are passed directly to `ggplot2::scale_fill_gradient2()`.
#' @param high String specifying colour code to use for high-value metrics.
#'   Arguments are passed directly to `ggplot2::scale_fill_gradient2()`.
#' @param textsize A numeric value specifying the text size to show in the plot.
#'
#' @import dplyr
#' @import ggplot2
#' @import reshape2
#' @importFrom stats reorder
#'
#' @return
#' Returns a ggplot object by default, when `'plot'` is passed in `return`.
#' When `'table'` is passed, a summary table is returned as a data frame.
#'
#' @family Visualization
#'
#' @examples
#' # Heatmap plot is returned by default
#' keymetrics_scan(sq_data)
#'
#' # Heatmap plot with custom colours
#' keymetrics_scan(sq_data, low = "purple", high = "yellow")
#'
#' # Return summary table
#' keymetrics_scan(sq_data, hrvar = "LevelDesignation", return = "table")
#'
#' @export

keymetrics_scan <- function(data,
                            hrvar = "Organization",
                            mingroup = 5,
                            metrics = c("Workweek_span",
                                        "Collaboration_hours",
                                        "After_hours_collaboration_hours",
                                        "Meetings",
                                        "Meeting_hours",
                                        "After_hours_meeting_hours",
                                        "Low_quality_meeting_hours",
                                        "Meeting_hours_with_manager_1_on_1",
                                        "Meeting_hours_with_manager",
                                        "Emails_sent",
                                        "Email_hours",
                                        "After_hours_email_hours",
                                        "Generated_workload_email_hours",
                                        "Total_focus_hours",
                                        "Internal_network_size",
                                        "Networking_outside_organization",
                                        "External_network_size",
                                        "Networking_outside_company"),
                            return = "plot",
                            low = rgb2hex(7, 111, 161),
                            mid = rgb2hex(241, 204, 158),
                            high = rgb2hex(216, 24, 42),
                            textsize = 2){

  ## Handling NULL values passed to hrvar
  if(is.null(hrvar)){
    data <- totals_col(data)
    hrvar <- "Total"
  }

  ## Omit if metrics do not exist in dataset
  metrics <- dplyr::intersect(metrics, names(data))


  ## Summary table
  myTable <-
    data %>%
    rename(group = !!sym(hrvar)) %>% # Rename HRvar to `group`
    group_by(group, PersonId) %>%
    summarise_at(vars(metrics), ~mean(., na.rm = TRUE)) %>%
    group_by(group) %>%
    summarise_at(vars(metrics), ~mean(., na.rm = TRUE)) %>%
    left_join(hrvar_count(data, hrvar = hrvar, return = "table") %>%
                rename(Employee_Count = "n"),
              by = c("group" = hrvar)) %>%
    filter(Employee_Count >= mingroup)  # Keep only groups above privacy threshold

  myTable %>%
    reshape2::melt(id.vars = "group") %>%
    reshape2::dcast(variable ~ group) -> myTable_wide

  myTable_long <- reshape2::melt(myTable, id.vars=c("group")) %>%
    mutate(variable = factor(variable)) %>%
    group_by(variable) %>%
    # Heatmap by row
    mutate(value_rescaled = maxmin(value)) %>%
    ungroup()

  plot_object <-
    myTable_long %>%
    filter(variable != "Employee_Count") %>%
    ggplot(aes(x = group,
               y = stats::reorder(variable, desc(variable)))) +
    geom_tile(aes(fill = value_rescaled),
              colour = "#FFFFFF",
              size = 2) +
    geom_text(aes(label=round(value, 1)), size = textsize) +
    # Fill is contingent on max-min scaling
    scale_fill_gradient2(low = low,
                         mid = mid,
                         high = high,
                         midpoint = 0.5,
                         breaks = c(0, 0.5, 1),
                         labels = c("Minimum", "", "Maximum"),
                         limits = c(0, 1)) +
    scale_x_discrete(position = "top") +
    scale_y_discrete(labels = us_to_space) +
    theme_wpa_basic() +
    theme(axis.line = element_line(color = "#FFFFFF")) +
    labs(title = "Key metrics",
         subtitle = paste("Weekly average by", camel_clean(hrvar)),
         y =" ",
         x =" ",
         fill = " ",
         caption = extract_date_range(data, return = "text")) +
    theme(axis.text.x = element_text(angle = 90, hjust = 0),
          plot.title = element_text(color="grey40", face="bold", size=20))


  if(return == "table"){

    myTable_wide %>%
      as_tibble() %>%
      return()

  } else if(return == "plot"){

    return(plot_object)

  } else {

    stop("Please enter a valid input for `return`.")

  }

}
