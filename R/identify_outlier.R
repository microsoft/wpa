# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Identify metric outliers over a date interval
#'
#' @description This function takes in a selected metric and uses
#' z-score (number of standard deviations) to identify outliers
#' across time. There are applications in this for identifying
#' weeks with abnormally low collaboration activity, e.g. holidays.
#' Time as a grouping variable can be overridden with the `group_var`
#' argument.
#'
#' @param data A Standard Person Query dataset in the form of a data frame.
#' @param group_var A string with the name of the grouping variable.
#' Defaults to `Date`.
#' @param metric Character string containing the name of the metric,
#' e.g. "Collaboration_hours"
#'
#' @import dplyr
#'
#' @examples
#' identify_outlier(sq_data, metric = "Collaboration_hours")
#'
#' @return
#' Returns a data frame with `Date` (if grouping variable is not set),
#' the metric, and the corresponding z-score.
#'
#' @family General
#' @family Data Validation
#'
#' @export
identify_outlier <- function(data,
                             group_var = "Date",
                             metric = "Collaboration_hours"){

  ## Check inputs
  required_variables <- c(group_var,
                          "PersonId",
                          metric)

  ## Error message if variables are not present
  ## Nothing happens if all present
  data %>%
    check_inputs(requirements = required_variables)

  main_table <-
    data %>%
    group_by(!!sym(group_var)) %>%
    summarise_at(vars(!!sym(metric)), ~mean(.)) %>%
    mutate(zscore = (!!sym(metric) - mean(!!sym(metric)))/sd(!!sym(metric)))

  return(main_table)
}

