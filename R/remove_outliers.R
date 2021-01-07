# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Remove outliers from a person query across time
#'
#' @description This function takes in a selected metric and uses z-score (number of standard
#' deviations) to identify and remove outlier weeks for individuals across time. There are applications
#' in this for removing weeks with abnormally low collaboration
#' activity, e.g. holidays. Retains metrics with z > -2.
#'
#' Function is based on `identify_outlier()`, but implements a more elaborate approach as the outliers are
#' identified and removed **with respect to each individual**, as opposed to the group. Note that `remove_outliers()`
#' has a longer runtime compared to `identify_outlier()`.
#'
#' @details
#' This function is on an experimental lifecycle.
#' For mature functions to remove common outliers, please see the following:
#' - `identify_holidayweeks()`
#' - `identify_nkw()`
#' - `identify_inactiveweeks`
#'
#' @param data A Standard Person Query dataset in the form of a data frame.
#' @param metric Character string containing the name of the metric,
#' e.g. "Collaboration_hours"
#'
#' @import dplyr
#' @family Data Validation
#'
#' @return
#' Returns a new data frame, "cleaned_data" with all metrics,
#' having removed the person-weeks that are below 2 standard
#' deviations of each individual's collaboration activity.
#'
#' @export
#'
remove_outliers <- function(data, metric = "Collaboration_hours") {

  remove_outlier <- function(data, metric = "Collaboration_hours") {
    output <-
      identify_outlier(data, group_var = "Date", metric = metric)

    output %>%
      filter(zscore > -2) %>%
      select(Date) %>%
      mutate(PersonId = data$PersonId[[1]]) # Take first one
  }

  p_list <-
    data %>%
    select(Date, PersonId, metric) %>%
    group_split(PersonId)

  personweeks <-
    p_list[1:length(p_list)] %>% # Increase as required
    purrr::map(remove_outlier) %>%
    bind_rows() %>%
    mutate(key = paste(Date, PersonId)) %>%
    select(key)

  cleaned_data <-
    data %>%
    mutate(key = paste(Date, PersonId)) %>%
    filter(key %in% personweeks$key) %>%
    select(-key)

  return(cleaned_data)

}
