# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Identify Inactive Weeks
#'
#' @description
#' This function scans a standard query output for weeks where collaboration hours is far outside the mean for any individual person in the dataset.
#' Returns a list of weeks that appear to be inactive weeks and optionally an edited dataframe with outliers removed.
#'
#' As best practice, run this function prior to any analysis to remove atypical collaboration weeks from your dataset.
#'
#' @param data A Standard Person Query dataset in the form of a data frame.
#' @param sd The standard deviation below the mean for collaboration hours that should define an outlier week. Enter a positive number. Default is 1 standard deviation.
#' @param return String to specify what to return.
#'
#' @import dplyr
#'
#' @family Data Validation
#'
#' @return
#' Returns an error message by default, where 'text' is returned.
#' When 'data_cleaned' is passed, a dataset with outlier weeks removed is returned as a dataframe.
#' When 'data_dirty' is passed, a dataset with outlier weeks is returned as a dataframe.
#'
#' @export
identify_inactiveweeks <- function(data, sd = 2, return = "text"){

  init_data <-
    data %>%
    group_by(PersonId) %>%
    mutate(z_score = (Collaboration_hours - mean(Collaboration_hours))/sd(Collaboration_hours))


  Calc <-
    init_data %>%
    filter(z_score <= -sd) %>%
    select(PersonId, Date, z_score) %>%
    data.frame()

  pop_mean <-
    data %>%
    dplyr::mutate(Total = "Total") %>%
    create_bar(metric = "Collaboration_hours",
               hrvar = "Total",
               return = "table") %>%
    dplyr::pull(Collaboration_hours) %>%
    round(digits = 1)


  Message <- paste0("There are ", nrow(Calc), " rows of data with weekly collaboration hours more than ",
                    sd," standard deviations below the mean (", pop_mean, ").")

  if(return == "text"){

    return(Message)

  } else if(return == "data_dirty"){

    init_data %>%
      filter(z_score <= -sd) %>%
      select(-z_score) %>%
      data.frame()

  } else if(return == "data_cleaned"){

    init_data %>%
      filter(z_score > -sd) %>%
      select(-z_score) %>%
      data.frame()

  } else if(return == "data"){

    init_data %>%
      mutate(inactiveweek = (z_score<= -sd)) %>%
      select(-z_score) %>%
      data.frame()

  } else {

    stop("Error: please check inputs for `return`")

  }
}
