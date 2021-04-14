# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------
#' @title
#' Calculate the p-value of the null hypothesis that two outcomes are from the
#' same dataset
#'
#' @description
#' Specify an outcome variable and return p-test outputs.
#' All numeric variables in the dataset are used as predictor variables.
#'
#' @author Mark Powers <mark.powers@@microsoft.com>
#'
#' @param data A Person Query dataset in the form of a data frame.
#' @param outcome A string specifying the name of a binary variable, i.e. can
#'   only contain the values 1 or 0. Used to group the two distributions.
#' @param behavior A character vector specifying the column to be used as the
#'   behavior to test.
#' @param paired Specify whether the dataset is paired or not. Defaults to
#' `TRUE`.
#'
#' @return
#' Returns a numeric value representing the p-value outcome of the test.
#'
#' @family Support
#'
#' @import dplyr
#'
#' @details
#' This function is a wrapper around `wilcox.test()` from 'stats'.
#'
#' @examples
#' # Simulate a binary variable X
#' # Returns a single p-value
#' library(dplyr)
#' sq_data %>%
#'   mutate(X = ifelse(Email_hours > 6, 1, 0)) %>%
#'   p_test(outcome = "X", behavior = "External_network_size")
#'
#' @export
p_test <- function(data,
                   outcome,
                   behavior,
                   paired = FALSE){

  train <- data %>%
    dplyr::filter(!!sym(outcome) == 1 | !!sym(outcome) == 0) %>%
    select(!!sym(outcome), !!sym(behavior)) %>%
    mutate(outcome = as.character(!!sym(outcome))) %>%
    mutate(outcome = as.factor(!!sym(outcome)))

  pos <- train %>% dplyr::filter(outcome == 1, na.rm=TRUE) %>% select(behavior)
  neg <- train %>% dplyr::filter(outcome == 0, na.rm=TRUE) %>% select(behavior)

  s <- stats::wilcox.test(unlist(pos), unlist(neg), paired = paired)

  return(s$p.value)
}
