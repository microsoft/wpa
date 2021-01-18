# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Reorder a value to the top of the summary table
#'
#' @description For a given data frame, reorder a row to the first row
#' of that data frame through matching a _value_ of a _variable_. The intended
#' usage of this function is to be used for reordering the "Total" row,
#' and _not_ with "flat" data. This can be used in conjunction with `totals_bind()`,
#' which is used to create a "Total" row in the data.
#'
#' @examples
#' sq_data %>%
#'   totals_bind(target_col = "LevelDesignation",
#'               target_value = "Total") %>%
#'   collab_sum(hrvar = "LevelDesignation",
#'              return = "table") %>%
#'   totals_reorder(target_col = "group", target_value = "Total")
#'
#' @param data Summary table in the form of a data frame.
#' @param target_col Character value of the column in which to reorder
#' @param target_value Character value of the value in `target_col` to match
#'
#' @export
totals_reorder <- function(data, target_col, target_value = "Total"){
  tc <- unique(data[[target_col]])
  the_rest <- tc[tc != target_value]
  order <- c(target_value, the_rest)
  dplyr::slice(data, match(order, !!sym(target_col)))
}
