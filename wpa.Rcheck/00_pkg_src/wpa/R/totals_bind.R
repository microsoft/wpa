# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Row-bind an identical data frame for computing grouped totals
#'
#' @description Row-bind an identical data frame and impute a specific
#' column with the `target_value`, which defaults as "Total". The purpose of
#' this is to enable to creation of summary tables with a calculated "Total"
#' row. See example below on usage.
#'
#' @examples
#' sq_data %>%
#'   totals_bind(target_col = "LevelDesignation", target_value = "Total") %>%
#'   collab_sum(hrvar = "LevelDesignation", return = "table")
#'
#' @param data data frame
#' @param target_col Character value of the column in which to impute `"Total"`.
#'   This is usually the intended grouping column.
#' @param target_value Character value to impute in the new data frame to
#'   row-bind. Defaults to `"Total"`.
#'
#' @return
#' data frame with twice the number of rows of the input data frame, where half
#' of those rows will have the `target_col` column imputed with the value from
#' `target_value`.
#'
#' @family Support
#'
#' @export
totals_bind <- function(data, target_col, target_value = "Total"){
  data %>%
    dplyr::bind_rows(dplyr::mutate(., !!sym(target_col) := target_value))
}
