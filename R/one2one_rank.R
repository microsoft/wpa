#' @title Manager 1:1 Time Ranking
#'
#' @description
#' This function scans a standard query output for groups with high levels of 'Manager 1:1 Time'.
#' Returns a table with a all of groups (across multiple HR attributes) ranked by hours of digital collaboration.
#'
#' @details
#' Uses the metric `Meeting_hours_with_manager_1_on_1`.
#' See `create_rank()` for applying the same analysis to a different metric.
#'
#' @inheritParams create_rank
#'
#'
#' @family Managerial Relations
#'
#' @return
#' When 'table' is passed in `return`, a summary table is returned as a data frame.
#'
#' @export

one2one_rank <- function(data,
                         hrvar = extract_hr(data),
                         mingroup = 5,
                         return = "table"){

  data %>%
    create_rank(metric = "Meeting_hours_with_manager_1_on_1",
                hrvar = hrvar,
                mingroup = mingroup,
                return = return)
}
