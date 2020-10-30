#' @title Email Hours Ranking
#'
#' @description
#' This function scans a standard query output for groups with high levels of 'Weekly Email Collaboration'.
#' Returns a table with a all of groups (across multiple HR attributes) ranked by hours of digital collaboration.
#'
#' @details
#' Uses the metric `Email_hours`.
#' See `create_rank()` for applying the same analysis to a different metric.
#'
#' @inheritParams create_rank
#'
#' @family Emails
#'
#' @return
#' When 'table' is passed in `return`, a summary table is returned as a data frame.
#'
#' @export

email_rank <- function(data,
                       hrvar = extract_hr(data),
                       mingroup = 5,
                       return = "table"){

  data %>%
    create_rank(metric = "Email_hours",
                hrvar = hrvar,
                mingroup = mingroup,
                return = return)

}
