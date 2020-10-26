#' @title Email Hours distribution
#'
#' @description
#' Analyze Email Hours distribution.
#' Returns a stacked bar plot by default.
#' Additional options available to return a table with distribution elements.
#'
#' @inheritParams create_dist
#'
#' @family Emails
#'
#' @examples
#' ## Return a plot
#' email_dist(sq_data, hrvar = "Organization")
#'
#' ## Return a table
#' email_dist(sq_data, hrvar = "Organization", return = "table")
#'
#' ## Return result with a custom specified breaks
#' email_dist(sq_data, hrvar = "LevelDesignation", cut = c(4, 7, 9))
#'
#' @export

email_dist <- function(data,
                       hrvar = "Organization",
                       mingroup = 5,
                       return = "plot",
                       cut = c(5, 10, 15)) {

  create_dist(data = data,
              metric = "Email_hours",
              hrvar = hrvar,
              mingroup = mingroup,
              return = return,
              cut = cut)

}
