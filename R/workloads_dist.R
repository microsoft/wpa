#' @title Distribution of Work Week Span
#'
#' @description
#' Analyze Work Week Span distribution.
#' Returns a stacked bar plot by default.
#' Additional options available to return a table with distribution elements.
#'
#' @inheritParams create_dist
#'
#' @family Workloads
#'
#' @examples
#' workloads_dist(sq_data, hrvar = "Organization", return = "table")
#' @export

workloads_dist <- function(data,
                           hrvar = "Organization",
                           mingroup = 5,
                           return = "plot",
                           cut = c(15, 30, 45)) {

  ## Inherit arguments
  create_dist(data = data,
              metric = "Workweek_span",
              hrvar = hrvar,
              mingroup = mingroup,
              return = return,
              cut = cut)

}
