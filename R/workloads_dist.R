#' @title Work Week Span distribution
#'
#' @description
#' Analyze Work Week Span distribution.
#' Returns a a 'fizzy' scatter plot by default.
#' Additional options available to return a table with distribution elements.
#'
#' @inheritParams create_fizz
#'
#' @family Workloads
#'
#' @examples
#' workloads_dist(sq_data, hrvar = "Organization", return = "table")
#' @export

workloads_dist <- function(data, hrvar = "Organization", mingroup = 5, return = "plot") {

  ## Inherit arguments
  create_fizz(data = data,
              metric = "Workweek_span",
              hrvar = hrvar,
              mingroup = mingroup,
              return = return)

}

#' @rdname workloads_dist
#' @export
workloads_distribution <- workloads_dist
