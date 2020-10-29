#' @title Distribution of Work Week Span (Fizzy Drink plot)
#'
#' @description
#' Analyze Work Week Span distribution, and returns
#' a 'fizzy' scatter plot by default.
#' Additional options available to return a table with distribution elements.
#'
#' @inheritParams create_fizz
#'
#' @family Workloads
#'
#' @examples
#' meeting_fizz(sq_data, hrvar = "Organization", return = "table")
#' @export

workloads_fizz <- function(data,
                           hrvar = "Organization",
                           mingroup = 5,
                           return = "plot"){

  create_fizz(data = data,
              metric = "Workweek_span",
              hrvar = hrvar,
              mingroup = mingroup,
              return = return)

}
