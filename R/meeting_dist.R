#' @title Meeting Hours distribution
#'
#' @description
#' Analyze Meeting Hours distribution.
#' Returns a stacked bar plot by default.
#' Additional options available to return a table with distribution elements.
#'
#' @inheritParams create_dist
#'
#' @family Meeting Culture
#'
#' @examples
#' ## Return a plot
#' meeting_dist(sq_data, hrvar = "Organization")
#'
#' ## Return a table
#' meeting_dist(sq_data, hrvar = "Organization", return = "table")
#'
#' ## Return result with a custom specified breaks
#' meeting_dist(sq_data, hrvar = "LevelDesignation", cut = c(4, 7, 9))
#'
#'
#' @export

meeting_dist <- function(data,
                         hrvar = "Organization",
                         mingroup = 5,
                         return = "plot",
                         cut = c(5, 10, 15)) {

  create_dist(data = data,
              metric = "Meeting_hours",
              hrvar = hrvar,
              mingroup = mingroup,
              return = return,
              cut = cut)
}

#' @rdname meeting_dist
#' @export
meeting_distribution <- meeting_dist
