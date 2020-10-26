#' @title Convert a numeric variable for hours into categorical
#'
#' @description
#' Supply a numeric variable, e.g. `Collaboration_hours`,
#' and the function returns a character vector
#'
#' @param metric A numeric variable representing hours.
#' @param cuts A numeric variable of length 3 to represent the
#' cut points required.
#'
#' @family General
#'
#' @examples
#' ## Direct use
#' cut_hour(1:30, cuts = c(15, 20, 25))
#'
#' ## Use on a query
#' cut_hour(sq_data$Collaboration_hours, cuts = c(10, 15, 20))
#'
#' @export

cut_hour <- function(metric, cuts){


  label1 <- paste0("< ", cuts[1], " hours")
  label2 <- paste0(cuts[1], " - ", cuts[2], " hours")
  label3 <- paste0(cuts[2], " - ", cuts[3], " hours")
  label4 <- paste0(cuts[3], "+ hours")

  out <-
    cut(metric,
        breaks = c(0, cuts, 100),
        include.lowest = TRUE,
        labels = c(label1,
                   label2,
                   label3,
                   label4))

  # out <- as.character(out)
  return(out)
}


