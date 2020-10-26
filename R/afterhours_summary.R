#' @title Summary of After-Hours Collaboration Hours
#'
#' @description
#' Provides an overview analysis of after-hours collaboration time.
#' Returns a bar plot showing average weekly after-hours collaboration hours by default.
#' Additional options available to return a summary table.
#'
#' @details
#' Uses the metric \code{After_hours_collaboration_hours}.
#'
#' @inheritParams create_bar
#'
#' @family After-Hours
#'
#' @return
#' Returns a ggplot object by default, where 'plot' is passed in `return`.
#' When 'table' is passed, a summary table is returned as a data frame.
#'
#' @examples
#' # Return a ggplot bar chart
#' afterhours_summary(sq_data, hrvar = "LevelDesignation")
#'
#' # Return a summary table
#' afterhours_summary(sq_data, hrvar = "LevelDesignation", return = "table")
#' @export
afterhours_summary <- function(data,
                              hrvar = "Organization",
                              mingroup = 5,
                              return = "plot"){
  create_bar(data = data,
             metric = "After_hours_collaboration_hours",
             hrvar = hrvar,
             mingroup = mingroup,
             return = return,
             bar_colour = "alert")

}


#' @rdname afterhours_summary
#' @export
afterhours_sum <- afterhours_summary





