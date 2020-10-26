#' @title Manager 1:1 Time Summary
#'
#' @description
#' Provides an overview analysis of Manager 1:1 Time.
#' Returns a bar plot showing average weekly minutes of Manager 1:1 Time by default.
#' Additional options available to return a summary table.
#'
#' @inheritParams create_bar
#'
#' @family Managerial Relations
#'
#' @return
#' Returns a ggplot object by default, where 'plot' is passed in `return`.
#' When 'table' is passed, a summary table is returned as a data frame.
#'
#' @export

one2one_sum <- function(data,
                        hrvar = "Organization",
                        mingroup = 5,
                        return = "plot"){

  cleaned_data <-
    data %>%
    mutate(`Scheduled 1:1 meeting minutes with manager` = Meeting_hours_with_manager_1_on_1 * 60)


  create_bar(data = cleaned_data,
             hrvar = hrvar,
             mingroup = mingroup,
             metric = "Scheduled 1:1 meeting minutes with manager",
             return = return,
             bar_colour = "darkblue")

}










