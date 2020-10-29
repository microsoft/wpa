#' @title Distribution of Manager 1:1 Time (Fizzy Drink plot)
#'
#' @description
#' Analyze weekly Manager 1:1 Time distribution, and returns
#' a 'fizzy' scatter plot by default.
#' Additional options available to return a table with distribution elements.
#'
#' @details
#' Uses the metric `After_hours_collaboration_hours`.
#'
#' @inheritParams create_fizz
#'
#' @family Managerial Relations
#'
#' @examples
#' one2one_fizz(sq_data, hrvar = "Organization", return = "table")
#' @export

one2one_fizz <- function(data,
                       hrvar = "Organization",
                       mingroup = 5,
                       return = "plot"){

  cleaned_data <-
    data %>%
    mutate(`Scheduled 1:1 meeting minutes with manager` = Meeting_hours_with_manager_1_on_1 * 60)

  create_fizz(data = cleaned_data,
              metric = "Scheduled 1:1 meeting minutes with manager",
              hrvar = hrvar,
              mingroup = mingroup,
              return = return)

}
