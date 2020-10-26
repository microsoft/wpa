#' @title Employee count over time
#'
#' @description Returns a line chart showing the change in
#' employee count over time. Part of a data validation process to check
#' for unusual license growth / declines over time.
#'
#' @param data A Standard Query dataset in the form of a data frame.
#' @param return Character vector specifying what to return, defaults to "plot".
#' Valid inputs are "plot" and "table".
#'
#' @import dplyr
#' @import ggplot2
#'
#' @examples
#' \dontrun{
#' hr_trend(sq_data)
#' hr_trend(dv_data)
#' hr_trend(dv_data, return = "table")
#' }
#'
#' @return
#' Returns a ggplot object by default, where 'plot' is passed in `return`.
#' When 'table' is passed, a summary table is returned as a data frame.
#'
#' @export
hr_trend <- function(data, return = "plot"){

  options(dplyr.summarise.inform = FALSE)

  data$Date <- as.Date(data$Date, format = "%m/%d/%Y")

  ## Date range data frame
  myPeriod <- extract_date_range(data)

  plot_data <-
    data %>%
    group_by(Date) %>%
    summarise(n = n_distinct(PersonId)) %>%
    ungroup()

  if(return == "plot"){
    plot_data %>%
      ggplot(aes(x = Date, y = n)) +
      geom_line(size = 1) +
      labs(title = "Population over time",
           subtitle = "Unique licensed population by week",
           caption = paste("Data from week of", myPeriod$Start, "to week of", myPeriod$End)) +
      ylab("Employee count") +
      xlab("Date") +
      scale_y_continuous(labels = round, limits = c(0,NA)) +
      theme_wpa_basic()

  } else if(return == "table"){
    plot_data
  } else {
    stop("Please enter a valid input for `return`.")
  }
}
