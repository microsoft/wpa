#' @title Create Ranking
#'
#' @description
#' This function scans a standard query output for groups with high levels of a given Workplace Analytics Metric.
#' Returns a table with a all of groups (across multiple HR attributes) ranked by the specified metric.
#'
#' @param data A Standard Query dataset in the form of a data frame.
#' @param metric Character string containing the name of the metric,
#' e.g. "Collaboration_hours"
#' @param hrvar A list of HR Variables to consider in the scan.
#' Defaults to all HR attributes identified.
#' @param mingroup Numeric value setting the privacy threshold / minimum group size.
#' Defaults to 5.
#' @param return A character vector specifying what to return.
#' Valid values include "table" (default) and "df" (data frame)
#'
#' @import dplyr
#' @import ggplot2
#' @import reshape2
#' @import scales
#' @importFrom stats reorder
#'
#' @family General
#'
#' @return
#' Returns a ggplot object by default, where 'plot' is passed in `return`.
#' When 'table' is passed, a summary table is returned as a data frame.
#'
#' @export

create_rank <- function(data,
                        metric,
                        hrvar = extract_hr(data),
                        mingroup = 5,
                        return = "table"){

  results <-
    data %>% create_bar(
             metric = metric,
             hrvar = hrvar[1],
             mingroup = mingroup,
             return = return, bar_colour = "default")

  results$hrvar <- ""

  results <- results[0,]

  	for (p in hrvar) {
  	table1 <-
  	  data %>%
	  create_bar(metric = metric,
             hrvar = p,
             mingroup = mingroup,
             return = "table", bar_colour = "default")

  	table1$hrvar <- p

  	results <- rbind(results,table1)
  	}

  output <-
    results %>% arrange(desc(get(metric))) %>%
    select(hrvar, everything())

  if(return == "table"){
    return(output)
  } else {
    stop("Invalid `return` argument.")
  }
}
