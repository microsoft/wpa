#' @title Rank all groups across HR attributes on a selected Workplace Analytics metric
#'
#' @description
#' This function scans a standard Person query output for groups with high levels of a given Workplace Analytics Metric.
#' Returns a table with all groups (across multiple HR attributes) ranked by the specified metric.
#'
#' @param data A Standard Person Query dataset in the form of a data frame.
#' @param metric Character string containing the name of the metric,
#' e.g. "Collaboration_hours"
#' @param hrvar A list of HR Variables to consider in the scan.
#' Defaults to all HR attributes identified.
#' @param mingroup Numeric value setting the privacy threshold / minimum group size.
#' Defaults to 5.
#' @param return A character vector specifying what to return.
#' Valid values include "table" (default). Features are being considered for alternative return options but are currently
#' unavailable.
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
    create_bar(data,
               metric = metric,
               hrvar = hrvar[1],
               mingroup = mingroup,
               return = "table")

  ## Create a blank column
  results$hrvar <- ""

  ## Empty table
  results <- results[0,]

  ## Loop through each HR attribute supplied in argument
  for (p in hrvar) {
  	table1 <-
  	  data %>%
  	  create_bar(metric = metric,
  	             hrvar = p,
  	             mingroup = mingroup,
  	             return = "table")

  	table1$hrvar <- p

  	results <- rbind(results,table1)
  	}

  output <-
    results %>%
    arrange(desc(get(metric))) %>%
    select(hrvar, everything())

  if(return == "table"){
    return(output)
  } else {
    stop("Invalid `return` argument.")
  }
}
