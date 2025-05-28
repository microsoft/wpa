# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Identify groups under privacy threshold
#'
#' @description
#' This function scans a standard query output for groups with of employees
#' under the privacy threshold. The method consists in reviewing each individual
#' HR attribute, and count the distinct people within each group.
#'
#' @param data A Standard Person Query dataset in the form of a data frame.
#' @param hrvar A list of HR Variables to consider in the scan.
#' Defaults to all HR attributes identified.
#' @param mingroup Numeric value setting the privacy threshold / minimum group
#'   size. Defaults to 5.
#' @param return String specifying what to return. This must be one of the
#'   following strings:
#'   - `"table"`
#'   - `"text"`
#'
#' See `Value` for more information.
#'
#' @return
#' A different output is returned depending on the value passed to the `return`
#' argument:
#'   - `"table"`: data frame. A summary table of groups that fall below the
#'   privacy threshold.
#'   - `"text"`: string. A diagnostic message.
#'
#' @examples
#' # Return a summary table
#' dv_data %>% identify_privacythreshold(return = "table")
#'
#' # Return a diagnostic message
#' dv_data %>% identify_privacythreshold(return = "text")
#'
#' @import dplyr
#' @import ggplot2
#' @import reshape2
#' @import scales
#' @importFrom stats reorder
#'
#' @family Data Validation
#'
#' @return
#' Returns a ggplot object by default, where 'plot' is passed in `return`.
#' When 'table' is passed, a summary table is returned as a data frame.
#'
#' @export

identify_privacythreshold <- function(data,
                        hrvar = extract_hr(data),
                        mingroup = 5,
                        return = "table"){

  results <-
    data %>% hrvar_count(
             hrvar = hrvar[1],
             return = "table")

  results$hrvar <- ""

  results <- results[0,]

  	for (p in hrvar) {
  	table1 <-
  	  data %>%
	  hrvar_count(hrvar = p,
             return = "table")

  	table1$hrvar <- p
	colnames(table1)[1] <- "group"

  	results <- rbind(results,table1)
  	}

 output <-  results %>% arrange(n) %>% select(hrvar, everything())

 groups_under <-  results %>% filter(n<mingroup) %>% nrow()

 MinGroupFlagMessage_Warning <- paste0("[Warning]  There are ", groups_under, " groups under the minimum group size privacy threshold of ", mingroup, ".")
 MinGroupFlagMessage_Low <- paste0("[Pass] There is only ", groups_under, " group under the minimum group size privacy threshold of ", mingroup, ".")
 MinGroupFlagMessage_Zero <- paste0("[Pass] There are no groups under the minimum group size privacy threshold of ", mingroup, ".")


  if(groups_under > 1){
    MinGroupFlagMessage <- MinGroupFlagMessage_Warning
  } else if(groups_under == 1 ){
    MinGroupFlagMessage <- MinGroupFlagMessage_Low
  } else if(groups_under ==0){
    MinGroupFlagMessage <- MinGroupFlagMessage_Zero
  }

  if(return == "table"){
    return(output)
  } else if(return == "message"){
    message(MinGroupFlagMessage)
  } else if(return == "text"){
    MinGroupFlagMessage
  } else {
    stop("Invalid `return` argument.")
  }
}
