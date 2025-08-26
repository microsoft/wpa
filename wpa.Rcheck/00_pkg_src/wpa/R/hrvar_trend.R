# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Track count of distinct people over time in a specified HR variable
#'
#' @description
#' This function provides a week by week view of the count of the distinct
#' people by the specified HR attribute.The default behaviour is to return a
#' week by week heatmap bar plot.
#'
#' @param data A Standard Person Query dataset in the form of a data frame.
#' @param hrvar HR Variable by which to split metrics, defaults to
#'   "Organization" but accepts any character vector, e.g. "LevelDesignation".
#'   If a vector with more than one value is provided, the HR attributes are
#'   automatically concatenated.
#' @param return String specifying what to return. This must be one of the
#'   following strings:
#'   - `"plot"`
#'   - `"table"`
#'
#' See `Value` for more information.
#'
#' @return
#' A different output is returned depending on the value passed to the `return`
#' argument:
#'   - `"plot"`: 'ggplot' object containing a bar plot.
#'   - `"table"`: data frame containing a count table.
#'
#' @import ggplot2
#' @import dplyr
#' @importFrom data.table ":=" "%like%" "%between%"
#'
#' @family Visualization
#' @family Data Validation
#'
#' @examples
#' # Return a bar plot
#' hrvar_trend(sq_data, hrvar = "LevelDesignation")
#'
#' # Return a summary table
#' hrvar_trend(sq_data, hrvar = "LevelDesignation", return = "table")
#'
#'@export

hrvar_trend <- function(data,
                        hrvar = "Organization",
                        return = "plot"){

  ## Allow multiple HRvar inputs
  if(length(hrvar) > 1){
    hrvar_flat <- paste(hrvar, collapse = ", ")

    summary_table <-
      data %>%
      select(PersonId, Date, all_of(hrvar)) %>%
      mutate(!!sym(hrvar_flat) := select(., hrvar) %>%
               apply(1, paste, collapse = ", ")) %>%
      group_by(Date, !!sym(hrvar_flat)) %>%
      summarise(n = n_distinct(PersonId)) %>%
      arrange(desc(n))

    # Single reference for single and multiple org attributes
    hrvar_label <- hrvar_flat

  } else {

    summary_table <-
      data %>%
      select(PersonId, Date, all_of(hrvar)) %>%
      group_by(Date, !!sym(hrvar)) %>%
      summarise(n = n_distinct(PersonId)) %>%
      arrange(desc(n))

    # Single reference for single and multiple org attributes
    hrvar_label <- hrvar
  }

  if(return == "table"){

    summary_table %>%
      mutate(PersonId = "") %>%
      create_trend(metric = "n",
                   hrvar = hrvar,
                   mingroup = 0,
                   return = "table")

  } else if(return == "plot"){

    ## This is re-run to enable multi-attribute grouping without concatenation
    summary_table %>%
      mutate(PersonId="") %>%
      create_trend(metric = "n",
                   hrvar = hrvar,
                   mingroup = 0,
                   return = "plot",
                   legend_title = "Number of employees") +
      labs(title = "Employees over time",
           subtitle = paste0("Dynamics by ", tolower(camel_clean(hrvar))))


  } else {

    stop("Please enter a valid input for `return`.")

  }

}

