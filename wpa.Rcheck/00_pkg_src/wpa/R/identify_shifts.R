# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title
#' Identify shifts based on outlook time settings for work day start and end
#' time
#'
#' @description
#' This function uses outlook calendar settings for start and end time of work
#' day to identify work shifts. The relevant variables are
#' `WorkingStartTimeSetInOutlook` and `WorkingEndTimeSetInOutlook`.
#'
#'
#' @param data A data frame containing data from the Hourly Collaboration query.
#'
#' @param return String specifying what to return. This must be one of the
#'   following strings:
#'   - `"plot"`
#'   - `"table"`
#'   - `"data"`
#'
#' See `Value` for more information.
#'
#' @return
#' A different output is returned depending on the value passed to the `return`
#' argument:
#'   - `"plot"`: ggplot object. A bar plot for the weekly count of shifts.
#'   - `"table"`: data frame. A summary table for the count of shifts.
#'   - `"data`: data frame. Input data appended with the `Shifts` columns.
#'
#' @importFrom data.table ":=" "%like%" "%between%"
#'
#' @family Data Validation
#' @family Working Patterns
#'
#' @examples
#' # Return plot
#' dv_data %>% identify_shifts()
#'
#' # Return summary table
#' dv_data %>% identify_shifts(return = "table")
#'
#' @export
identify_shifts <- function(data, return = "plot"){

  clean_times <- function(x){
    out <- gsub(pattern = ":00", replacement = "", x = x)
    as.numeric(out)
  }

  data <- data.table::as.data.table(data)
  # data <- data.table::copy(data)

  # Make sure data.table knows we know we're using it
  .datatable.aware = TRUE

  data[, Shifts := paste(WorkingStartTimeSetInOutlook, WorkingEndTimeSetInOutlook, sep = "-")]


  # outputTable <- data[, .(count = .N), by = Shifts]
  outputTable <- data[, list(WeekCount = .N,
                             PersonCount = dplyr::n_distinct(PersonId)), by = Shifts]

  outputTable <- data.table::setorder(outputTable, -PersonCount)


  if(return == "table"){
    dplyr::as_tibble(outputTable)

  } else if(return == "plot"){

    outputTable %>%
      utils::head(10) %>%
      create_bar_asis(group_var = "Shifts",
                      bar_var = "WeekCount",
                      title = "Most frequent outlook shifts",
                      subtitle = "Showing top 10 only",
                      caption = extract_date_range(data, return = "text"),
                      ylab = "Shifts",
                      xlab = "Frequency")

  } else if(return == "data"){

    output_data <- data
    dplyr::as_tibble(output_data)
  }
}
