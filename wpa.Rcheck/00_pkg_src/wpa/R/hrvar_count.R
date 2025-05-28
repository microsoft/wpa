# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Create a count of distinct people in a specified HR variable
#'
#' @description
#' This function enables you to create a count of the distinct people
#' by the specified HR attribute.The default behaviour is to return a
#' bar chart as typically seen in 'Analysis Scope'.
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
#' hrvar_count(sq_data, hrvar = "LevelDesignation")
#'
#' # Return a summary table
#' hrvar_count(sq_data, hrvar = "LevelDesignation", return = "table")
#'
#'@export

hrvar_count <- function(data,
                        hrvar = "Organization",
                        return = "plot"){

  ## Allow multiple HRvar inputs
  if(length(hrvar) > 1){
    hrvar_flat <- paste(hrvar, collapse = ", ")

    summary_table <-
      data %>%
      select(PersonId, all_of(hrvar)) %>%
      mutate(!!sym(hrvar_flat) := select(., hrvar) %>%
               apply(1, paste, collapse = ", ")) %>%
      group_by(!!sym(hrvar_flat)) %>%
      summarise(n = n_distinct(PersonId)) %>%
      arrange(desc(n))

    # Single reference for single and multiple org attributes
    hrvar_label <- hrvar_flat

  } else {

    summary_table <-
      data %>%
      select(PersonId, all_of(hrvar)) %>%
      group_by(!!sym(hrvar)) %>%
      summarise(n = n_distinct(PersonId)) %>%
      arrange(desc(n))

    # Single reference for single and multiple org attributes
    hrvar_label <- hrvar
  }

  if(return == "table"){

    data %>%
      data.table::as.data.table() %>%
      .[, .(n = n_distinct(PersonId)), by = hrvar] %>%
      as_tibble() %>%
      arrange(desc(n))

  } else if(return == "plot"){

    ## This is re-run to enable multi-attribute grouping without concatenation
    summary_table %>%
      ggplot(aes(x = stats::reorder(!!sym(hrvar_label), -n),
                 y = n)) +
      geom_col(fill = rgb2hex(0, 120, 212)) +
      geom_text(aes(label = n),
                vjust = -1,
                fontface = "bold",
                size = 4)+
      theme_wpa_basic() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(title = paste("People by", camel_clean(hrvar_label))) +
      scale_y_continuous(limits = c(0, max(summary_table$n) * 1.1)) +
      xlab(camel_clean(hrvar_label)) +
      ylab("Number of employees")

  } else {

    stop("Please enter a valid input for `return`.")

  }

}

#' @rdname hrvar_count
#' @export
analysis_scope <- hrvar_count

