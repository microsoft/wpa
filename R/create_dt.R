# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Create interactive tables in HTML with 'download' buttons.
#'
#' @description
#' See
#' <https://martinctc.github.io/blog/vignette-downloadable-tables-in-rmarkdown-with-the-dt-package/>
#' for more.
#'
#' @param x Data frame to be passed through.
#' @param rounding Numeric vector to specify the number of decimal points to display.
#' Can also be a named list to specify different rounding for specific columns,
#' e.g., `list("Sepal.Width" = 1, "Sepal.Length" = 2)`. When a list is provided,
#' columns not specified in the list will use the default of 1 decimal place.
#' @param freeze Number of columns from the left to 'freeze'. Defaults to 2,
#' which includes the row number column.
#' @param percent Logical value specifying whether to display numeric columns
#' as percentages.
#' @param show_rows Numeric value or "All" to specify the default number of rows
#' to display. Defaults to 10. When set to a specific number, that number will be
#' the first option in the length menu. When set to "All", all rows will be shown
#' by default.
#'
#' @family Import and Export
#'
#' @examples
#' out_tb <- hrvar_count(sq_data, hrvar = "Organization", return = "table")
#' create_dt(out_tb)
#' 
#' # Show 25 rows by default
#' create_dt(out_tb, show_rows = 25)
#' 
#' # Show all rows by default  
#' create_dt(out_tb, show_rows = "All")
#' 
#' # Apply different rounding to specific columns
#' out_tb$prop <- out_tb$n / sum(out_tb$n)
#' create_dt(out_tb, rounding = list("n" = 0, "prop" = 3))
#' 
#' # Mix of list and default rounding
#' create_dt(out_tb, rounding = list("prop" = 3))  # Other numeric columns get 1 dp
#'
#' @return
#' Returns an HTML widget displaying rectangular data.
#'
#' @export
create_dt <- function(x, rounding = 1, freeze = 2, percent = FALSE, show_rows = 10){

  # Construct length menu based on show_rows parameter
  if(show_rows == "All" || show_rows == -1) {
    length_menu_values <- c(-1, 25, 10, 50)
    length_menu_labels <- c("All", 25, 10, 50)
  } else {
    # Ensure show_rows is numeric
    show_rows <- as.numeric(show_rows)
    # Create menu with show_rows first, then other common options
    other_options <- c(10, 25, 50, -1)
    other_options <- other_options[other_options != show_rows]
    length_menu_values <- c(show_rows, other_options)
    
    other_labels <- c(10, 25, 50, "All")
    other_labels <- other_labels[other_labels != show_rows]
    length_menu_labels <- c(show_rows, other_labels)
  }

  # Round all numeric to "rounding" number of dp
  num_cols <- dplyr::select(x, where(is.numeric)) %>% names()
  
  # Handle rounding parameter - can be numeric or named list
  if(is.list(rounding)) {
    # When rounding is a list, extract specified columns and their rounding values
    specified_cols <- intersect(names(rounding), num_cols)
    rounding_list <- rounding[specified_cols]
    # Default rounding for unspecified numeric columns
    default_rounding <- 1
    unspecified_cols <- setdiff(num_cols, specified_cols)
  } else {
    # When rounding is numeric (backward compatibility)
    rounding_list <- NULL
    default_rounding <- rounding
    unspecified_cols <- num_cols
    specified_cols <- character(0)
  }

  if(length(num_cols) == 0){ # No numeric columns

    out <-
      DT::datatable(
        x,
        extensions = c('Buttons',
                       'FixedColumns'),
        options = list(
          dom = 'Blfrtip',
          fixedColumns = list(leftColumns = freeze),
          scrollX = TRUE,
          buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
          lengthMenu = list(length_menu_values,
                            length_menu_labels)
        )
      )

  } else {

    out <-
      DT::datatable(
        x,
        extensions = c('Buttons',
                       'FixedColumns'),
        options = list(
          dom = 'Blfrtip',
          fixedColumns = list(leftColumns = freeze),
          scrollX = TRUE,
          buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
          lengthMenu = list(length_menu_values,
                            length_menu_labels)
        )
      )

    # Apply rounding - handle both single value and list cases
    if(length(unspecified_cols) > 0) {
      # Apply default rounding to unspecified columns
      out <- out %>% DT::formatRound(columns = unspecified_cols, default_rounding)
    }
    
    if(length(specified_cols) > 0) {
      # Apply specific rounding to specified columns
      for(col in specified_cols) {
        out <- out %>% DT::formatRound(columns = col, rounding_list[[col]])
      }
    }

    if(percent == TRUE){
      # Apply percentage formatting with appropriate rounding
      if(is.list(rounding)) {
        # Apply specific rounding to specified columns
        for(col in specified_cols) {
          out <- out %>% DT::formatPercentage(columns = col, rounding_list[[col]])
        }
        # Apply default rounding to unspecified columns
        if(length(unspecified_cols) > 0) {
          out <- out %>% DT::formatPercentage(columns = unspecified_cols, default_rounding)
        }
      } else {
        # Apply same rounding to all numeric columns (backward compatibility)
        out <- out %>% DT::formatPercentage(columns = num_cols, rounding)
      }
    }
  }

  out
}
