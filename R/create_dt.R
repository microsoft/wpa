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
#' @param rounding Numeric vector to specify the number of decimal points to display
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
      ) %>%
      DT::formatRound(columns = num_cols, rounding)

    if(percent == TRUE){

      out <-
        out %>%
        DT::formatPercentage(columns = num_cols, rounding)

    }
  }

  out
}
