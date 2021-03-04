# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Create interactive tables in HTML with 'download' buttons.
#'
#' @description
#' See https://martinctc.github.io/blog/vignette-downloadable-tables-in-rmarkdown-with-the-dt-package/ for more.
#'
#' @param x Data frame to be passed through.
#' @param rounding Numeric vector to specify the number of decimal points to display
#' @param freeze Number of columns from the left to 'freeze'. Defaults to 2,
#' which includes the row number column.
#'
#' @import DT
#' @importFrom dplyr mutate_if
#'
#' @family Import and Export
#'
#' @export
create_dt <- function(x, rounding = 1, freeze = 2){

  # Round all numeric to "rounding" number of dp
  num_cols <- dplyr::select_if(x, is.numeric) %>% names()

  if(length(num_cols) == 0){ # No numeric columns

    DT::datatable(x,
                  extensions = c('Buttons',
                                 'FixedColumns'),
                  options = list(dom = 'Blfrtip',
                                 fixedColumns = list(leftColumns = freeze),
                                 scrollX = TRUE,
                                 buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                 lengthMenu = list(c(10,25,50,-1),
                                                   c(10,25,50,"All"))))

  } else {

    DT::datatable(x,
                  extensions = c('Buttons',
                                 'FixedColumns'),
                  options = list(dom = 'Blfrtip',
                                 fixedColumns = list(leftColumns = freeze),
                                 scrollX = TRUE,
                                 buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                 lengthMenu = list(c(10,25,50,-1),
                                                   c(10,25,50,"All")))) %>%
      DT::formatRound(columns = num_cols, rounding)


  }
}
