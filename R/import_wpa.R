# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Import a Workplace Analytics Query
#'
#' @description
#' Import a Workplace Analytics Query from a local CSV File, with variable classifications optimised
#' for other 'wpa' functions.
#'
#' @details
#' `import_wpa()` uses `data.table::fread()` to import CSV files for speed,
#' and by default `stringsAsFactors` is set to FALSE.
#' A data frame is returned by the function (not a `data.table`).
#'
#' @param x String containing the path to the Workplace Analytics query to be imported.
#' The input file must be a CSV file, and the file extension must be explicitly entered,
#' e.g. "/files/standard query.csv"
#' @param standardise logical. If TRUE, `import_wpa()` runs `standardise_pq()` to make a Collaboration
#' Assessment query's columns name standard and consistent with a Standard Person Query. Note that this
#' will have no effect if the query being imported is not a Ways of Working Assessment query. Defaults
#' as FALSE.
#'
#' @return A `tibble` is returned.
#'
#' @export
import_wpa <- function(x, standardise = FALSE){

  return_data <- data.table::fread(x, stringsAsFactors = FALSE) %>% as.data.frame()

  # Columns which are Dates
  dateCols <- sapply(return_data, function(x) all(is_date_format(x)))
  dateCols <- dateCols[dateCols == TRUE]

  return_data <-
    return_data %>%
    dplyr::mutate_at(dplyr::vars(names(dateCols)), ~as.Date(., format = "%m/%d/%Y"))

  message("Query has been imported successfully!")

  ## Query check only available for Person Queries
  if(identify_query(return_data) == "Person Query"){
    check_query(return_data)
  }

  ## Standardise query if `standardise == TRUE`
  if(standardise == TRUE & identify_query(return_data) == "Ways of Working Assessment Query"){
    message("Standardising column names for a Ways of Working Assessment query to
            a Person query...")
    return_data <- standardise_pq(return_data)
  }

  dplyr::as_tibble(return_data)
}




