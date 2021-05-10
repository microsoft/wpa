# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title
#' Read in csv files with the same column structure and row-bind
#'
#' @description
#' csv files with a matched pattern in the file name in the given file
#' path are read in, and a row-bind operation is run to combine them. Pattern
#' matching is optional, and the user can choose whether to return a combined
#' data frame or save the output with the same name in the directory.
#'
#' @param path String containing the file path to the csv files.
#' @param pattern String containing pattern to match in the file name. Optional
#' and defaults to `NULL` (all csv files in path will be read in).
#' @param save_csv Logical, defaults to `FALSE`. If set to `TRUE`, a csv file
#' containing the combined data frame is saved in the same folder as the
#' provided path.
#'
#' @return
#' When `save_csv` is set to `FALSE`, a data frame is returned. When set to
#' `TRUE`, a csv file containing the combined data frame is saved in the same
#' folder as the provided path.
#'
#' @section How to Run:
#' ```
#' # Read files in and save output to path
#' bind_csv(
#'   path = "data",
#'   pattern = "2021-04-05",
#'   save_csv = TRUE
#' )
#'
#' # Read files in and return data frame
#' bind_csv(
#'   path = "data"
#' )
#' ```
#'
#' @export
bind_csv <- function(path,
                     pattern = NULL,
                     save_csv = FALSE){

  # List all files in path --------------------------------------------------

  file_str <- list.files(path = path)

  # List csv file in path ---------------------------------------------------
  csv_str <- file_str[grepl(pattern = "\\.csv$",
                            x = file_str,
                            ignore.case = TRUE)]

  # Match all files with pattern --------------------------------------------
  if(is.null(pattern)){

    csv_match_str <- csv_str # No pattern matching

  } else {

    csv_match_str <- csv_str[grepl(pattern = pattern,
                                   x = csv_str,
                                   ignore.case = FALSE)]

  }

  # Append file path --------------------------------------------------------

  csv_match_str <- paste0(path, "/", csv_match_str)

  # Read all csv files in ---------------------------------------------------

  readin_list <-
    purrr::map(csv_match_str,
               function(i){

                 # data.table::fread(input = i, fill = TRUE)
                 readr::read_csv(file = i)

               })

  # Column name cleaning ----------------------------------------------------

  readin_csv <- dplyr::bind_rows(readin_list) %>%
    clean_zoom()

  # Save CSV

  if(save_csv == FALSE){

    readin_csv

  } else {

    #TODO: min and max date - adds significant time!
    #TODO: move to archive directory
    save_path <- paste0(path, "/combinedzoomparticipant_", wpa::tstamp(), ".csv")

    readr::write_excel_csv(
      readin_csv,
      save_path
    )

    message(paste("Combined file saved to", save_path))

  }
}
