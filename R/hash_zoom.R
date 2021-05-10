# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Reads in unhashed Zoom meeting participants file and merges data with
#'   a hash file
#'
#' @description
#' Reads in an unhashed Zoom meeting participants file from .csv and replaces
#' usernames with a `HashID` that is supplied in a separate look up file. This
#' is to be used _after_ `bind_csv()` which will combine all the separate Zoom
#' participants file into a single file.
#'
#' @param zoom_path string containing the path to the unhashed Zoom participants
#'   .csv file.
#' @param hash_file string containing the path to the .csv file containing the
#' hash IDs. The file should contain only two columns with the following
#' headers:
#'   - `PersonID`: column containing email addresses that map to the Zoom
#'   `User_Name` columns.
#'   - `HashID`: column containing HashIDs which will be uploaded as
#'   organizational attributes to Workplace Analytics.
#' @param match_only logical. Determines whether to include only rows where
#' there is a corresponding match on the hash file. Defaults to `FALSE`.
#'
#'
#' @export

hash_zoom <- function(zoom_path,
                      hash_path,
                      match_only = FALSE){


  zoom_dt <- data.table::fread(zoom_path, encoding = "UTF-8")
  hash_dt <- data.table::fread(hash_path, encoding = "UTF-8")

  # Drop unnecessary columns ------------------------------------------------

  zoom_dt[, User_Name := NULL]
  zoom_dt[, X13 := NULL]
  zoom_dt[, Display_name := NULL]
  zoom_dt[, Phone_numberName_Original_Name := NULL]

  # Standardize cases prior to replacement ----------------------------------

  hash_dt[, PersonID := str_trim(tolower(PersonID))]
  zoom_dt[, User_Email_1 := str_trim(tolower(User_Email_1))]
  zoom_dt[, User_Email_2 := str_trim(tolower(User_Email_2))]


  # Replace `User_Email_1` --------------------------------------------------
  setkey(zoom_dt, "User_Email_1")
  setkey(hash_dt, "PersonID")

  zoom_dt <- hash_dt[zoom_dt]

  zoom_dt[, User_Email_1 := HashID]
  zoom_dt[, HashID := NULL] # Drop `HashID`

  # Replace `User_Email_2` --------------------------------------------------
  setkey(zoom_dt, "User_Email_2")
  zoom_dt <- hash_dt[zoom_dt]

  zoom_dt[, User_Email_2 := HashID]
  zoom_dt[, HashID := NULL] # Drop `HashID`

  # Drop unnecessary columns ------------------------------------------------

  zoom_dt[, PersonID := NULL]
  zoom_dt[, i.PersonID := NULL]

  # Remove non-matches ------------------------------------------------------

  if(match_only == TRUE){

    zoom_dt %>%
      .[!is.na(User_Email_1)] %>%
      .[!is.na(User_Email_2)]

  } else {

    zoom_dt[]

  }
}

