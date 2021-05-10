# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Import a Zoom Meeting Participants csv file
#'
#' @description
#' A fast method to import a Zoom Meeting Participants csv file whilst
#' preserving UTF-8 encoding. A convenience wrapper around
#' `data.table::fread()`.
#'
#' @export
import_zoom <- function(path, date_format = "%m/%d/%Y %H:%M:%S"){
    suppressWarnings(
      data.table::fread(path, encoding = "UTF-8"),
    )
}

#' @title Perform variable name cleaning on a Zoom Meeting Participants file
#'
#' @description
#' This is used within `bind_csv()` to perform variable name cleaning on a Zoom
#' Meeting Participants file.
#'
#' @noRd
clean_zoom <- function(x, date_format = "%m/%d/%Y %H:%M:%S"){

  x %>%
    # Remove blanks -------------------------------------------------------
  dplyr::filter(
    !is.na(Topic),
    !is.na(`Meeting ID`),
    !is.na(`User Name`)
  ) %>%
    set_names(gsub(pattern = " ", replacement = "_", x = names(.))) %>%
    set_names(gsub(pattern = "[?]|\\(|\\)", replacement = "", x = names(.))) %>%
    set_names(gsub(pattern = "[...]", replacement = "", x = names(.))) %>%
    dplyr::rename(
      User_Email_2 = "User_Email_1",
      User_Email_1 = "User_Email",
      Duration_Minutes_2 = "Duration_Minutes_1",
      Duration_Minutes_1 = "Duration_Minutes"
    )  %>%

    # Enforce variable type ------------------------------------------------
  mutate(
    across(
      .cols =
        c(Join_Time,
          Leave_Time,
          Creation_Time,
          Start_Time,
          End_Time),
      .fns = ~as.POSIXct(x = ., format = date_format, tz = "UTC")
    )
  )
}
