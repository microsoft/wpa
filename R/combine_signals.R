# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Combine signals from Emails and IMs
#'
#' @description
#' Takes in an Hourly Collaboration Data, and for each hour sums and stores the
#' signal for `Emails_sent` and `IMs_sent` in `Signals_sent`. This is an internal
#' function used in the Working Patterns functions.
#'
#' @param data Data containing Emails_sent and IMs_sent variables
#' @param hr Numeric value between 0 to 23 to iterate through
#'
#' @export
combine_signals <- function(data, hr){

  if(!is.numeric(hr) | hr < 0 | hr > 23){

    stop("Check inputs for `hr` in `combine_signals()`")

  }

  hr_two <- hr + 1

  hr1 <- ifelse(nchar(hr) == 1, paste0(0, hr), hr)
  hr2 <- ifelse(nchar(hr_two) == 1, paste0(0, hr_two), hr_two)

  full_string <- paste0("Signals_sent_", hr1, "_", hr2)
  em_string <- paste0("Emails_sent_", hr1, "_", hr2)
  im_string <- paste0("IMs_sent_", hr1, "_", hr2)


  dplyr::transmute(data, !!sym(full_string) := !!sym(em_string) + !!sym(im_string))
}
