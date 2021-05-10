# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title
#' Takes in a numeric value of 'hhmm' format and convert this into a 'hms' format
#'
#' @param x String in 'hhmm' format.
#'
#' @export
split_hm <- function(x){

  # Preserve signs ---------------------------------------------------------

  sign_x <- ifelse(
    x < 0, # Less than zero
    -1,
    1
  )

  # Remove signs -----------------------------------------------------------

  x <- as.character(x)
  x <- gsub(pattern = "-", replacement = "", x = x)
  nchar_x <- nchar(x)
  x_max <- max(c(nchar_x, 1))



  out <-
    dplyr::case_when(
      # nchar_x >= 5 ~ NA,
      x == "0" ~ hms::hms(hour = 0, minutes = 0), # Special case
      nchar_x == 4 ~  hms::hms(
        hour = as.numeric(substr(x, start = 1, stop = 2)),
        minutes = as.numeric(substr(x, start = 3, stop = 4))
      ),
      nchar_x == 3 ~  hms::hms(
        hour = as.numeric(substr(x, start = 1, stop = 1)),
        minutes = as.numeric(substr(x, start = 2, stop = 3))
      ),
      nchar_x %in% 1:2 ~  hms::hms(
        hour = rep(0, length(x)),
        minutes = as.numeric(substr(x, start = nchar_x - 1, stop = x_max))
      )
    )

  # Apply signs -----------------------------------------------------------
  out * sign_x

  out <- ifelse(
    sign_x == -1, # If negative
    hms::as_hms(hms::hms(hours = 24) - out),
    out)

  hms::as_hms(out)
}
