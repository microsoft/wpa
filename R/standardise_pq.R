# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Standardise variable names to a Standard Person Query
#'
#' @description
#' This function standardises the variable names to a Standard Person
#' Query, where the standard use case is to pass a Ways of Working Assessment
#' Query to the function.
#'
#' @details
#' The following standardisation steps are taken:
#' - `Collaboration_hrs` -> `Collaboration_hours`
#' - `Instant_message_hours` -> `Instant_Message_hours`
#'
#' @param data A Ways of Working Assessment query to pass through as a data
#'   frame.
#'
#' @family Data Validation
#' @family Import and Export
#'
#' @export
standardise_pq <- function(data){

  if(identify_query(data) != "Ways of Working Assessment Query"){
    stop("Currently only Ways of Working Assessment Query to Standard Person Query
         conversions are supported.")
  }

  ## Message what the function is doing
  message("Standardising variable names with a Person Query...")

  ## Collaboration_hours
  if(!("Collaboration_hours" %in% names(data)) &
     ("Collaboration_hrs" %in% names(data))){

    data <- data %>% rename(Collaboration_hours = "Collaboration_hrs")
    message("`Collaboration_hrs` renamed to `Collaboration_hours`")

  } else if(!("Collaboration_hrs" %in% names(data)) &
            ("Collaboration_hours" %in% names(data))){

            # Do nothing

  } else {

    warning("No collaboration hour metric exists in the data.")

  }

## Instant_Message_hours
  if(!("Instant_message_hours" %in% names(data)) &
   ("Instant_Message_hours" %in% names(data))){

  # Do nothing

  } else if(!("Instant_Message_hours" %in% names(data)) &
          ("Instant_message_hours" %in% names(data))){

    data <- data %>% rename(Instant_Message_hours = "Instant_message_hours")
    message("`Instant_message_hours` renamed to `Instant_Message_hours`")

  } else {

    warning("No instant message hour metric exists in the data.")

  }

  return(data)
}

#' @rdname standardise_pq
#' @export
standardize_pq <- standardise_pq

