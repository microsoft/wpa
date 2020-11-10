# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Identify the query type of the passed data frame
#'
#' @description
#' Pass a Workplace Analytics dataset and return the identified
#' query type as a string. This function uses variable name string
#' matching to 'guess' the query type of the data frame.
#'
#' @param data A Workplace Analytics dataset in the form of a data frame.
#' If the data is not identified as a Workplace Analytics dataset, the function
#' will return an error.
#' @param threshold Debugging use only. Increase to raise the 'strictness' of the
#' guessing algorithm. Defaults to 2.
#'
#' @family Data Validation
#'
#' @examples
#' \dontrun{
#' identify_query(sq_data) # Standard query
#' identify_query(mt_data) # Meeting query
#' identify_query(em_data) # Hourly collaboration query
#' identify_query(iris) # Will return an error
#' identify_query(mtcars) # Will return an error
#' }
#'
#' @export
identify_query <- function(data, threshold = 2){

  ## variables to check for in each query type
  spq_var <- c("PersonId", "Collaboration_hours", "Instant_Message_hours") # Standard Person query
  caq_var <- c("PersonId", "Collaboration_hrs", "Instant_message_hours") # Collaboration Assessment query
  smq_var <- c("MeetingId", "Date", "Attendees") # Standard Meeting Query
  shc_var <- c("PersonId", "Emails_sent_00_01", "IMs_sent_23_24") # Standard Hourly Collaboration

  ## see if there are columns which do not exist
  spq_check <- check_inputs(input = data, requirements = spq_var, return = "names")
  caq_check <- check_inputs(input = data, requirements = caq_var, return = "names")
  smq_check <- check_inputs(input = data, requirements = smq_var, return = "names")
  shc_check <- check_inputs(input = data, requirements = shc_var, return = "names")

  ## length of checks
  spq_check_n <- length(spq_check)
  caq_check_n <- length(caq_check)
  smq_check_n <- length(smq_check)
  shc_check_n <- length(shc_check)
  total_check_vec <- c(spq_check_n, caq_check_n, smq_check_n, shc_check_n)

  ## should never be zero
  total_check_n <- sum(total_check_vec, na.rm = TRUE)

  ## Labels
  qlabels <- c("Person Query",
               "Collaboration Assessment Query",
               "Meeting Query",
               "Hourly Collaboration Query")

  ## Minimum number of non-matches
  min_nm <- min(total_check_vec)

  ## Final guess
  f_guess <- qlabels[which.min(total_check_vec)]

  if(total_check_n == 0){
    stop("Error: please check if query data is properly formatted query data.")
  } else if(min_nm > threshold){
    stop("Column mismatches: please check if query data is properly formatted query data.")
  } else {
    f_guess
  }
}

