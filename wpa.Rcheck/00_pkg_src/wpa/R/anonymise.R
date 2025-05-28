# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Anonymise a categorical variable by replacing values
#'
#' @description
#' Anonymize categorical variables such as HR variables by replacing values with
#' dummy team names such as 'Team A'. The behaviour is to make 1 to 1
#' replacements by default, but there is an option to completely randomise
#' values in the categorical variable.
#'
#' @param x Character vector to be passed through.
#' @param scramble Logical value determining whether to randomise values in the
#' categorical variable.
#' @param replacement Character vector containing the values to replace original
#'   values in the categorical variable. The length of the vector must be at
#'   least as great as the number of unique values in the original variable.
#'   Defaults to `NULL`, where the replacement would consist of `"Team A"`,
#'   `"Team B"`, etc.
#'
#' @examples
#' unique(anonymise(sq_data$Organization))
#'
#' rep <- c("Manager+", "Manager", "IC")
#' unique(anonymise(sq_data$Layer), replacement = rep)
#'
#' @seealso jitter
#' @export

anonymise <- function(x,
                      scramble = FALSE,
                      replacement = NULL){

  n_to_rep <- length(x)
  v_to_rep <- unique(x)
  nd_to_rep <- length(v_to_rep)

  if(is.null(replacement)){

    replacement <- paste("Team", LETTERS[1:nd_to_rep])

  } else {

    replacement <- replacement[1:nd_to_rep]

  }

  if(scramble == TRUE){

    sample(x = replacement,
           size = n_to_rep,
           replace = TRUE)

  } else if(scramble == FALSE){

    replacement[match(x, v_to_rep)]

  }
}

#' @rdname anonymise
#' @export
anonymize <- anonymise

#' @title Jitter metrics in a data frame
#'
#' @description Convenience wrapper around `jitter()` to add a layer of
#'   anonymity to a query. This can be used in combination with `anonymise()` to
#'   produce a demo dataset from real data.
#'
#' @param data Data frame containing a query.
#' @param cols Character vector containing the metrics to jitter. When set to
#' `NULL` (default), all numeric columns in the data frame are jittered.
#' @param ... Additional arguments to pass to `jitter()`.
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr across
#'
#' @examples
#' jittered <- jitter_metrics(sq_data, cols = "Collaboration_hours")
#' head(
#'   data.frame(
#'     original = sq_data$Collaboration_hours,
#'     jittered = jittered$Collaboration_hours
#'   )
#' )
#'
#' @seealso anonymise
#' @export

jitter_metrics <- function(data, cols = NULL, ...){

  if(!is.null(cols)){

    data %>%
      mutate(
        across(
          .cols = cols,
          .fns = ~abs(jitter(., ...))
        )
      )

  } else {

    data %>%
      mutate(
        across(
          .cols = where(~is.numeric(.)),
          .fns = ~abs(jitter(., ...))
        )
      )

  }

}
