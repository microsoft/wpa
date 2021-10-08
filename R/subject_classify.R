# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Create a new logical variable that classifies meetings by patterns in
#' subject lines
#'
#' @description Take a meeting query with subject lines and create a new
#' TRUE/FALSE column which classifies meetings by a provided set of patterns in
#' the subject lines.
#'
#' @param data A Meeting Query dataset in the form of a data frame.
#' @param var_name String containing the name of the new column to be created.
#' @param keywords Character vector containing the keywords to match.
#' @param pattern String to use for regular expression matching instead of
#'   `keywords`. When both `keywords` and `pattern` are supplied, `pattern`
#'   takes priority and is used instead.
#' @param ignore_case Logical value to determine whether to ignore case when
#'   performing pattern matching.
#' @param return String specifying what output to return.
#'
#' @examples
#' class_df <-
#'   mt_data %>%
#'   subject_classify(
#'     var_name = "IsSales",
#'     keywords = c("sales", "marketing")
#'   )
#'
#' class_df %>% dplyr::count(IsSales)
#'
#' # Return a table directly
#' mt_data %>% subject_classify(pattern = "annual", return = "table")
#'
#' @export
subject_classify <- function(data,
                             var_name = "class",
                             keywords = NULL,
                             pattern = NULL,
                             ignore_case = FALSE,
                             return = "data"
                             ){

  if(is.null(pattern) & is.null(keywords)){

    stop("Please supply a character vector to either `keywords` or `pattern`.")

  } else if(is.null(pattern)){

    message("Using character vector supplied to `keywords`.")

    pattern <- paste(keywords, collapse = "|")

  }

  # Create logical variable and match pattern ------------------------------

  data[[var_name]] <-
    grepl(
      pattern = pattern,
      x = data[["Subject"]],
      ignore.case = ignore_case
    )

  if(return == "data"){

    message("Returning data frame with additional column: ",
            var_name)

    data

  } else if(return == "table"){

    dplyr::count(data, !!sym(var_name))

  } else {

    stop("Invalid value to `return`.")

  }
}
