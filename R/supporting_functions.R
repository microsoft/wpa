#' @title Check whether a data frame contains all the required variable
#'
#' @description
#' Checks whether a data frame contains all the required variables.
#' Matching works via variable names, and used to support individual
#' functions in the package. Not used directly.
#'
#' @param input Pass a data frame for checking
#' @param requirements A character vector specifying the required variable names
#' @param return A character string specifying what to return.
#' The default value is "stop". Also accepts "names" and "warning".
#'
#'
#' @return The default behaviour is to return an error message,
#' informing the user what variables are not included. When `return` is set
#' to "names", a character vector containing the unmatched variable names
#' is returned.
#'
#' @examples
#'
#' ## Return error message
#' \dontrun{
#' check_inputs(iris, c("Sepal.Length", "mpg"))
#' }
#'
#' #' ## Return warning message
#' check_inputs(iris, c("Sepal.Length", "mpg"), return = "warning")
#'
#' ## Return variable names
#' check_inputs(iris, c("Sepal.Length", "Sepal.Width", "RandomVariable"), return = "names")
#'
#' @export
check_inputs <- function(input, requirements, return = "stop"){

  ## Get variable names of input df
  nm_input <- names(input)

  ## Get logical vector
  test_results <- requirements %in% nm_input

  ## Return FALSE results
  not_in <- requirements[!test_results]

  ## Comma separated
  not_in_p <- paste(not_in, collapse = ", ")

  ## Create warning message
  warning_string <-
    paste("The following variables are not included in the input data frame:\n",
          not_in_p)

  if(length(not_in) > 0){

    if(return == "names"){

      return(not_in)

    } else if(return == "warning"){

      warning(warning_string)

    } else if(return == "stop"){

      stop(warning_string)

    } else {

      stop("Invalid input: please check `return` argument for `check_inputs()`")

    }
  }
}

#' @title Convert "CamelCase" to "Camel Case"
#'
#' @description
#' Convert a text string from the format "CamelCase" to "Camel Case".
#' This is used for converting variable names such as
#' "LevelDesignation" to "Level Designation" for the purpose
#' of prettifying plot labels.
#'
#' @param string A string vector in 'CamelCase' format to format
#'
#' @examples
#' camel_clean("NoteHowTheStringIsFormatted")
#'
#' @export
camel_clean <- function(string){

  gsub("([a-z])([A-Z])", "\\1 \\2", string)

}

#' @title Convert rgb to HEX code
#'
#' @param r,g,b Values that correspond to the three RGB parameters
#'
#' @export
rgb2hex <- function(r,g,b){
  grDevices::rgb(r, g, b, maxColorValue = 255)
}

#' @title Extract date period
#'
#' @description
#' Return a data frame with the start and end date
#' of the query data by default. There are options to return a descriptive
#' string, which is used in the caption of plots in this package.
#'
#' @param data Data frame containing a query to pass through.
#' The data frame must contain a `Date` column.
#' Accepts a Person query or a Meeting query.
#'
#' @param return String specifying what output to return.
#' Returns a table by default ("table"), but allows returning
#' a descriptive string ("text").
#'
#' @export
extract_date_range <- function(data, return = "table"){

  if("Date" %in% names(data)){

    date_var <- as.Date(data$Date, "%m/%d/%Y")

  } else if(all(c("StartDate", "EndDate") %in% names(data))){

    ## meeting query
    date_var <- c(data$StartDate, data$EndDate)
    date_var <- as.Date(date_var, "%m/%d/%Y")

  } else {

    stop("Error: no date variable found.")

  }

  myPeriod <-
    data.frame(Start = min(date_var),
               End = max(date_var))

  ## Alternative return
  if(return == "table"){

    myPeriod

  } else if(return == "text"){

    paste("Data from week of", myPeriod$Start, "to week of", myPeriod$End)

  }
}

#' @title Add comma separator for thousands
#'
#' @description
#' Takes a numeric value and returns a character value
#' which is rounded to the whole number, and adds a comma
#' separator at the thousands. A convenient wrapper function
#' around `round()` and `format()`.
#'
#' @param x A numeric value
#'
#' @export
comma <- function(x){
  x <- round(x, 0)
  format(x, nsmall = 0, big.mark=",")
}

