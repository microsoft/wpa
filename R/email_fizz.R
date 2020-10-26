#' @title Distribution of Email Hours (Fizzy Drink plot)
#'
#' @description
#' Analyze weekly email hours distribution, and returns
#' a 'fizzy' scatter plot by default.
#' Additional options available to return a table with distribution elements.
#'
#' @inheritParams create_fizz
#'
#' @family Emails
#'
#' @examples
#' email_fizz(sq_data, hrvar = "Organization", return = "table")
#' @export

email_fizz <- function(data,
                       hrvar = "Organization",
                       mingroup = 5,
                       return = "plot"){

  create_fizz(data = data,
              metric = "Email_hours",
              hrvar = hrvar,
              mingroup = mingroup,
              return = return)

}
