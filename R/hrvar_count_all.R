# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Create count of distinct fields and percentage of employees with
#'   missing values for all HR variables
#'
#' @description `r lifecycle::badge('experimental')`
#'
#' This function enables you to create a summary table to validate
#' organizational data. This table will provide a summary of the data found in
#' the Viva Insights _Data sources_ page. This function will return a summary
#' table with the count of distinct fields per HR attribute and the percentage
#' of employees with missing values for that attribute. See `hrvar_count()`
#' function for more detail on the specific HR attribute of interest.
#'
#' @param data A Standard Person Query dataset in the form of a data frame.
#' @param n_var number of HR variables to include in report as rows. Default is
#'   set to 50 HR variables.
#' @param return String to specify what to return
#' @param threshold The max number of unique values allowed for any attribute.
#'   Default is 100.
#' @param maxna The max percentage of NAs allowable for any column. Default is
#'   20.
#'
#' @import dplyr
#'
#' @family Data Validation
#'
#' @examples
#' # Return a summary table of all HR attributes
#' hrvar_count_all(sq_data, return = "table")
#'
#' @return
#' Returns an error message by default, where 'text' is passed in `return`.
#'   - `'table'`: data frame. A summary table listing the number of distinct
#' fields and percentage of missing values for the specified number of HR
#' attributes will be returned.
#'   - `'message'`: outputs a message indicating which values are
#' beyond the specified thresholds.
#'
#' @export
hrvar_count_all <- function(data,
                            n_var = 50,
                            return = "message",
                            threshold = 100,
                            maxna = 20
                            ){

  ## Character vector of HR attributes
  extracted_chr <- extract_hr(
    data,
    return = "names",
    max_unique = threshold,
    exclude_constants = FALSE
    )

  summary_table_n <-
    data %>%
    select(PersonId, extracted_chr) %>%
    summarise_at(vars(extracted_chr), ~n_distinct(.,na.rm = TRUE)) # Excludes NAs from unique count

  ## Note: WPA here is used for a matching separator
  results <-
    data %>%
    select(PersonId, extracted_chr) %>%
    summarise_at(vars(extracted_chr),
                 list(`WPAn_unique` = ~n_distinct(., na.rm = TRUE), # Excludes NAs from unique count
                      `WPAper_na` = ~(sum(is.na(.))/ nrow(data) * 100),
                      `WPAsum_na` = ~sum(is.na(.)) # Number of missing values
                      )) %>% # % of missing values
    tidyr::gather(attribute, values) %>%
    tidyr::separate(col = attribute, into = c("attribute", "calculation"), sep = "_WPA") %>%
    tidyr::spread(calculation, values)

    ## Single print message
    if(sum(results$n_unique >= threshold)==0){

      printMessage <- paste("No attributes have greater than", threshold, "unique values.")
    }

    if(sum(results$per_na >= maxna)==0){
      newMessage <- paste("No attributes have more than", maxna, "percent NA values.")
      printMessage <- paste(printMessage, newMessage, collapse = "\n")

    }

    for (i in 1:nrow(results)) {
      if(results$n_unique[i] >= threshold){

        newMessage <- paste0("The attribute '",results$attribute[i],"' has a large amount of unique values. Please check.")
        printMessage <- paste(printMessage, newMessage, collapse = "\n")
        }

      if(results$per_na[i]>=maxna){

        newMessage <- paste0("The attribute '",results$attribute[i],"' has a large amount of NA values. Please check.")
        printMessage <- paste(printMessage, newMessage, collapse = "\n")
        }
    }

    if(return == "table"){

      results <-
        results %>%
        select(Attributes = "attribute",
               `Unique values` = "n_unique",
               `Total missing values` = "sum_na",
               `% missing values` = "per_na")

      return(utils::head(results, n_var))

    } else if(return == "text"){

      printMessage

    } else if(return == "message"){

      message(printMessage)

    } else {

      stop("Error: please check inputs for `return`")

    }
}

