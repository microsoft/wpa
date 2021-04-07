# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Tenure calculation based on different input dates, returns data
#'   summary table or histogram
#'
#' @description
#' This function calculates employee tenure based on different input dates.
#' `identify_tenure` uses the latest Date available if user selects "Date",
#'  but also have flexibility to select a specific date, e.g. "1/1/2020".
#'
#' @family Data Validation
#'
#' @param data A Standard Person Query dataset in the form of a data frame.
#' @param end_date A string specifying the name of the date variable
#'   representing the latest date. Defaults to "Date".
#' @param beg_date A string specifying the name of the date variable
#'   representing the hire date. Defaults to "HireDate".
#' @param maxten A numeric value representing the maximum tenure.
#' If the tenure exceeds this threshold, it would be accounted for in the flag message.
#'
#' @param return String specifying what to return. This must be one of the
#'   following strings:
#'   - `"message"`
#'   - `"text"`
#'   - `"plot"`
#'   - `"data_cleaned"`
#'   - `"data_dirty"`
#'   - `"data"`
#'
#' See `Value` for more information.

#' @return
#' A different output is returned depending on the value passed to the `return`
#' argument:
#'   - `"message"`: message on console with a diagnostic message.
#'   - `"text"`: string containing a diagnostic message.
#'   - `"plot"`: 'ggplot' object. A line plot showing tenure.
#'   - `"data_cleaned"`: data frame filtered only by rows with tenure values
#'   lying within the threshold.
#'   - `"data_dirty"`: data frame filtered only by rows with tenure values
#'   lying outside the threshold.
#'   - `"data"`: data frame with the `PersonId` and a calculated variable called
#'   `TenureYear` is returned.
#'
#'
#' @examples
#' library(dplyr)
#' # Add HireDate to sq_data
#' sq_data2 <-
#'   sq_data %>%
#'   mutate(HireDate = as.Date("1/1/2015", format = "%m/%d/%Y"))
#'
#' identify_tenure(sq_data2)
#'
#' @export
identify_tenure <- function(data,
                            end_date = "Date",
                            beg_date = "HireDate",
                            maxten = 40,
                            return = "message"){

  required_variables <- c("HireDate")
  ## Error message if variables are not present
  ## Nothing happens if all present
  data %>%
    check_inputs(requirements = required_variables)

  data_prep <-
    data %>%
    mutate(Date = as.Date(Date, format= "%m/%d/%Y"), # Re-format `Date`
           end_date = as.Date(!!sym(end_date), format= "%m/%d/%Y"), # Access a symbol, not a string
           beg_date = as.Date(!!sym(beg_date), format= "%m/%d/%Y")) %>% # Access a symbol, not a string
    arrange(end_date) %>%
    mutate(End = last(end_date))

  last_date <- data_prep$End

  # graphing data
  tenure_summary <-
    data_prep %>%
    filter(Date == last_date) %>%
    mutate(tenure_years = (Date - beg_date)/365) %>%
    group_by(tenure_years)%>%
    summarise(n = n(),.groups = 'drop')


  # off person IDs
  oddpeople <-
    data_prep %>%
    filter(Date == last_date) %>%
    mutate(tenure_years = (Date - beg_date)/365) %>%
    filter(tenure_years >= maxten) %>%
    select(PersonId)

  # message
  Message <- paste0("The mean tenure is ",round(mean(tenure_summary$tenure_years,na.rm = TRUE),1)," years.\nThe max tenure is ",
                   round(max(tenure_summary$tenure_years,na.rm = TRUE),1),".\nThere are ",
                   length(tenure_summary$tenure_years[tenure_summary$tenure_years>=maxten])," employees with a tenure greater than ",maxten," years.")

  if(return == "text"){
    return(Message)

  } else if(return == "message"){
    message(Message)

  } else if(return == "plot"){

    suppressWarnings(
      ggplot(data = tenure_summary,aes(x = as.numeric(tenure_years))) +
        geom_density() +
        labs(title = "Tenure - Density",
             subtitle = "Calculated with `HireDate`") +
        xlab("Tenure in Years") +
        ylab("Density - number of employees") +
        theme_wpa_basic()
    )

  } else if(return == "data_cleaned"){

    return(data %>% filter(!(PersonId %in% oddpeople$PersonId)) %>% data.frame())

  } else if(return == "data_dirty"){

    return(data %>% filter((PersonId %in% oddpeople$PersonId)) %>% data.frame())

  } else if(return == "data"){

    data_prep %>%
      filter(Date == last_date) %>%
      mutate(TenureYear = as.numeric((Date - beg_date)/365)) %>%
      select(PersonId, TenureYear)

  } else {

    stop("Error: please check inputs for `return`")

  }

}
