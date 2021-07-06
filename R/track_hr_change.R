# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Sankey chart of organizational movement between HR attributes and
#'   missing values (outside company move) (Data Overview)
#'
#' @description
#' Creates a list of everyone at a specified start date and a specified end date
#' then aggregates up people who have moved between organizations between this
#' to points of time and visualizes the move through a sankey chart.
#'
#' Through this chart you can see:
#' -  The HR attribute/orgs that have the highest move out
#' -  The HR attribute/orgs that have the highest move in
#' -  The number of people that do not have that HR attribute or if they are no
#' longer in the system
#'
#' @author Tannaz Sattari Tabrizi <Tannaz.Sattari@@microsoft.com>
#'
#' @param data A Person Query dataset in the form of a data frame.
#' @param start_date A start date to compare changes. See `end_date`.
#' @param end_date An end date to compare changes. See `start_date`.
#' @param hrvar HR Variable by which to compare changes between, defaults to
#'   `"Organization"` but accepts any character vector, e.g.
#'   `"LevelDesignation"`
#' @param mingroup Numeric value setting the privacy threshold / minimum group
#'   size. Defaults to 5.
#' @param return Character vector specifying what to return, defaults to
#'   `"plot"`. Valid inputs are `"plot"` and `"table"`.
#' @param NA_replacement Character replacement for NA defaults to "out of
#'   company"
#'
#' @import dplyr
#'
#' @family Data Validation
#'
#' @return
#' Returns a 'NetworkD3' object by default, where 'plot' is passed in `return`.
#' When 'table' is passed, a summary table is returned as a data frame.
#'
#' @examples
#'
#' dv_data %>% track_HR_change()
#'
#' @export
track_HR_change <- function(data,
                            start_date = min(data$Date),
                            end_date = max(data$Date),
                            hrvar = "Organization",
                            mingroup = 5,
                            return = "plot",
                            NA_replacement = "Out of Company"){

  ## Check inputs
  required_variables <- c("Date",
                          "PersonId",
                          hrvar)

  ## Error message if variables are not present
  ## Nothing happens if all present
  data %>%
    check_inputs(requirements = required_variables)

  ## Define Start and End Dates
  data_start_date <- min(as.Date(data$Date,"%m/%d/%Y"))
  data_end_date <- max(as.Date(data$Date,"%m/%d/%Y"))

  ## Validate dates

  if (as.Date(end_date,"%m/%d/%Y") < data_start_date){
    stop("Please enter a valid start date.")
  }
  if (as.Date(start_date,"%m/%d/%Y") > data_end_date ){
    stop("Please enter a valid end date.")
  }

  ## filter data to starting point
  start_data <- data %>%
    filter(Date == start_date) %>%
    select("PersonId", hrvar) %>%
    rename(group := !!sym(hrvar))

  ## filter data to end point
  end_data <- data %>%
    filter(Date == end_date) %>%
    select("PersonId", hrvar) %>%
    rename(pre_group := !!sym(hrvar))

  ## Join the data to track the changes
  dt <- merge(start_data, end_data, by = "PersonId", all.x = TRUE)

  # aggregate the data over different orgs
  moves <-
    dt %>%
    group_by(pre_group, group) %>%
    summarise(Employee_Count = n(), .groups = "drop") %>%
    # clean the data to changes and replace NAs
    mutate(group = tidyr::replace_na(group, NA_replacement),
           pre_group = tidyr::replace_na(pre_group, NA_replacement)) %>%
    arrange(desc(Employee_Count))

  if(length(moves) == 0){
    stop("No change has happened.")
  }

  ## Save summary table to myTableReturn
  myTableReturn <- moves


  if(return == "table"){

    return(myTableReturn)

  } else if(return == "plot"){

    ## Plotting for sankey chart
    ## Set up `nodes`
    group_source <- unique(moves$pre_group)
    group_target <- paste0(unique(moves$group), " ")

    groups <- c(group_source, group_target)

    nodes_source <- tibble(name = group_source)
    nodes_target <- tibble(name = group_target)
    nodes <- rbind(nodes_source, nodes_target) %>% mutate(node = 0:(nrow(.) - 1))

    ## Set up `links`
    links <-
      moves %>%
      mutate(group = paste0(group, " ")) %>%
      select(source = "pre_group",
             target = "group",
             value = "Employee_Count")

    nodes_source <- nodes_source %>% select(name) # Make `nodes` a single column data frame
    nodes_target <- nodes_target %>% select(name) # Make `nodes` a single column data frame

    links <-
      links %>%
      left_join(nodes %>% rename(IDsource = "node"), by = c("source" = "name")) %>%
      left_join(nodes %>% rename(IDtarget = "node"), by = c("target" = "name"))


    networkD3::sankeyNetwork(Links = as.data.frame(links),
                             Nodes = as.data.frame(nodes),
                             Source = 'IDsource', # Change reference to IDsource
                             Target = 'IDtarget', # Change reference to IDtarget
                             Value = 'value',
                             NodeID = 'name',
                             units="count",
                             sinksRight = FALSE)

  } else {

    stop("Please enter a valid input for `return`.")

  }
}



