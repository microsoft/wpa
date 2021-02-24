# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Generate a Information Value HTML Report
#'
#' @description
#' The function generates an interactive HTML report using Standard Person Query
#' data as an input. The report contains a full Information Value analysis,  a
#' data exploration technique that helps determine which columns in a data set
#' have predictive power or influence on the value of a specified dependent
#' variable.
#'
#' @param data A Standard Person Query dataset in the form of a data frame.
#' @param predictors A character vector specifying the columns to be used as
#'   predictors. Defaults to NULL, where all numeric vectors in the data will be
#'   used as predictors.
#' @param outcome A string specifying a binary variable, i.e. can only contain
#'   the values 1 or 0.
#' @param bins Number of bins to use in `Information::create_infotables()`,
#'   defaults to 10.
#' @param path Pass the file path and the desired file name, _excluding the file
#'   extension_. For example, "collaboration report".
#' @param timestamp Logical vector specifying whether to include a timestamp in
#'   the file name. Defaults to TRUE.
#'
#' @examples
#' \dontrun{
#' sq_data %>%
#'   mutate(X = ifelse(Collaboration_hours > 12, 1, 0)) %>% # Simulate binary variable
#'   IV_report(outcome =  "CH_binary",
#'             predictors = c("Email_hours", "Workweek_span"))
#' }
#'
#' @family Reports
#' @family Information Value
#'
#' @inherit generate_report return
#'
#' @export
IV_report <- function(data,
                      predictors = NULL,
                      outcome,
                      bins = 5,
                      path = "IV report",
                      timestamp = TRUE){

  ## Create timestamped path (if applicable)
  if(timestamp == TRUE){
    newpath <- paste(path, wpa::tstamp())
  } else {
    newpath <- path
  }

 table_list <-
   data %>%
   create_IV(outcome = outcome,
             predictors = predictors,
             bins = bins,
             return = "list")

 ## List of ggplot
 plot_list <-
   data %>%
   create_IV(outcome = outcome,
             predictors = predictors,
             bins = bins,
             return = "plot-WOE")

 table_names <- gsub("_", " ", x = names(table_list))

  output_list <-
    list(
      data %>% check_query(return = "text"),
      data %>% create_IV(outcome = outcome, predictors=predictors, bins= bins),
      data %>% create_IV(outcome = outcome,
                         predictors = predictors,
                         bins = bins,
                         return="summary"),
      read_preamble("blank.md") # Header for WOE Analysis
         ) %>%
    c(plot_list) %>%
    c(list(read_preamble("blank.md"))) %>% # Header for Summary Tables
    c(table_list)  %>%
    purrr::map_if(is.data.frame, create_dt) %>%
    purrr::map_if(is.character, md2html)

  title_list <-
    c("Data Overview",
      "Top Predictors",
      "",
      "WOE Analysis",
      rep("", length(plot_list)),
      "Summary - Predictors",
      table_names)


  n_title <- length(title_list)

  title_levels <-
    c(
      2,
      2,
      4,
      2, # Header for WOE Analysis
      rep(4, length(plot_list)),
      2, # Header for WOE Analysis
      rep(3, length(table_list))
      )

  generate_report(title = "Information Value Report",
                  filename = newpath,
                  outputs = output_list,
                  titles = title_list,
                  subheaders = rep("", n_title),
                  echos = rep(FALSE, n_title),
                  levels = title_levels,
                  theme = "cosmo",
                  preamble = read_preamble("IV_report.md"))

}




