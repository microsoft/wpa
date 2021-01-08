# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Calculate Information Value for a selected outcome variable
#'
#' @description
#' Specify an outcome variable and return IV outputs.
#' All numeric variables in the dataset are used as predictor variables.
#'
#' @param data A Person Query dataset in the form of a data frame.
#' @param predictors A character vector specifying the columns to be used as predictors.
#' Defaults to NULL, where all numeric vectors in the data will be used as predictors.
#' @param outcome A string specifying a binary variable, i.e. can only contain
#' the values 1 or 0.
#' @param bins Number of bins to use in `Information::create_infotables()`, defaults to 5.
#' @param siglevel Significance level to use in comparing populations for the outcomes,
#' defaults to 0.05
#' @param return String specifying what output to return.
#' Defaults to "plot" that return a bar plot summarising the information value.
#' "summary" returns a summary table, "list" returns a list of outputs for all the
#' input variables, "plot-WOE" commpares distribution for top predictors.
#'
#' @import dplyr
#'
#' @family Information Value
#'
#' @examples
#' \dontrun{
# sq_data %>%
#   mutate(X = ifelse(Email_hours > 6, 1, 0)) %>%
#   create_IV(outcome = "X", return = "summary")
#' sq_data %>%
#'   mutate(X = ifelse(Collaboration_hours > 2, 1, 0)) %>%
#'   create_IV(outcome = "X",
#'             predictors = c("Email_hours", "Meeting_hours"),
#'             return = "list")
#' }
#'
#' @export
create_IV <- function(data,
                      predictors = NULL,
                      outcome,
                      bins = 5,
                      siglevel = 0.05,
                      return = "plot"){
  
  if(is.null(all_of(predictors))){
    train <-
      data %>%
      rename(outcome = outcome) %>%
      select_if(is.numeric) %>%
      tidyr::drop_na()
  } else {
    train <-
      data %>%
      rename(outcome = outcome) %>%
      select(tidyselect::all_of(predictors), outcome) %>%
      tidyr::drop_na()
  }
  
  # Calculate Odds
  odds <- sum(train$outcome) / (length(train$outcome) - sum(train$outcome))
  lnodds <- log(odds)
  
  
  # Calculate p-value  
  predictors <- data.frame(unlist(names(train)))
  names(predictors) <- c("Variable") 
  predictors <- predictors %>% filter(Variable != "outcome")
  
  for (i in 1:(nrow(predictors))){
    predictors$pval[i] <- p_test(outcome, predictors$Variable[i])
  }
  
  # Filter out variables whose p-value is above the significance level
  predictors <- predictors %>% filter(pval <= siglevel)
  train <- train %>% select(predictors$Variable, outcome)
  
  # IV Analysis
  IV <- Information::create_infotables(data = train, y = "outcome", bins = bins)
  IV_names <- names(IV$Tables)
  
  # Output list
  output_list <-
    IV_names %>%
    purrr::map(function(x){
      IV$Tables[[x]] %>%
        mutate(ODDS = exp(WOE + lnodds),
               PROB = ODDS / (ODDS + 1))
    }) %>%
    purrr::set_names(IV_names)
  
  
  IV_summary <- inner_join(IV$Summary, predictors, by = c("Variable"))
  
  
  
  if(return == "summary"){
    IV_summary
  } else if(return == "plot"){
    IV_summary %>%
      utils::head(12) %>%
      create_bar_asis(group_var = "Variable",
                      bar_var = "IV",
                      title = "Information Value (IV)",
                      subtitle = "Showing top 12 only")
    
  } else if(return == "plot-WOE"){
    Information::plot_infotables(IV, IV$Summary$Variable[], same_scale=TRUE) %>% grDevices::recordPlot()
    
  } else if(return == "list"){
    output_list
  } else {
    stop("Please enter a valid input for `return`.")
  }
}