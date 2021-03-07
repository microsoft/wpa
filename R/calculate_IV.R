# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title
#' Calculate Weight of Evidence (WOE) and Information Value (IV) between a
#' single predictor and a single outcome variable.
#'
#' @description
#' Calculates Weight of Evidence (WOE) and Information Value (IV) between a
#' single predictor and a single outcome variable. This function implements the
#' common Information Value calculations whilst maintaining the minimum reliance
#' on external dependencies. Use `map_IV()` for the equivalent of
#' `Information::create_infotables()`, which performs calculations for multiple
#' predictors and a single outcome variable.
#'
#' @details
#' The approach used mirrors the one used in `Information::create_infotables()`.
#'
#' @param data Data frame containing the data.
#' @param outcome String containing the name of the outcome variable.
#' @param predictor String containing the name of the predictor variable.
#' @param bins Numeric value representing the number of bins to use.
#'
#' @import dplyr
#'
#' @return A data frame is returned as an output.
#'
calculate_IV <- function(data,
                         outcome,
                         predictor,
                         bins){

  pred_var <- data[[predictor]]
  outc_var <- data[[outcome]]

  # Compute q
  q <- quantile(pred_var,
                probs = c(1:(bins - 1) / bins),
                na.rm = TRUE,
                type = 3)

  # Compute cuts
  cuts <- unique(q)

  # Compute intervals
  intervals <-
    findInterval(
      pred_var,
      vec = cuts,
      rightmost.closed = FALSE)

  # Compute cut_table
  cut_table <-
    table(
      intervals,
      outc_var) %>%
    as.data.frame.matrix()

  ## get min/max
  cut_table_2 <-
    data.frame(
    var = pred_var,
    intervals
  ) %>%
    group_by(intervals) %>%
    summarise(
      min = min(var, na.rm = TRUE) %>% round(digits = 1),
      max = max(var, na.rm = TRUE) %>% round(digits = 1),
      n = n(),
      .groups = "drop"
    ) %>%
    mutate(!!sym(predictor) :=
    glue::glue("[{round(min, digits = 1)},{round(max, digits = 1)}]")) %>%
    mutate(percentage = n / sum(n)) %>%
    select(!!sym(predictor), intervals, n, percentage)

  # Non-events in group
  n_non_event <- cut_table$`1`*sum(cut_table$`0`) # t$y_1*sum_y_0
  n_yes_event <- cut_table$`0`*sum(cut_table$`1`) # t$y_0*sum_y_1

  # Compute WOE

  cut_table_2$WOE <-
    ifelse(
      cut_table$`1` > 0 & cut_table$`0` > 0, # Both positive
      log(n_non_event / n_yes_event), # % of non-events divided by % of events
           0) # Otherwise impute 0

  # Compute IV_weight
  p1 <- cut_table$`1` / sum(cut_table$`1`)
  p0 <- cut_table$`0` / sum(cut_table$`0`)

  cut_table_2$IV_weight <- p1 - p0
  cut_table_2$IV <- cut_table_2$WOE * cut_table_2$IV_weight

  cut_table_2 %>%
    mutate(IV = cumsum(IV)) %>%
    # Maintain consistency with `Information::create_infotables()`
    select(
      !!sym(predictor),
      N = "n",
      Percent = "percentage",
      WOE,
      IV)
}

#' @title
#' Calculate Weight of Evidence (WOE) and Information Value (IV) between
#' multiple predictors and a single outcome variable, returning a list of
#' statistics.
#'
#' @description
#' This is a wrapper around `calculate_IV()` to loop through multiple predictors
#' and calculate their Weight of Evidence (WOE) and Information Value (IV) with
#' respect to an outcome variable.
#'
#' @details
#' The approach used mirrors the one used in `Information::create_infotables()`.
#'
#' @param data Data frame containing the data.
#' @param outcome String containing the name of the outcome variable.
#' @param predictors Character vector containing the names of the predictor
#'   variables. If `NULL` (default) is supplied, all numeric variables in the
#'   data will be used.
#' @param bins Numeric value representing the number of bins to use.
#'
#' @import dplyr
#'
#' @return A list of data frames is returned as an output. The first layer of
#' the list contains `Tables` and `Summary`:
#'   -  `Tables` is a list of data frames containing the WOE and cumulative sum
#'   IV for each predictor.
#'   - `Summary` is a single data frame containing the IV for all predictors.
#'
map_IV <- function(data,
                   predictors = NULL,
                   outcome,
                   bins){

  if(is.null(predictors)){

    predictors <-
      data %>%
      select(-!!sym(outcome)) %>%
      select(
        where(is.numeric)
      ) %>%
      names()
  }

  # List of individual tables
  Tables <-
    predictors %>%
    purrr::map(function(pred){

      calculate_IV(
        data = data,
        outcome = outcome,
        predictor = pred,
        bins = bins
      )
    }) %>%
    purrr::set_names(
      nm = purrr::map(
        .,
        function(df){
          names(df)[[1]]
        }
      )
    )

  # Compile Summary Table
  Summary <-
    list("df" = Tables,
         "names" = names(Tables)) %>%
    purrr::pmap(function(df, names){

      IV_final <-
        df %>%
        slice(nrow(df)) %>%
        pull(IV)

      data.frame(
        Variable = names,
        IV = IV_final
      )
    }) %>%
    bind_rows() %>%
    arrange(desc(IV))

  # Reorder and combine list
  c(
    list("Tables" = Tables[Summary$Variable]), # Reordered
    list("Summary" = Summary)
  )
}
