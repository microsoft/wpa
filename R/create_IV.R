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
#' @param return String specifying what to return. This must be one of the
#'   following strings:
#'   - `"plot"`
#'   - `"summary"`
#'   - `"list"`
#'   - `"plot-WOE"`
#'   - `"IV"`
#'
#' See `Value` for more information.
#'
#' @return
#' A different output is returned depending on the value passed to the `return`
#' argument:
#'   - `"plot"`: ggplot object. A bar plot showing the IV value of the top
#'   (maximum 12) variables.
#'   - `"summary"`: data frame. A summary table for the metric.
#'   - `"list"`: list. A list of outputs for all the input variables.
#'   - `"plot-WOE"`: A list of ggplot objects that show the WOE for each
#'   predictor used in the model.
#'   - `"IV"` returns the original Information object returned by
#'   `Information::create_infotables()`.
#'
#' @import dplyr
#'
#' @family Variable Association
#' @family Information Value
#'
#' @examples
#' # Return a summary table of IV
#' sq_data %>%
#'   dplyr::mutate(X = ifelse(Workweek_span > 40, 1, 0)) %>%
#'   create_IV(outcome = "X",
#'             predictors = c("Email_hours",
#'                            "Meeting_hours",
#'                            "Instant_Message_hours"),
#'             return = "plot")
#'
#'
#' # Return summary
#' sq_data %>%
#'   dplyr::mutate(X = ifelse(Collaboration_hours > 2, 1, 0)) %>%
#'   create_IV(outcome = "X",
#'             predictors = c("Email_hours", "Meeting_hours"),
#'             return = "summary")
#'
#' @export
create_IV <- function(data,
                      predictors = NULL,
                      outcome,
                      bins = 5,
                      siglevel = 0.05,
                      return = "plot"){

  # Preserve string
  pred_chr <- NULL
  pred_chr <- predictors


  if(is.null(tidyselect::all_of(predictors))){

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
  predictors <- data.frame(Variable = unlist(names(train)))
  predictors <-
    predictors %>%
    dplyr::filter(Variable != "outcome") %>%
    mutate(Variable = as.character(Variable)) # Ensure not factor

  for (i in 1:(nrow(predictors))){

     predictors$pval[i] <-
       p_test(train,
              outcome = "outcome",
              behavior = predictors$Variable[i])

  }


  # Filter out variables whose p-value is above the significance level
  predictors <- predictors %>% dplyr::filter(pval <= siglevel)
  train <- train %>% select(predictors$Variable, outcome)

  # IV Analysis
  # IV <- Information::create_infotables(data = train, y = "outcome", bins = bins)

  IV <- map_IV(data = train,
               predictors = pred_chr,
               outcome = "outcome", # string not variable
               bins = bins)


  IV_names <- names(IV$Tables)

  IV_summary <- inner_join(IV$Summary, predictors, by = c("Variable"))

  if(return == "summary"){

    IV_summary

  } else if(return == "IV"){

    c(
      IV,
      list("lnodds" = lnodds)
      )

  } else if(return == "plot"){

    top_n <-
      min(
        c(12, nrow(IV_summary))
      )

    IV_summary %>%
      utils::head(top_n) %>%
      create_bar_asis(group_var = "Variable",
                      bar_var = "IV",
                      title = "Information Value (IV)",
                      subtitle =
                        paste("Showing top",
                              top_n,
                              "predictors"))

  } else if(return == "plot-WOE"){

    ## Return list of ggplots

    IV$Summary$Variable %>%
      as.character() %>%
      purrr::map(~plot_WOE(IV = IV, predictor = .))

    } else if(return == "list"){

      # Output list
      output_list <-
        IV_names %>%
        purrr::map(function(x){
          IV$Tables[[x]] %>%
            mutate(ODDS = exp(WOE + lnodds),
                   PROB = ODDS / (ODDS + 1))
        }) %>%
        purrr::set_names(IV_names)

      output_list

    } else {

      stop("Please enter a valid input for `return`.")

    }
}

#' @title Plot WOE graphs with an IV object
#'
#' @description
#' Internal function within `create_IV()` that plots WOE graphs using an IV
#' object. Can also be used for plotting individual WOE graphs.
#'
#' @param IV IV object created with 'Information'.
#' @param predictor String with the name of the predictor variable.
#'
#' @return
#' ggplot object. Bar plot with 'WOE' as the y-axis and bins of the predictor
#' variable as the horizontal axis.
#'
#' @family Support
#' @family Variable Association
#' @family Information Value
#'
#' @import dplyr
#' @import ggplot2
#'
#' @export
plot_WOE <- function(IV, predictor){

  # Identify right table
  plot_table <-
    IV$Tables[[predictor]] %>%
    mutate(labelpos = ifelse(WOE <= 0, 1.2, -1))

  # Get range
  WOE_range <-
    IV$Tables %>%
    purrr::map(~pull(., WOE)) %>%
    unlist() %>%
    range()

  # Plot
  plot_table %>%
    mutate(!!sym(predictor) :=
             factor(!!sym(predictor),
                    levels =
                      pull(
                        plot_table,
                        predictor
                      )
             )) %>%
    ggplot(aes(x = !!sym(predictor),
               y = WOE)) +
    geom_col(fill = rgb2hex(49,97,124)) +
    geom_text(aes(label = round(WOE, 1),
                  vjust = labelpos)) +
    labs(title = us_to_space(predictor),
         subtitle = "Weight of Evidence",
         x = us_to_space(predictor),
         y = "Weight of Evidence (WOE)") +
    theme_wpa_basic() +
    ylim(WOE_range[1] * 1.1, WOE_range[2] * 1.1)

}
