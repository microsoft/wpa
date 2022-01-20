# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Generate a wordcloud with meeting subject lines
#'
#' @description
#' Generate a wordcloud with the meeting query.
#' This is a sub-function that feeds into `meeting_tm_report()`.
#'
#' @details
#' Uses the 'ggwordcloud' package for the underlying implementation, thus
#' returning a 'ggplot' object. Additional layers can be added onto the plot
#' using a ggplot `+` syntax.
#' The recommendation is not to return over 100 words in a word cloud.
#'
#' @details
#' This function uses `tm_clean()` as the underlying data wrangling function.
#' There is an option to remove stopwords by passing a data frame into the
#' `stopwords` argument.
#'
#' @param data A Meeting Query dataset in the form of a data frame.
#' @param stopwords A character vector OR a single-column data frame labelled
#'   `'word'` containing custom stopwords to remove.
#' @param seed A numeric vector to set seed for random generation.
#' @param keep A numeric vector specifying maximum number of words to keep.
#' @param return String specifying what to return. This must be one of the
#'   following strings:
#'   - `"plot"`
#'   - `"table"`
#'
#' See `Value` for more information.
#' @param ... Additional parameters to be passed to
#'   `ggwordcloud::geom_text_wordcloud()`
#'
#' @return
#' A different output is returned depending on the value passed to the `return`
#' argument:
#'   - `"plot"`: 'ggplot' object containing a word cloud.
#'   - `"table"`: data frame returning the data used to generate the word cloud.
#'
#' @import dplyr
#' @examples
#' tm_wordcloud(mt_data, keep = 30)
#'
#' # Removing stopwords
#' tm_wordcloud(mt_data, keep = 30, stopwords = c("weekly", "update"))
#'
#' @family Text-mining
#'
#' @export

tm_wordcloud <- function(data,
                         stopwords = NULL,
                         seed = 100,
                         keep = 100,
                         return = "plot",
                         ...){

  set.seed(seed)

  clean_data <-
    suppressMessages(tm_clean(
      data = data,
      token = "words",
      stopwords = stopwords
    ))

  plot_data <-
    clean_data %>% # Remove additional stop words
    count(word, name = "freq") %>%
    arrange(desc(freq))

  if(nrow(plot_data) < keep){
    keep <- nrow(plot_data)
  }

  plot_data <- plot_data %>% slice(1:keep)

  if(return == "plot"){
    output <-
      plot_data %>%
      ggplot(aes(label = word, size = freq)) +
      ggwordcloud::geom_text_wordcloud(rm_outside = TRUE, ...) +
      scale_size_area(max_size = 15) +
      theme_minimal()

    return(output)
  } else if (return == "table"){
    return(plot_data)
  } else {
    stop("Please enter a valid input for `return`.")
  }
}
