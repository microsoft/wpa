# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Clean subject line text prior to analysis
#'
#' @description
#' This function processes the `Subject` column in a Meeting Query by applying
#' tokenisation using`tidytext::unnest_tokens()`, and removing any stopwords
#' supplied in a data frame (using the argument `stopwords`). This is a
#' sub-function that feeds into `tm_freq()`, `tm_cooc()`, and `tm_wordcloud()`.
#' The default is to return a data frame with tokenised counts of words or
#' ngrams.
#'
#'
#' @param data A Meeting Query dataset in the form of a data frame.
#' @param token A character vector accepting either `"words"` or `"ngram"`,
#'   determining type of tokenisation to return.
#' @param stopwords A single-column data frame labelled `'word'` containing
#'   custom stopwords to remove.
#'
#' @import dplyr
#' @import ggplot2
#' @importFrom tidytext unnest_tokens
#'
#' @family Text-mining
#'
#' @export
tm_clean <- function(data,
                     token = "words",
                     stopwords = NULL){

  # Get a dataset with only subjects and a subject line ID
  text_df <-
    data %>%
    select(Subject) %>%
    mutate(line = 1:n(),
           text = as.character(Subject)) %>%
    select(line, text)

  # Expand dataset to have each word in the subject as a different observation
  text_df <- text_df %>% tidytext::unnest_tokens(word, text, token = token)

  # Remove common English stop words (and, or, at, etc)
  text_df <-
    text_df %>%
    dplyr::anti_join(tidytext::stop_words)

  # Remove WPI custom irrelevant words
  if(!is.data.frame(stopwords)){

    stopwords <- dplyr::tibble(word = "")

  }

  text_df <- text_df %>% anti_join(stopwords)

  return(text_df)
}
