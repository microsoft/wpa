# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title
#' Analyse word co-occurrence in subject lines and return a network plot
#'
#' @description
#' This function generates a word co-occurrence network plot, with options to
#' return a table. This function is used within `meeting_tm_report()`.
#'
#' @author Carlos Morales <carlos.morales@@microsoft.com>
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
#' @param return String specifying what to return. This must be one of the
#'   following strings:
#'   - `"plot"`
#'   - `"table"`
#'
#' See `Value` for more information.
#'
#' @param lmult A multiplier to adjust the line width in the output plot.
#'   Defaults to 0.05.
#'
#' @return
#' A different output is returned depending on the value passed to the `return`
#' argument:
#'   - `"plot"`: 'ggplot' and 'ggraph' object. A network plot.
#'   - `"table"`: data frame. A summary table.
#'
#' @import dplyr
#' @import ggplot2
#' @import ggraph
#' @importFrom igraph graph_from_data_frame
#' @importFrom tidytext unnest_tokens
#'
#' @family Text-mining
#'
#' @examples
#' # Demo using a subset of `mt_data`
#' mt_data %>%
#'   dplyr::slice(1:20) %>%
#'   tm_cooc(lmult = 0.01)
#'
#' @export
tm_cooc <- function(data,
                    stopwords = NULL,
                    seed = 100,
                    return = "plot",
                    lmult = 0.05){

  # Clean data
  text_df <- suppressMessages(tm_clean(data = data,
                                       token = "words",
                                       stopwords = stopwords))



  # Calculate frequency of pairs
  title_word_pairs <-
    text_df %>%
    pairwise_count(id = "line", word = "word")

  # Graph networks
  set.seed(seed)

  p <-
    title_word_pairs %>%
    dplyr::top_n(500) %>%
    igraph::graph_from_data_frame() %>%
    ggraph::ggraph(layout = "fr") +
    ggraph::geom_edge_link(aes(edge_alpha = 0.5, edge_width = n * lmult), edge_colour = "cyan4") +
    ggraph::geom_node_point(size = 2) +
    ggraph::geom_node_text(aes(label = name),
                           repel = TRUE,
                           point.padding = unit(0.2, "lines")) +
    ggplot2::theme(panel.background = ggplot2::element_rect(fill = 'white'), legend.position = "none")

  if(return == "table"){

    title_word_pairs %>%
      dplyr::top_n(500) %>%
      as_tibble() %>%
      return()

  } else if(return == "plot"){

    return(p)

  } else {

    stop("Please enter a valid input for `return`.")

  }

}
