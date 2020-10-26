#' @title Frequency Analysis
#'
#' @description
#' Generate a circular bar plot with frequency of words / ngrams.
#' This is a sub-function that feeds into `meeting_tm_report()`.
#'
#' @details
#' This function uses `tm_clean()` as the underlying data wrangling function.
#' There is an option to remove stopwords by passing a data frame into the `stopwords`
#' argument.
#'
#' @param data A Meeting Query dataset in the form of a data frame.
#' @param token A character vector accepting either "words" or "ngram", determining
#' type of tokenisation to return.
#' @param stopwords A single-column data frame labelled 'word' containing custom stopwords to remove.
#' @param keep A numeric vector specifying maximum number of words to keep.
#' @param return Character vector specifying what to return, defaults to "plot".
#' Valid inputs are "plot" and "table".
#'
#' @import dplyr
#' @import ggplot2
#' @importFrom tidytext unnest_tokens
#' @importFrom stats na.omit
#'
#' @examples
#' tm_freq(mt_data)
#'
#' @examples
#' \dontrun{
#' tm_freq(mt_data, token = "words")
#' tm_freq(mt_data, token = "ngrams")
#'}
#'
#' @family Text-mining
#'
#' @export
tm_freq <- function(data,
                    token = "words",
                    stopwords = NULL,
                    keep = 100,
                    return = "plot"){
  # Clean data
  text_df <- suppressMessages(tm_clean(data = data,
                                       token = token,
                                       stopwords = stopwords))

  # Calculate frequency of word sand keep top 100 terms
  text_count <-
    text_df %>%
    count(word, sort = TRUE) %>%
    stats::na.omit() %>%
    top_n(keep)

  # Plot as a circular bar chart
  # Note that id is a factor. If x is numeric, there is some space between the first bar
  # `fill` in `geom_bar` add the bars with a blue color
  # Limits of the plot = very important.
  # The negative value controls the size of the inner circle,
  # the positive one is useful to add size over each bar
  # Custom the theme: no axis title and no cartesian grid
  # `coord_polar` makes the coordinate polar instead of cartesian.
  # `geom_text` adds the labels, using the label_data dataframe that we have created before

  p <-
    ggplot(text_count, aes(x=as.factor(word), y=n)) +
    geom_bar(stat="identity", fill=alpha("skyblue", 0.7)) +
    ylim(-100,max(text_count$n) + 10) +
    theme_minimal() +
    theme(
      axis.text = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      plot.margin = unit(rep(-1,4), "cm") # Adjust the margin to make in sort labels are not truncated!
    ) +
    coord_polar(start = 0) +
    geom_text(data=text_count,
              aes(x=word, y=n+10, label=word),
              color="black",
              fontface="bold",
              alpha=0.6,
              size=2.5,
              inherit.aes = FALSE)

  if(return == "table"){

    text_count %>%
      as_tibble() %>%
      return()

  } else if(return == "plot"){

    return(p)

  } else {

    stop("Please enter a valid input for `return`.")

  }

}
