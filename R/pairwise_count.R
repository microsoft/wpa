#' @title Perform a pairwise count of words by id
#'
#' @description This is a 'data.table' implementation that mimics the output of
#'   `pairwise_count()` from 'widyr' to reduce package dependency. This is used
#'   internally within `tm_cooc()`.
#'
#' @param data Data frame output from `tm_clean()`.
#' @param id String to represent the id variable. Defaults to `"line"`.
#' @param word String to represent the word variable. Defaults to `"word"`.
#'
#' @return
#' data frame with the following columns representing a pairwise count:
#'   - `"item1"`
#'   - `"item2"`
#'   - `"n"`
#'
#' @importFrom data.table ":=" "%like%" "%between%" rbindlist as.data.table
#'
#' @family Support
#' @family Text-mining
#'
#' @examples
#' td <- data.frame(line = c(1, 1, 2, 2),
#'                  word = c("work", "meeting", "catch", "up"))
#'
#' pairwise_count(td, id = "line", word = "word")
#'
#' @export
pairwise_count <- function(data,
                           id = "line",
                           word = "word"){

  # Make sure data.table knows we know we're using it
  .datatable.aware = TRUE

  data <-
    data %>%
    dplyr::rename(word := !!sym(word),
                  id := !!sym(id))


  DT <- data.table::as.data.table(data)

  # convert to character
  DT[, word := as.character(word)]

  # subset those with >1 per id
  DT2 <- DT[, N := .N, by = id][N>1]

  # create all combinations of 2
  # return as a data.table with these as columns `V1` and `V2`
  # then count the numbers in each id
  out_data <-
    DT2[, rbindlist(utils::combn(word,2,
                        FUN = function(x) as.data.table(as.list(x)),
                        simplify = FALSE)), by = id] %>%
    .[, .N, by = list(V1,V2)]

  # format and sort
  out_data %>%
    dplyr::as_tibble() %>%
    dplyr::rename(item1 = "V1",
                  item2 = "V2",
                  n = "N") %>%
    dplyr::arrange(desc(n))
}
