# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title
#' Count top words in subject lines grouped by a custom attribute
#'
#' @description `r lifecycle::badge('experimental')`
#'
#' This function generates a matrix of the top occurring words in meetings,
#' grouped by a specified attribute such as organisational attribute, day of the
#' week, or hours of the day.
#'
#' @param data A Meeting Query dataset in the form of a data frame.
#' @param hrvar String containing the name of the HR Variable by which to split
#'   metrics.
#' @param n Numeric value specifying the top number of words to show.
#'
#' @import dplyr
#'
#' @examples
#' mt_data %>% subject_scan()
#'
#' @export
subject_scan <- function(data, hrvar, n = 10){

  # long table -------------------------------------------------------

  out_tb_long <-
    data %>%
    group_split(!!sym(hrvar)) %>%
    purrr::map(function(x){

      dow <- x[[hrvar]][1]

      long_t <- tm_clean(x)

      long_t %>%
        count(word) %>%
        arrange(desc(n)) %>%
        head(n) %>%
        mutate(group = dow)
    }) %>%
    bind_rows()

  # wide table -------------------------------------------------------

  out_tb_wide <-
    out_tb_long %>%
    group_split(group) %>%
    purrr::map(function(x){

      dow <- x[["group"]][1]

      x %>%
        rename(
          !!sym(paste0(dow, "_word")) := "word",
          !!sym(paste0(dow, "_n")) := "n"
        ) %>%
        select(-group)
    }) %>%
    bind_cols()

  out_simple <-
    out_tb %>%
    select(-ends_with("_n")) %>%
    set_names(nm = gsub(pattern = "_word", replacement = "",
                        x = names(.)))

  return(out_simple)
}
