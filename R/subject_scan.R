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
#' @param return String specifying what to return. This must be one of the
#'   following strings:
#'   - `"plot"`
#'   - `"table"`
#'   - `"data"`
#'
#' See `Value` for more information.
#'
#' @return
#' A different output is returned depending on the value passed to the `return`
#' argument:
#'   - `"plot"`: 'ggplot' object. A heatmapped grid.
#'   - `"table"`: data frame. A summary table for the metric.
#'   - `"data"`: data frame.
#'
#' @import dplyr
#'
#' @examples
#' mt_data %>% subject_scan()
#'
#' @export
subject_scan <- function(data,
                         hrvar,
                         n = 10,
                         return = "plot"){

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

  # return simple table -----------------------------------------------

  out_simple <-
    out_tb_wide %>%
    select(-ends_with("_n")) %>%
    set_names(nm = gsub(pattern = "_word", replacement = "",
                        x = names(.)))

  # return chunk -------------------------------------------------------

  if(return == "plot"){

    out_tb_long %>%
      mutate(n = maxmin(n)) %>%
      arrange(desc(n)) %>%
      group_by(group) %>%
      mutate(id = 1:n()) %>%
      ungroup() %>%
      ggplot(aes(x = group, y = id)) +
      geom_tile(aes(fill = n)) +
      geom_text(aes(label = word)) +
      scale_fill_gradient2(low = rgb2hex(7, 111, 161),
                           mid = rgb2hex(241, 204, 158),
                           high = rgb2hex(216, 24, 42),
                           midpoint = 0.5,
                           breaks = c(0, 0.5, 1),
                           labels = c("Minimum", "", "Maximum"),
                           limits = c(0, 1)) +
      scale_x_discrete(position = "top") +
      scale_y_reverse() +
      theme_wpa_basic() +
      theme(axis.text.x = element_text(angle = 45, hjust = 0),
            plot.title = element_text(color="grey40", face="bold", size=20),
            axis.text.y = element_blank()) +
      labs(
        title = "Top words",
        subtitle = "Divided by group",
        y = "Top words by frequency in Subject",
        x = "Group"
      )

  } else if(return == "table"){

    out_simple

  }
}
