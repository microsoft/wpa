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
#' @param mode String specifying what variable to use for grouping subject
#'   words. Valid values include:
#'   - `"hours"`
#'   - `"days"`
#'   - `NULL` (defaults to `hrvar`)
#' When the value passed to `mode` is not `NULL`, the value passed to `hrvar`
#' will be discarded and instead be over-written by setting specified in `mode`.
#' @param top_n Numeric value specifying the top number of words to show.
#' @inheritParams tm_clean
#' @param return String specifying what to return. This must be one of the
#'   following strings:
#'   - `"plot"`
#'   - `"table"`
#'   - `"data"`
#'
#' See `Value` for more information.
#' @param weight String specifying the column name of a numeric variable for
#'   weighting data, such as `"Invitees"`. The column must contain positive
#'   integers. Defaults to `NULL`, where no weighting is applied.
#' @param ... Additional parameters to pass to `tm_clean()`.
#'
#' @return
#' A different output is returned depending on the value passed to the `return`
#' argument:
#'   - `"plot"`: 'ggplot' object. A heatmapped grid.
#'   - `"table"`: data frame. A summary table for the metric.
#'   - `"data"`: data frame.
#'
#' @import dplyr
#' @import ggplot2
#'
#' @examples
#' mt_data %>% subject_scan(hrvar = "Organizer_Organization")
#'
#' @export
subject_scan <- function(data,
                         hrvar,
                         mode = NULL,
                         top_n = 10,
                         token = "words",
                         return = "plot",
                         weight = NULL,
                         ...){

  # weighting -------------------------------------------------------

  if(!is.null(weight)){

    d_weight <- data[[weight]]

    if(any(is.na(d_weight) | d_weight <= 0 | d_weight %% 1 != 0)){

      stop("Please check 'weight' variable.")

    }

    # duplicate rows according to numeric weight
    # numeric weight must be an integer
    data_w <- data[rep(seq_len(nrow(data)), d_weight),]

  } else {

    data_w <- data

  }

  # modes -----------------------------------------------------------

  if(is.null(mode)){

    # do nothing

  } else if(mode == "hours"){

    data_w <-
      data_w %>%
      mutate(HourOfDay = substr(StartTimeUTC, start = 1, stop = 2) %>%
               as.numeric()) %>%
      mutate(HourOfDay =
               case_when(HourOfDay > 19 ~ "After 7PM",
                         HourOfDay >= 17 ~ "5 - 7 PM",
                         HourOfDay >= 14 ~ "2 - 5 PM",
                         HourOfDay >= 11 ~ "11AM - 2 PM",
                         HourOfDay >= 9 ~ "9 - 11 AM",
                         TRUE ~ "Before 9 AM"
               ) %>%
               factor(
                 levels = c(
                   "Before 9 AM",
                   "9 - 11 AM",
                   "11AM - 2 PM",
                   "2 - 5 PM",
                   "5 - 7 PM",
                   "After 7PM"
                 )
               ))

    hrvar <- "HourOfDay"

  } else if(mode == "days"){

    data_w <-
      data_w %>%
      mutate(DayOfWeek = weekdays(StartDate))

    hrvar <- "DayOfWeek"

  }

  # long table -------------------------------------------------------

  out_tb_long <-
    data_w %>%
    group_split(!!sym(hrvar)) %>%
    purrr::map(function(x){

      dow <- x[[hrvar]][1]

      long_t <- tm_clean(x, token = token, ...) %>%
        filter(!is.na(word))

      long_t %>%
        count(word) %>%
        arrange(desc(n)) %>%
        head(top_n) %>%
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
    purrr::set_names(nm = gsub(pattern = "_word", replacement = "",
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
      geom_text(aes(label = word), size = 3) +
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
