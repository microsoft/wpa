#' @title Plot a Sample of Working Patterns using Flexibility Index output
#'
#' @description This is a helper function for plotting visualizations for the
#'   Flexibility Index using the `data` output from `flex_index()`. This is used
#'   within `flex_index()` itself as an internal function.
#'
#' @param data Data frame. Direct data output from `flex_index()`.
#' @param sig_label Character string for identifying signal labels.
#' @param method Character string for determining which plot to return.
#' Options include "sample", "common", and "time". "sample"
#' plots a sample of ten working patterns; "common" plots the ten most common
#' working patterns; "time" plots the Flexibility Index for the group over time.
#' @param start_hour See `flex_index()`.
#' @param end_hour See `flex_index()`.
#' @param mode See `flex_index()`.
#' @import dplyr
#' @import ggplot2
#' @importFrom data.table ":=" "%like%" "%between%"
#'
#' @family Working Patterns
#'
#' @examples
#' # Pre-calculate Flexibility Index
#' fi_output <- flex_index(em_data, return = "data")
#'
#'
#' # Examples of how to test the plotting options individually
#' # Sample of 10 work patterns
#' plot_flex_index(fi_output, method = "sample")
#'
#' # 10 most common work patterns
#' plot_flex_index(fi_output, method = "common")
#'
#' # Plot Flexibility Index over time
#' plot_flex_index(fi_output, method = "time")
#'
#' @return ggplot object. See `method`.
#'
#' @export
plot_flex_index <- function(data,
                            sig_label = "Signals_sent_",
                            method = "sample",
                            start_hour = 9,
                            end_hour = 17,
                            mode = "binary"){

  ## Bindings for variables
  TakeBreaks <- NULL
  ChangeHours <- NULL
  ControlHours <- NULL
  FlexibilityIndex <- NULL

  ## Avoid confusion
  sig_label_ <- sig_label

  ## Table for annotation - plotting only
  ## Different calculation for results
  myTable_legends <-
    data %>%
    dplyr::summarise_at(vars(TakeBreaks, ChangeHours, ControlHours), ~mean(., na.rm = TRUE), .groups = "drop_last") %>%
    dplyr::mutate(FlexibilityIndex = select(., TakeBreaks, ChangeHours, ControlHours) %>% apply(1, mean),
           patternRank = 5) # 5 so that it shows right in the middle


  ## Used for captions
  score_tb <-
    myTable_legends %>%
    dplyr::mutate_at(vars(FlexibilityIndex), ~round(.*100)) %>%
    dplyr::mutate_at(vars(TakeBreaks, ChangeHours, ControlHours), ~scales::percent(.))

  ## Make for pretty printing
  myTable_legends <-
    myTable_legends %>%
    dplyr::mutate(FlexibilityIndex = scales::percent(FlexibilityIndex))


  ## Main plot
  ## Different plots if different `method` is specified

  if(method == "sample"){

    # Sample of ten working patterns

    plot_data <-
      data %>%
      .[sample(nrow(.), size = 10), ]

    plot_title <- "Random sample of 10 Working patterns"

  } else if(method == "common"){

    # Top ten most common patterns

    ## Make sure data.table knows we know we're using it
    .datatable.aware = TRUE

    data_tb <- data.table::as.data.table(data)

    plot_title <- "Top 10 Most Common Working patterns"

  } else if(method == "time"){

    plot_data <- data

  } else {

    stop("Invalid input value for `method`")

  }


  ## Branch out - sample/common VS time

  if(method %in% c("sample", "common")){


    if(mode == "binary"){

      input_var <- names(data)[grepl(sig_label_, names(data))]

      data_tb <- data_tb[, list(WeekCount = .N,
                                PersonCount = dplyr::n_distinct(PersonId)),
                         by = input_var]

      plot_data <-
        data_tb %>%
        as.data.frame() %>%
        dplyr::arrange(desc(WeekCount)) %>%
        slice(1:10)

      plot_data_long <-
        plot_data %>%
        mutate(patternRank = 1:nrow(.)) %>%
        dplyr::select(patternRank, dplyr::starts_with(sig_label_))  %>%
        purrr::set_names(nm = gsub(pattern = sig_label_, replacement = "", x = names(.))) %>%
        purrr::set_names(nm = gsub(pattern = "_.+", replacement = "", x = names(.)))

    } else if(mode == "prop"){

      input_var <- names(data)[grepl(sig_label_, names(data))]

      sig_label_ <- gsub(
        pattern = "_sent_",
        replacement = "_ori_",
        x = sig_label_
      )

      ## 00, 01, 02, etc.
      hours_col <- stringr::str_pad(seq(0,23), width = 2, pad = 0)

      # Use `mutate()` method
      # Will get 10 IDs, not 10 rows
      # NOTE: `input_var` is used to identify a distinct work pattern
      plot_data <-
        data_tb %>%
        data.table::as.data.table() %>%
        .[, `:=`(WeekCount = .N,
                 PersonCount = dplyr::n_distinct(PersonId),
                 Id = .GRP), # group id assignment
          by = input_var] %>%
        dplyr::arrange(desc(WeekCount))

      plot_data <-
        plot_data %>%
        dplyr::select(Id, dplyr::contains("_ori_"), WeekCount)  %>%
        purrr::set_names(nm = gsub(
          pattern = ".+_ori_",
          replacement = "",
          x = names(.)
        )) %>%
        purrr::set_names(nm = gsub(
          pattern = "_.+",
          replacement = "",
          x = names(.)
        )) %>%
        # Need aggregation
        .[, Signals_Total := rowSums(.SD), .SDcols = hours_col] %>%
        .[, c(hours_col) := .SD / Signals_Total, .SDcols = hours_col] %>%
        .[, Signals_Total := NULL] %>% # Remove unneeded column
        .[, lapply(.SD, mean, na.rm = TRUE), .SDcols = hours_col, by = list(Id, WeekCount)]

      plot_data_long <-
        plot_data %>%
        dplyr::arrange(desc(WeekCount)) %>%
        dplyr::mutate(patternRank = 1:nrow(.)) %>%
        slice(1:10)

    } else {

      stop("Invalid value to `mode`")
    }


    plot_data_long %>%
      plot_hourly_pat(
        start_hour = start_hour,
        end_hour = end_hour,
        legend = myTable_legends,
        legend_label = "FlexibilityIndex",
        legend_text = paste("Observed activity"),
        rows = 10, # static
        title = "Work Patterns and Flexibility Index",
        subtitle = paste0(plot_title,
                          "\n",
                          "Group Flexibility Index: ",
                          score_tb$FlexibilityIndex),
        caption = paste0("% Taking Breaks: ", score_tb$TakeBreaks, "\n",
                         "% Change Hours: ", score_tb$ChangeHours, "\n",
                         "% Keep Hours Under Control: ", score_tb$ControlHours, "\n",
                         extract_date_range(data, return = "text")),
        ylab = "Work patterns"
      )




    # plot_data %>%
    #   mutate(patternRank = 1:nrow(.)) %>%
    #   dplyr::select(patternRank, dplyr::starts_with(sig_label_))  %>%
    #   purrr::set_names(nm = gsub(pattern = sig_label_, replacement = "", x = names(.))) %>%
    #   purrr::set_names(nm = gsub(pattern = "_.+", replacement = "", x = names(.))) %>%
    #   tidyr::gather(Hours, Freq, -patternRank) %>%
    #   ggplot2::ggplot(ggplot2::aes(x = Hours, y = patternRank, fill = Freq)) +
    #   ggplot2::geom_tile(height=.5) +
    #   ggplot2::ylab("Work Patterns") +
    #   ggplot2::scale_fill_gradient2(low = "white", high = "#1d627e") +
    #   ggplot2::scale_y_reverse(breaks=seq(1,10)) +
    #   wpa::theme_wpa_basic() +
    #   ggplot2::theme(legend.position = "none") +
    #   ggplot2::annotate("text",
    #                     y = myTable_legends$patternRank,
    #                     x = 26.5,
    #                     label = scales::percent(myTable_legends$FlexibilityIndex), size = 3) +
    #   ggplot2::annotate("rect",
    #                     xmin = 25,
    #                     xmax = 28,
    #                     ymin = 0.5,
    #                     ymax = 10 + 0.5,
    #                     alpha = .2) +
    #   ggplot2::annotate("rect",
    #                     xmin = 0.5,
    #                     xmax = start_hour + 0.5,
    #                     ymin = 0.5,
    #                     ymax = 10 + 0.5,
    #                     alpha = .1,
    #                     fill = "gray50") +
    #   ggplot2::annotate("rect",
    #                     xmin = end_hour + 0.5,
    #                     xmax = 24.5,
    #                     ymin = 0.5,
    #                     ymax = 10 + 0.5,
    #                     alpha = .1,
    #                     fill = "gray50") +
    #   ggplot2::labs(title = "Work Patterns and Flexibility Index",
    #                 subtitle = paste0(plot_title,
    #                                   "\n",
    #                                   "Group Flexibility Index: ", score_tb$FlexibilityIndex),
    #                 caption = paste0("% Taking Breaks: ", score_tb$TakeBreaks, "\n",
    #                                  "% Change Hours: ", score_tb$ChangeHours, "\n",
    #                                  "% Keep Hours Under Control: ", score_tb$ControlHours, "\n",
    #                                  extract_date_range(data, return = "text")))

  } else if(method == "time"){

    plot_data %>%
      group_by(Date) %>%
      summarise_at(vars(FlexibilityIndex), ~mean(., na.rm = TRUE)) %>%
      wpa::create_line_asis(date_var = "Date",
                            metric = "FlexibilityIndex",
                            title = "Flexibility Index",
                            subtitle = paste0("Score over time\n",
                                              "Average Flexibility Index: ", score_tb$FlexibilityIndex),
                            xlab = "Flexibility Index",
                            caption = paste0("% Taking Breaks: ", score_tb$TakeBreaks, "\n",
                                             "% Change Hours: ", score_tb$ChangeHours, "\n",
                                             "% Keep Hours Under Control: ", score_tb$ControlHours, "\n",
                                             extract_date_range(data, return = "text")))

  }
}
