# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Create an area plot of emails and IMs by hour of the day
#'
#' @description
#' Uses the Hourly Collaboration query to produce an area plot of
#' Emails sent and IMs sent attended by hour of the day.
#'
#' @param data A data frame containing data from the Hourly Collaboration query.
#' @param hrvar HR Variable by which to split metrics. Accepts a character vector,
#' defaults to "Organization" but accepts any character vector, e.g. "LevelDesignation"
#' @param mingroup Numeric value setting the privacy threshold / minimum group size, defaults to 5.
#' @param return Character vector to specify what to return.
#' Valid options include "plot" (default) and "table".
#' "plot" returns an overlapping area plot.
#' "table" returns a summary table.
#' @param values Character vector to specify whether to return percentages
#' or absolute values in "data" and "plot". Valid values are "percent" (default)
#' and "abs".
#' @param start_hour A character vector specifying starting hours,
#' e.g. "0900"
#' @param end_hour A character vector specifying starting hours,
#' e.g. "1700"
#'
#'
#' @import dplyr
#' @import tidyselect
#' @import ggplot2
#' @importFrom tidyr replace_na
#'
#' @examples
#' ## Return visualization of percentage distribution
#' workpatterns_area(em_data, return = "plot", values = "percent")
#'
#' ## Return visualization of absolute values
#' workpatterns_area(em_data, return = "plot", values = "abs")
#'
#' ## Return a table
#' workpatterns_area(em_data, return = "table")
#'
#' @family Work Patterns
#'
#' @export
workpatterns_area <- function(data,
                              hrvar = "Organization",
                              mingroup = 5,
                              return = "plot",
                              values = "percent",
                              start_hour = "0900",
                              end_hour = "1700"){

  ## Match all relevant signal columns
  match_index <- grepl(pattern = "^IMs_sent|^Emails_sent", x = names(data))

  input_var <- names(data)[match_index]

  input_var_im <- input_var[grepl("^IMs_sent", input_var)]
  input_var_em <- input_var[grepl("^Emails_sent", input_var)]

  ## Date range data frame
  myPeriod <- extract_date_range(data)

  ## Average signals sent by Person
  signals_df <-
    data %>%
    select(PersonId, hrvar, all_of(input_var)) %>%
    rename(group = !!sym(hrvar)) %>%
    group_by(PersonId, group) %>%
    summarise_all(~mean(.)) %>%
    ungroup() %>%
    left_join(data %>%
                rename(group = !!sym(hrvar)) %>%
                group_by(group) %>%
                summarise(Employee_Count = n_distinct(PersonId)),
              by = "group") %>%
    filter(Employee_Count >= mingroup)

  ## Normalised pattern data
  ptn_data_norm <-
    signals_df %>%
    # Normalise IMs
    mutate(IMs_Total = select(., all_of(input_var_im)) %>% apply(1, sum)) %>%
    mutate_at(vars(input_var_im), ~./IMs_Total) %>%
    # Normalise emails
    mutate(Emails_Total = select(., all_of(input_var_em)) %>% apply(1, sum)) %>%
    mutate_at(vars(input_var_em), ~./Emails_Total) %>%

    select(PersonId, group, all_of(input_var)) %>%
    mutate_all(~tidyr::replace_na(., 0)) # Replace NAs with 0s

  # Percentage vs Absolutes
  if(values == "percent"){

    # Use normalised data
    ptn_data_final <- ptn_data_norm
    abs_true <- FALSE

  } else if(values == "abs"){

    # Use data with direct person-average
    ptn_data_final <- signals_df
    abs_true <- TRUE

  } else {

    stop("Invalid `values` input. Please either input 'percent' or 'abs'.")

  }

  ## Create summary table
  summary_data <-
    ptn_data_final %>%
    group_by(group) %>%
    summarise_at(vars(input_var), ~mean(.)) %>%
    gather(Signals, Value, -group) %>%
    mutate(Hours = sub(pattern = "^IMs_sent_|^Emails_sent_", replacement = "", x = Signals)) %>%
    mutate(Hours = sub(pattern = "_.+", replacement = "", x = Hours)) %>%
    mutate(Hours = as.numeric(Hours)) %>%
    mutate(Signals = sub(pattern = "_\\d.+", replacement = "", x = Signals)) %>%
    spread(Signals, Value)

  ## Create the two-digit zero-padded format
  ## Used in `scale_x_continuous()`
  pad2 <- function(x){
    x <- as.character(x)

    ifelse(nchar(x) == 1, paste0("0", x), x)
  }

  ## Return
  if(return == "data"){

    return(ptn_data_final)

  } else if(return == "plot"){

    start_line <- as.numeric(gsub(pattern = "0", replacement = "", x = start_hour))
    end_line <- as.numeric(gsub(pattern = "0", replacement = "", x = end_hour))

    plot <-
      summary_data %>%
      mutate_at(vars(group), ~as.factor(.)) %>%
      gather(Signals, Value, -group, -Hours) %>%
      ggplot(aes(x = Hours, y = Value, colour = Signals)) +
      geom_line(size = 1) +
      geom_area(aes(fill = Signals), alpha = 0.2, position = 'identity') +
      geom_vline(xintercept = c(start_line, end_line), linetype="dotted") +
      scale_x_continuous(labels = pad2) +
      facet_wrap(.~group) +
      theme_wpa_basic() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      ggtitle(label = "Distribution of collaboration activities\n by hour of day",
              subtitle = paste("By", camel_clean(hrvar))) +
      labs(caption = paste("Data from week of", myPeriod$Start, "to week of", myPeriod$End)) +
      {if (abs_true) ylab(paste("Collaboration activity\n(absolute)"))
        else ylab(paste("Collaboration activity\n(percentage of daily total)"))
      }

    return(plot)

  } else if(return == "table"){

    ## Count table
    count_tb <-
      ptn_data_final %>%
      group_by(group) %>%
      summarise(n = n_distinct(PersonId))

    summary_data %>%
      left_join(count_tb, by = "group") %>%
      return()

  } else if(return == "hclust"){

    return(h_clust)

  } else if(return == "dist"){

    return(dist_m)

  } else {

    stop("Invalid input for `return`.")

  }
}
