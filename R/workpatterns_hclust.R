# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Create a hierarchical clustering of email or IMs by hour of day
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' Apply hierarchical clustering to emails sent by hour of day.
#' The hierarchical clustering uses cosine distance and the ward.D method
#' of agglomeration.
#'
#' @details
#' The hierarchical clustering is applied on the person-average volume-based (pav) level.
#' In other words, the clustering is applied on a dataset where the collaboration hours
#' are averaged by person and calculated as % of total daily collaboration.
#'
#' @param data A data frame containing data from the Hourly Collaboration query.
#' @param k Numeric vector to specify the `k` number of clusters to cut by.
#'
#' @param return String specifying what to return. This must be one of the
#'   following strings:
#'   - `"plot"`
#'   - `"data"`
#'   - `"table"`
#'   - `"plot-area"`
#'   - `"hclust"`
#'   - `"dist"`
#'
#' See `Value` for more information.
#'
#' @param values Character vector to specify whether to return percentages
#' or absolute values in "data" and "plot". Valid values are:
#'   - "percent": percentage of signals divided by total signals (default)
#'   - "abs": absolute count of signals
#'
#' @param signals Character vector to specify which collaboration metrics to use:
#'   - `"email"` (default) for emails only
#'   - `"IM"` for Teams messages only
#'   - `"unscheduled_calls"` for Unscheduled Calls only
#'   - `"meetings"` for Meetings only
#'   - or a combination of signals, such as `c("email", "IM")`
#' @param start_hour A character vector specifying starting hours,
#' e.g. "0900"
#' @param end_hour A character vector specifying starting hours,
#' e.g. "1700"
#'
#' @return
#' A different output is returned depending on the value passed to the `return`
#' argument:
#'   - `"plot"`: ggplot object of a bar plot (default)
#'   - `"data"`: data frame containing raw data with the clusters
#'   - `"table"`: data frame containing a summary table. Percentages of signals
#'   are shown, e.g. x% of signals are sent by y hour of the day.
#'   - `"plot-area"`: ggplot object. An overlapping area plot
#'   - `"hclust"`: `hclust` object for the hierarchical model
#'   - `"dist"`: distance matrix used to build the clustering model
#'
#' @import dplyr
#' @import tidyselect
#' @import ggplot2
#' @importFrom proxy dist
#' @importFrom stats hclust
#' @importFrom stats rect.hclust
#' @importFrom stats cutree
#' @importFrom tidyr replace_na
#'
#' @examples
#' \donttest{
#' # Run clusters, returning plot
#' workpatterns_hclust(em_data, k = 5, return = "plot")
#'
#' # Run clusters, return raw data
#' workpatterns_hclust(em_data, k = 4, return = "data") %>% head()
#'
#' # Run clusters for instant messages only, return hclust object
#' workpatterns_hclust(em_data, k = 4, return = "hclust", signals = c("IM"))
#' }
#'
#' @family Clustering
#' @family Working Patterns
#'
#' @export
workpatterns_hclust <- function(data,
                                k = 4,
                                return = "plot",
                                values = "percent",
                                signals = "email",
                                start_hour = "0900",
                                end_hour = "1700"){

  # Text replacement only for allowed values

  if(any(signals %in% c("email", "IM", "unscheduled_calls", "meetings"))){

    signal_set <- gsub(pattern = "email", replacement = "Emails_sent", x = signals) # case-sensitive
    signal_set <- gsub(pattern = "IM", replacement = "IMs_sent", x = signal_set)
    signal_set <- gsub(pattern = "unscheduled_calls", replacement = "Unscheduled_calls", x = signal_set)
    signal_set <- gsub(pattern = "meetings", replacement = "Meetings", x = signal_set)

  } else {

    stop("Invalid input for `signals`.")

  }

  ## Create 24 summed `Signals_sent` columns
  signal_cols <- purrr::map(0:23, ~combine_signals(data, hr = ., signals = signal_set))
  signal_cols <- bind_cols(signal_cols)

  ## Use names for matching
  input_var <- names(signal_cols)

  ## Average signals sent by Person
  signals_df <-
    data %>%
    select(PersonId) %>%
    cbind(signal_cols) %>%
    group_by(PersonId) %>%
    summarise_all(~mean(.))

  ## Signal label
  sig_label <- ifelse(length(signal_set) > 1, "Signals_sent", signal_set)

  ## Normalised pattern data
  ptn_data_norm <-
    signals_df %>%
    mutate(Signals_Total = select(., all_of(input_var)) %>% apply(1, sum)) %>%
    mutate_at(vars(input_var), ~./Signals_Total) %>%
    filter(Signals_Total > 0) %>%
    select(PersonId, all_of(input_var)) %>%
    mutate(across(where(is.numeric), ~tidyr::replace_na(., 0))) # Replace NAs with 0s

  ## Distance matrix
  dist_m <-
    ptn_data_norm %>%
    select(all_of(input_var)) %>%
    proxy::dist(method = "cosine")

  ## Run hclust
  h_clust <-
    dist_m %>%
    stats::hclust(method = "ward.D")

  ## Cut tree
  cuts <- stats::cutree(h_clust, k = k)

  # Percentage vs Absolutes
  if(values == "percent"){

    # bind cut tree to data frame
    ptn_data_final <- cbind(ptn_data_norm, "cluster" = cuts)

  } else if(values == "abs"){

    ptn_data_final <-
      ptn_data_norm %>%
      select(PersonId) %>%
      cbind("cluster" = cuts) %>%
      left_join(signals_df, by = "PersonId")

  } else {

    stop("Invalid `values` input. Please either input 'percent' or 'abs'.")

  }

  ## Return
  if(return == "data"){

    return(ptn_data_final)

  } else if(return == "plot"){

    plot <-
      plot_signal_clust(ptn_data_final,
                        group_label = "cluster",
                        type = "bar",
                        sig_label = sig_label)

    return(plot)

  } else if(return == "plot-area"){

    plot <-
      plot_signal_clust(ptn_data_final,
                        group_label = "cluster",
                        type = "area",
                        sig_label = sig_label)

    return(plot)

  } else if(return == "table"){

    ## Count table
    count_tb <-
      ptn_data_final %>%
      group_by(cluster) %>%
      summarise(n = n()) %>%
      mutate(prop = n / sum(n))

    ## Summary statistics
    sums_tb <-
      ptn_data_final %>%
      run_sum_hr(sig_label = sig_label)

    ## Time slots
    times_tb <-
      ptn_data_final %>%
      run_hour_splits(start_hour = start_hour,
                      end_hour = end_hour,
                      group_label = "cluster")

    count_tb %>%
      left_join(sums_tb, by = "cluster") %>%
      left_join(times_tb, by = "cluster") %>%
      return()

  } else if(return == "hclust"){

    return(h_clust)

  } else if(return == "dist"){

    return(dist_m)

  } else {

    stop("Invalid input for `return`.")

  }
}

#' @title Plot signal hour patterns by clusters
#'
#' @description Workhorse function feeding into `workpatterns_hclust()`.
#' This is a build on `plot_email_clust` to allow flexible inputs.
#'
#' @param data Data frame containing the normalised email by hour
#' data and the cluster variable
#'
#' @param group_label Character vector specifying the name of the variable
#' containing the clusters.
#'
#' @param type Character vector to specify type of plot to return.
#' Accepted values are "bar" (default) and "area".
#'
#' @param sig_label Character vector to select required columns,
#' e.g. "Emails_sent", "IMs_sent", "Signals_sent"
#'
#' @import ggplot2
#' @import dplyr
#' @importFrom tidyselect all_of
#' @importFrom tidyr gather
#'
#' @noRd
plot_signal_clust <- function(data,
                              group_label,
                              type = "bar",
                              sig_label = "Emails_sent"){

  metric_label <- sub(pattern = "_sent", replacement = "", x = sig_label)

  ## Maximum value
  max_val <-
    data %>%
    select(starts_with(sig_label)) %>%
    gather(key, value) %>%
    pull(value) %>%
    max()

  ## Absolute vs Percent
  abs_true <-
    ifelse(max_val > 1, TRUE, FALSE)

  ## Create labels for n
  ptn_data_n <-
    data %>%
    rename(clusters = tidyselect::all_of(group_label)) %>%
    count(clusters) %>%
    mutate(label = paste0(clusters, "\n\n", "n=",n))

  ## Create plotting data
  plot_data <-
    data %>%
    rename(clusters = tidyselect::all_of(group_label)) %>%
    select(clusters, starts_with(sig_label)) %>%
    group_by(clusters) %>%
    summarise_at(vars(starts_with(sig_label)), ~mean(.)) %>%
    tidyr::gather(Hours, !!sym(sig_label), -clusters) %>%
    left_join(ptn_data_n %>% select(clusters, label),
              by = "clusters") %>%
    mutate_at("Hours", ~sub(pattern = paste0(sig_label, "_"), replacement = "", x = .)) %>%
    mutate_at("Hours", ~sub(pattern = "_.+", replacement = "", x = .))

  ## bar plot
  output_bar <-
    plot_data %>%
    ggplot(aes(x = Hours, y = !!sym(sig_label))) +
    geom_col(fill = "#2d7a8a") +
    theme_minimal() +
    {if (abs_true)
      scale_y_continuous(labels = round)
      else
        scale_y_continuous(labels = scales::percent)
    } +
    theme_wpa_basic() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    facet_grid(label~.) +
    ggtitle(label = paste("Distribution of", metric_label, "by Hour"),
            subtitle = group_label) +
    {if (abs_true) ylab(paste(metric_label, "sent (absolute)"))
      else ylab(paste(metric_label, "sent (percentage of daily total"))
    }

  ## overlapping area plot
  output_area <-
    plot_data %>%
    mutate_at("Hours", ~as.numeric(.)) %>%
    mutate_at("clusters", ~as.factor(.)) %>%
    ggplot(aes(x = Hours, y = !!sym(sig_label), colour = clusters)) +
    geom_line(size = 1) +
    geom_area(aes(fill = clusters), alpha = 0.2, position = 'identity') +
    theme_wpa_basic() +
    scale_x_continuous(labels = pad2) +
    scale_y_continuous(labels = round) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    ggtitle(label = paste("Distribution of", metric_label, "by Hour"),
            subtitle = group_label) +
    {if (abs_true) ylab(paste(metric_label, "sent (absolute)"))
      else ylab(paste(metric_label, "sent (percentage of daily total)"))
    }

  if(type == "bar"){

    return(output_bar)

  } else if(type == "area"){

    return(output_area)

  } else {

    stop("Invalid `type` input.")

  }
}

#' @title Run summary statistics for hours of day
#'
#' @description
#' Supporting function that takes the raw data from `workpatterns_hclust()`
#' as an input.
#'
#'
#' @import dplyr
#' @param data See `workpatterns_hclust()`.
#' @param group_label A character vector for the column name containing the clusters
#' or the groups. Currently accepted values are "cluster" and "Personas".
#' @param sig_label See `workpatterns_hclust()`.
#'
#' @noRd
run_sum_hr <- function(data,
                       group_label = "cluster",
                       sig_label = "Emails_sent"){
  data %>%
    rename(cluster = tidyselect::all_of(group_label)) %>%
    group_by(cluster) %>%
    summarise_at(vars(starts_with(sig_label)), ~mean(.)) %>%
    tidyr::gather(Signals, prop, -cluster) %>%
    arrange(-prop) %>%
    mutate(Hours = gsub(pattern = "[^[:digit:]{2}]", replacement = "", x = Signals)) %>%
    mutate(Hours = substr(Hours, start = 1, stop = 2)) %>%
    mutate(Hours = as.numeric(Hours)) %>%
    select(cluster, prop, Hours) %>%
    dplyr::group_split(cluster) %>%
    purrr::map(function(x){

      hour_raw <-
        x %>%
        mutate_at("prop", ~.*1000) %>%
        with(rep(Hours, prop))

      ## Unique values of hour_raw
      uni <- unique(hour_raw)
      ## Mode
      mode_val <- uni[which.max(tabulate(match(hour_raw, uni)))]


      dplyr::tibble(cluster = x$cluster[1],
                     median_hour = median(hour_raw),
                     p5_hour = stats::quantile(hour_raw, .05),
                     p25_hour = stats::quantile(hour_raw, .25),
                     p75_hour = stats::quantile(hour_raw, .75),
                     p95_hour = stats::quantile(hour_raw, .95),
                     mode_hour = mode_val,
                     sd_hour = sd(hour_raw),
                     mean_hour = mean(hour_raw))

    }) %>%
    dplyr::bind_rows() %>%
    rename(!!sym(group_label) := "cluster") %>%
    return()
}

#' @title Run working hour splits for signals sent
#'
#' @description
#' Used internally within `workpatterns_hclust()`
#'
#' @import dplyr
#'
#' @param data See `workpatterns_hclust()`.
#' @param start_hour See `workpatterns_hclust()`.
#' @param end_hour See `workpatterns_hclust()`.
#' @param group_label See `workpatterns_hclust()`.
#'
#' @noRd
run_hour_splits <- function(data,
                            start_hour = "0900",
                            end_hour = "1700",
                            group_label = "cluster"){

  ## Coerce to numeric
  start_hour <- as.numeric(sub(pattern = "00$", replacement = "", x = start_hour))
  end_hour <- as.numeric(sub(pattern = "00$", replacement = "", x = end_hour))

  data %>%
    rename(cluster = tidyselect::all_of(group_label)) %>%
    tidyr::gather(Signals, Prop, -PersonId, -cluster) %>%
    mutate(StartEnd = gsub(pattern = "[^[:digit:]]", replacement = "", x = Signals),
           Start = as.numeric(substr(StartEnd, start = 1, stop = 2)),
           End = as.numeric(substr(StartEnd, start = 3, stop = 4))) %>%
    mutate(Before_start = (Start < start_hour)) %>% # Earlier than working hours
    mutate(After_end = (End > end_hour)) %>% # Later than working hours
    mutate(Within_hours = (Start >= start_hour & End <= end_hour)) %>%
    mutate(HourType = case_when(Before_start == TRUE ~ "Before_start",
                                After_end == TRUE ~ "After_end",
                                Within_hours == TRUE ~ "Within_hours",
                                TRUE ~ NA_character_)) %>%
    select(PersonId, HourType, Prop, cluster) %>%
    group_by(cluster, PersonId, HourType) %>%
    summarise(Prop = sum(Prop)) %>%
    tidyr::spread(HourType, Prop) %>%
    ungroup() %>%
    group_by(cluster) %>%
    summarise_at(vars(Before_start, Within_hours, After_end),
                 ~mean(.)) %>%
    rename(!!sym(group_label) := "cluster") %>%
    return()
}
