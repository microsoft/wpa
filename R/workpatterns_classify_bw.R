# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Classify working pattern week archetypes using a rule-based algorithm,
#'   using the binary week-based ('bw') method.
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' Apply a rule based algorithm to emails sent by hour of day, using the binary
#' week-based ('bw') method.
#'
#' @param data A data frame containing email by hours data.
#'
#' @param hrvar A string specifying the HR attribute to cut the data by.
#'   Defaults to NULL. This only affects the function when "table" is returned.
#'
#' @param return Character vector to specify what to return.
#' Valid options include:
#'   - `"plot"`: returns a heatmap plot of signal distribution by hour
#'   and archetypes (default)
#'   - `"data"`: returns the raw data with the classified
#'   archetypes
#'   - `"table"`: returns a summary table of the archetypes
#'   - `"plot-area"`: returns an area plot of the percentages of archetypes
#'   shown over time
#'   - `"plot-hrvar"`: returns a bar plot showing the count of archetypes,
#'   faceted by the supplied HR attribute.
#'
#' @param signals Character vector to specify which collaboration metrics to
#'   use:
#'   - a combination of signals, such as `c("email", "IM")` (default)
#'   - `"email"` for emails only
#'   - `"IM"` for Teams messages only
#'   - `"unscheduled_calls"` for Unscheduled Calls only
#'   - `"meetings"` for Meetings only
#'
#' @param active_threshold A numeric value specifying the minimum number of
#'   signals to be greater than in order to qualify as _active_. Defaults to 0.
#'
#' @param start_hour A character vector specifying start hours,
#' e.g. "0900"
#' @param end_hour A character vector specifying finish hours,
#' e.g. "1700"
#'
#' @return
#' A different output is returned depending on the value passed to the `return`
#' argument:
#'   - `"plot"`: returns a summary grid plot of the classified archetypes
#'   (default). A 'ggplot' object.
#'   - `"data"`: returns a data frame of the raw data with the classified
#'   archetypes
#'   - `"table"`: returns a data frame of summary table of the archetypes
#'   - `"plot-area"`: returns an area plot of the percentages of archetypes
#'   shown over time. A 'ggplot' object.
#'   - `"plot-hrvar"`: returns a bar plot showing the count of archetypes,
#'   faceted by the supplied HR attribute. A 'ggplot' object.
#'
#' @import ggplot2
#' @importFrom magrittr "%>%"
#' @importFrom data.table ":=" "%like%" "%between%"
#'
#'
#' @family Working Patterns
#'
workpatterns_classify_bw <- function(data,
                                     hrvar = NULL,
                                     signals = c("email","IM"),
                                     start_hour = "0900",
                                     end_hour = "1700",
                                     active_threshold = 0,
                                     return = "plot"){

  ## assign to hrvar_str
  hrvar_str <- NULL
  hrvar_str <- hrvar

  ## convert to data.table
  data2 <-
    data %>%
    dplyr::mutate(Date = as.Date(Date, format = "%m/%d/%Y")) %>%
    data.table::as.data.table() %>%
    data.table::copy()

  # Make sure data.table knows we know we're using it
  .datatable.aware = TRUE

  ## Save original
  start_hour_o <- start_hour
  end_hour_o <- end_hour

  ## Coerce to numeric, remove trailing zeros
  start_hour <- as.numeric(gsub(pattern = "00$", replacement = "", x = start_hour))
  end_hour <- as.numeric(gsub(pattern = "00$", replacement = "", x = end_hour))

  # Calculate hours within working hours
  d <- (end_hour - start_hour) - 1

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

  ## Signals sent by Person and Date
  signals_df <-
    data2 %>%
    .[, c("PersonId", "Date")] %>%
    cbind(signal_cols)

  ## Signal label
  sig_label <- ifelse(length(signal_set) > 1, "Signals_sent", signal_set)

  ## Create binary variable 0 or 1
  num_cols <- names(which(sapply(signals_df, is.numeric))) # Get numeric columns

  signals_df <-
    signals_df %>%
    data.table::as.data.table() %>%
    # active_threshold: minimum signals to qualify as active
    .[, (num_cols) := lapply(.SD, function(x) ifelse(x > active_threshold, 1, 0)), .SDcols = num_cols] %>%
    .[, ("Signals_Total") := apply(.SD, 1, sum), .SDcols = input_var]

  ## Classify PersonId-Signal data by time of day

  WpA_classify <-
    signals_df %>%
    tidyr::gather(!!sym(sig_label), sent, -PersonId,-Date,-Signals_Total) %>%
    data.table::as.data.table()

  WpA_classify[, StartEnd := gsub(pattern = "[^[:digit:]]", replacement = "", x = get(sig_label))]
  WpA_classify[, Start := as.numeric(substr(StartEnd, start = 1, stop = 2))]
  WpA_classify[, End := as.numeric(substr(StartEnd, start = 3, stop = 4))]
  WpA_classify[, Before_start := Start < (start_hour)] # Earlier than start hour
  WpA_classify[, After_end := End > (end_hour)] # Later than  start hour
  WpA_classify[, Within_hours := (Start >= start_hour & End <= end_hour)]
  WpA_classify[, HourType := NA_character_]
  WpA_classify[After_end == TRUE, HourType := "After_end"]
  WpA_classify[Before_start == TRUE, HourType := "Before_start"]
  WpA_classify[Within_hours == TRUE, HourType := "Within_hours"]


  WpA_classify <-
    WpA_classify[, c("PersonId", "Date", "Signals_Total", "HourType", "sent")] %>%
    .[, .(sent = sum(sent)), by = c("PersonId", "Date", "Signals_Total", "HourType")] %>%
    tidyr::spread(HourType, sent)%>%
    left_join(WpA_classify%>%   ## Calculate first and last activity for day_span
                filter(sent>0)%>%
                group_by(PersonId,Date)%>%
                summarise(First_signal=min(Start),
                          Last_signal=max(End)),
              by=c("PersonId","Date"))%>%
    mutate(Day_Span=Last_signal-First_signal,
           Signals_Break_hours=Day_Span-Signals_Total)


  personas_levels <-
    c("0 < 3 hours on",
      "1 Standard with breaks workday",
      "2 Standard continuous workday",
      "3 Standard flexible workday",
      "4 Long flexible workday",
      "5 Long continuous workday",
      "6 Always on (13h+)")

  ptn_data_personas <- data.table::copy(WpA_classify)
  ptn_data_personas[, Personas := "Unclassified"]
  ptn_data_personas[Signals_Total > d & Signals_Total==Day_Span , Personas := "5 Long continuous workday"]
  ptn_data_personas[Signals_Total > d & Signals_Total<Day_Span, Personas := "4 Long flexible workday"]
  ptn_data_personas[Signals_Total <= d & (Before_start>0|After_end>0), Personas := "3 Standard flexible workday"] #do we want to split betwen block and non block?
  ptn_data_personas[Signals_Total == d+1 & Within_hours ==d+1, Personas := "2 Standard continuous workday"]
  ptn_data_personas[Signals_Total<= d & Before_start==0 & After_end == 0, Personas := "1 Standard with breaks workday"]
  ptn_data_personas[Signals_Total >= 13, Personas := "6 Always on (13h+)"]
  ptn_data_personas[Signals_Total < 3, Personas := "0 < 3 hours on"]
  ptn_data_personas[, Personas := factor(Personas, levels = personas_levels)]

  # bind cut tree to data frame
  ptn_data_final <-
    ptn_data_personas %>%
    left_join(signals_df, by = c("PersonId","Date"))

  ## Return-chunks
  return_data <- function(){
    dplyr::as_tibble(ptn_data_final)
  }

  # NOW DEFUNCT - NOT USED ---------------------------------------------------
  return_plot <- function(){
    ## Table for annotation
    myTable_legends <-
      ptn_data_final %>%
      dplyr::as_tibble() %>%
      dplyr::group_by(Personas) %>%
      dplyr::summarise(WeekCount = n()) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(WeekPercentage = WeekCount / sum(WeekCount, na.rm = TRUE)) %>%
      dplyr::mutate(WeekCount = paste0("n= ", WeekCount, " \n(", scales::percent(WeekPercentage, accuracy = 0.1), ")"))

    ptn_data_final %>%
      dplyr::as_tibble() %>%
      dplyr::select(Personas, dplyr::starts_with(sig_label)) %>%
      dplyr::group_by(Personas) %>%
      dplyr::summarise_at(vars(dplyr::starts_with(paste0(sig_label, "_"))), ~mean(.)) %>%
      purrr::set_names(nm = gsub(pattern = paste0(sig_label, "_"), replacement = "", x = names(.))) %>%
      purrr::set_names(nm = gsub(pattern = "_.+", replacement = "", x = names(.))) %>%
      tidyr::gather(Hours, Freq, -Personas) %>%
      ggplot(aes(x = Hours, y = Personas, fill = Freq)) +
      geom_tile(height=.5) +
      # geom_text(aes(label = percent(Freq)), size = 3) +
      labs(title = "Distribution of Signals by Hour",
           subtitle = "Weekly Working Patterns Archetypes") +
      scale_fill_gradient2(low = "white", high = "red") +
      wpa::theme_wpa_basic() +
      ggplot2::theme(legend.position = "none") +
      ggplot2::annotate("text",
                        y = myTable_legends$Personas,
                        x = 26.5,
                        label = myTable_legends$WeekCount, size = 3) +
      ggplot2::annotate("rect",
                        xmin = 25,
                        xmax = 28,
                        ymin = 0.5,
                        ymax = length(myTable_legends$Personas) + 0.5,
                        alpha = .2) +
      ggplot2::annotate("rect",
               xmin = 0.5,
               xmax = start_hour + 0.5,
               ymin = 0.5,
               ymax = 7.5,
               alpha = .1,
               fill = "red") +
      ggplot2::annotate("rect",
               xmin = end_hour + 0.5,
               xmax = 24.5,
               ymin = 0.5,
               ymax = 7.5,
               alpha = .1,
               fill = "red")
  }

  # Plot area chart over time -----------------------------------------------

  return_plot_area <- function(){
    ptn_data_final %>%
      dplyr::group_by(Date, Personas) %>%
      dplyr::summarise(n = dplyr::n_distinct(PersonId)) %>%
      dplyr::mutate(per = n / sum(n)) %>%
      tidyr::complete(Date, Personas, fill = list(0)) %>%
      mutate(per = tidyr::replace_na(per, 0)) %>%
      ggplot(aes(x=Date, y=per, fill=Personas)) +
      geom_area() +
      theme_wpa_basic() +
      labs(title = "Distribution of Working Patterns over time",
           y = "Percentage",
           caption = extract_date_range(data2, return = "text")) +
      theme(legend.position = "right")
  }

  return_table <- function(hrvar = hrvar_str){

    if(is.null(hrvar)){

      ptn_data_final %>%
        data.table::as.data.table() %>%
        .[, .(n = .N), by = Personas] %>%
        .[, prop := n / sum(n)] %>%
        .[] %>%
        dplyr::as_tibble() %>%
        dplyr::arrange(Personas)

    } else {

      ptn_data_final %>%
        dplyr::left_join(select(data2, PersonId, Date, hrvar), by = c("PersonId", "Date")) %>%
        data.table::as.data.table() %>%
        .[, .(n = .N), by = c("Personas", hrvar)] %>%
        .[n >= 5, ] %>%
        .[, prop := n / sum(n), by = hrvar] %>% # % breakdown by HR org
        dplyr::as_tibble() %>%
        dplyr::select(-n) %>% # Remove n
        tidyr::spread(!!sym(hrvar), prop) %>%
        dplyr::arrange(Personas)
    }
  }


  ## Return

  if(return == "data"){

    return_data()

  } else if(return == "plot"){

    plot_workpatterns_classify_bw(ptn_data_final)

  } else if(return == "plot-area"){

    return_plot_area()

  } else if(return == "plot-hrvar"){

    plot_wp_bw_hrvar(
      x = return_table()
    )

  } else if (return == "table"){

    return_table()

  } else if (return == "list"){

    list(data = return_data(),
         plot = plot_workpatterns_classify_bw(ptn_data_final),
         plot_area = return_plot_area(),
         table = return_table())

  } else {

    stop("Invalid input for `return`.")

  }
}

#' @title Plotting function for `workpatterns_classify_bw()`
#'
#' @description Internal use only.
#'
#' @noRd

plot_workpatterns_classify_bw <- function(data){

  plot_table <-
    data %>%
    dplyr::mutate(
      PersonasNet =
        case_when(
          Personas == "0 < 3 hours on" ~ "Low activity",
          Personas == "1 Standard with breaks workday" ~ "Standard flexible",
          Personas == "2 Standard continuous workday" ~ "Standard (non-stop)",
          Personas == "3 Standard flexible workday" ~ "Standard flexible",
          Personas == "4 Long flexible workday" ~ "Long with breaks",
          Personas == "5 Long continuous workday" ~ "Long (non-stop)",
          Personas == "6 Always on (13h+)" ~ "Always On",
          TRUE ~ NA_character_
          )
    ) %>%
    dplyr::count(PersonasNet) %>%
    dplyr::mutate(Percent = n / sum(n, na.rm = TRUE))

  # Create base plot ----------------------------------------------------------

  base_df <-
    dplyr::tibble(id = 1:6,
               value = c("Always On",
                         "Long (non-stop)",
                         "Long with breaks",
                         "Standard (non-stop)",
                         "Standard flexible",
                         "Low activity"),
               text = c(rep("#FFFFFF", 3),
                        rep("grey5", 3)),
               fill = c(
                 rgb2hex(50, 83, 105),
                 rgb2hex(69, 113, 138),
                 rgb2hex(65, 150, 168),
                 rgb2hex(175, 175, 175),
                 rgb2hex(114, 194, 217),
                 rgb2hex(221, 221, 221)
               ))

  flexibility <-
    c(0, 0, 4, 4, # Always On
      0, 0, 2, 2, # Long (non-stop)
      2, 2, 4, 4, # Long with breaks
      0, 0, 2, 2, # Standard (non-stop)
      2, 2, 4, 4, # Standard flexible
      0, 0, 4, 4) # Low activity

  act_level <-
    c(6, 8, 8, 6, # Always On
      4, 6, 6, 4, # Long (non-stop)
      4, 6, 6, 4, # Long with breaks
      2, 4, 4, 2, # Standard (non-stop)
      2, 4, 4, 2, # Standard flexible
      0, 2, 2, 0) # Low activity

  main_plot_df <-
    rbind(base_df,
          base_df,
          base_df,
          base_df) %>%
    arrange(id) %>%
    mutate(Flexibility = flexibility,
           ActivityLevel = act_level)

  label_df <-
    main_plot_df %>%
    group_by(id, value) %>%
    # Get mid-points of coordinates
    summarise(flexibility = mean(Flexibility),
              act_level = mean(ActivityLevel),
              .groups = "drop") %>%
    left_join(
      plot_table,
      by = c("value" = "PersonasNet")
    ) %>%
    # Get colours and fill back into the data
    left_join(
      base_df,
      by = c("id", "value")
    ) %>%
    mutate(value2 = sub(" ", "\n", value)# ,
           # text = ifelse(id %in% c(1, 2, 5, 6),"#FFFFFF", "black"),
           # fill = ifelse(id %in% c(1, 2, 5, 6),"#1d627e", "#34b1e2")
           ) %>%
    mutate(Percent = paste(round(Percent * 100), "%")) %>%
    mutate(Percent = ifelse(is.na(Percent), "0 %", Percent)) %>%
    mutate(value2 = paste(value2, Percent, sep = "\n"))

  colo_v <- setNames(base_df$fill, nm = base_df$value)
  text_v <- setNames(base_df$text, nm = base_df$value)

  ## UNCOMMENT TO DEBUG
  # list(
  #   base_df,
  #   colo_v,
  #   text_v,
  #   main_plot_df,
  #   label_df
  # ) %>%
  #   print()

  main_plot_df %>%
    ggplot(aes(x = Flexibility, y = ActivityLevel)) +
    geom_polygon(
      aes(fill = value,
          group = id),
      colour = "#FFFFFF",
      size = 2) +
    scale_fill_manual(values = colo_v) +
    geom_text(data = label_df,
              aes(x = flexibility,
                  y = act_level,
                  label = value2,
                  colour = value),
              size = 5) +
    scale_colour_manual(values = text_v) +
    scale_y_continuous(breaks = 0:8,
                       labels = c("",
                                  "< 3 hours", "",
                                  "3 - 9 hours", "",
                                  "9 - 12 hours", "",
                                  "13+ hours", ""
                                  )) +
    scale_x_continuous(breaks = 0:4,
                       labels = c("", "No breaks", "",
                                  "Breaks", "")) +
    labs(title = "Distribution of Working Patterns",
         subtitle = "Classification of employee-weeks",
         x = "Flexibility level (breaks)",
         y = "Average activity level",
         caption = extract_date_range(data, return = "text")) +
    theme_wpa_basic() +
    theme(
      legend.position = "none",
      axis.line = element_blank(),
      axis.title = element_blank() # Toggle off axis title
      )
}

#' @title
#' Plotting a faceted bar plot for `workpatterns_classify_bw()`
#'
#' @description Internal use only.
#'
#' @details
#' Accepts a `"table"` input.
#'
#' @import ggplot2
#'
#' @noRd
plot_wp_bw_hrvar <- function(x){

  x %>%
    tidyr::pivot_longer(cols = -Personas) %>%
    ggplot(aes(x = Personas,
               y = value)) +
    geom_col(fill = "lightblue") +
    facet_wrap(. ~ name) +
    geom_text(aes(label = scales::percent(value, accuracy = 1)),
              size = 3,
              hjust = -0.3) +
    coord_flip() +
    scale_y_continuous(labels = scales::percent,
                       limits = c(NA, 1)) +
    theme_wpa_basic()

}

