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
#' @author Ainize Cidoncha <ainize.cidoncha@@microsoft.com>
#'
#' @param data A data frame containing email by hours data.
#'
#' @param hrvar A string specifying the HR attribute to cut the data by.
#'   Defaults to NULL. This only affects the function when "table" is returned.
#'
#' @param return Character vector to specify what to return.
#' Valid options include:
#'   - `"plot"`: returns a grid showing the distribution of archetypes by
#'   'breaks' and number of active hours (default)
#'   - `"plot-dist"`: returns a heatmap plot of signal distribution by hour
#'   and archetypes
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
#' @param start_hour A character vector specifying starting hours, e.g.
#'   `"0900"`. Note that this currently only supports **hourly** increments. If
#'   the official hours specifying checking in and 9 AM and checking out at 5
#'   PM, then `"0900"` should be supplied here.
#' @param end_hour A character vector specifying starting hours, e.g. `"1700"`.
#'   Note that this currently only supports **hourly** increments. If the
#'   official hours specifying checking in and 9 AM and checking out at 5 PM,
#'   then `"1700"` should be supplied here.
#'
#' @param exp_hours Numeric value representing the number of hours the population
#'   is expected to be active for throughout the workday. By default, this uses
#'   the difference between `end_hour` and `start_hour`.
#'
#' @param mingroup Numeric value setting the privacy threshold / minimum group
#'   size. Defaults to 5.
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
                                     mingroup = 5,
                                     exp_hours = NULL,
                                     active_threshold = 0,
                                     return = "plot"){

  ## set up variable -------------------------------------------------------
  Active_Hours <- NULL

  ## Handling NULL values passed to hrvar ----------------------------------
  if(is.null(hrvar)){
    data <- totals_col(data)

    ## assign to hrvar_str
    ## So R doesn't get confused
    hrvar_str <- NULL
    hrvar_str <- "Total"

  } else {

    hrvar_str <- hrvar

  }

  ## convert to data.table -------------------------------------------------
  data2 <-
    data %>%
    dplyr::mutate(Date = as.Date(Date, format = "%m/%d/%Y")) %>%
    data.table::as.data.table() %>%
    data.table::copy()

  # Make sure data.table knows we know we're using it
  .datatable.aware = TRUE

  ## Coerce to numeric, remove trailing zeros
  start_hour <- as.numeric(gsub(pattern = "00$", replacement = "", x = start_hour))
  end_hour <- as.numeric(gsub(pattern = "00$", replacement = "", x = end_hour))

  ## Total expected hours --------------------------------------------------
  ## If `NULL`, use the difference between `end_hour` and `start_hour`

  if(is.null(exp_hours)){

    exp_hours <- end_hour - start_hour

  }


  ## Warning message for extreme values of `exp_hours` ---------------------

  if(exp_hours >= 23){

    stop(
      glue::glue(
        "the total working hours is {exp_hours}.
        Please provide a valid range."
        )
    )

  } else if(exp_hours >= 12){

    message(
      glue::glue(
        "Note: the total working hours is {exp_hours}.
        Output archetypes will be reduced as the total number of hours is greater than or equal to 12."
      )
    )

  } else if(exp_hours <= 3){

    message(
      glue::glue(
        "Note: the total working hours is {exp_hours}.
        Output archetypes will be reduced as the total number of hours is fewer than or equal to 3."
      )
    )

  }

  ## Text replacement only for allowed values ------------------------------

  if(any(signals %in% c("email", "IM", "unscheduled_calls", "meetings"))){

    signal_set <- gsub(pattern = "email", replacement = "Emails_sent", x = signals) # case-sensitive
    signal_set <- gsub(pattern = "IM", replacement = "IMs_sent", x = signal_set)
    signal_set <- gsub(pattern = "unscheduled_calls", replacement = "Unscheduled_calls", x = signal_set)
    signal_set <- gsub(pattern = "meetings", replacement = "Meetings", x = signal_set)

  } else {

    stop("Invalid input for `signals`.")

  }

  ## Create 24 summed `Signals_sent` columns -------------------------------

  signal_cols <- purrr::map(0:23, ~combine_signals(data, hr = ., signals = signal_set))
  signal_cols <- bind_cols(signal_cols)

  ## Use names for matching
  input_var <- names(signal_cols)

  ## Signals sent by Person and Date
  ## Data frame with `PersonId`, `Date`, and the 24 signal columns

  signals_df <-
    data2 %>%
    .[, c("PersonId", "Date")] %>%
    cbind(signal_cols)

  ## Signal label
  ## Only show as `Signals_sent` if more than one signal, i.e. if there is
  ## aggregation of multiple signals
  sig_label <- ifelse(length(signal_set) > 1, "Signals_sent", signal_set)

  ## Create binary variable 0 or 1  ----------------------------------------

  num_cols <- names(which(sapply(signals_df, is.numeric))) # Get numeric columns

  signals_df <-
    signals_df %>%
    data.table::as.data.table() %>%
    # active_threshold: minimum signals to qualify as active
    .[, (num_cols) := lapply(.SD, function(x) ifelse(x > active_threshold, 1, 0)), .SDcols = num_cols] %>%
    .[, ("Active_Hours") := apply(.SD, 1, sum), .SDcols = input_var]

  ## Classify PersonId-Signal data by time of day --------------------------
  ## Long format table that classifies each hour of the day on whether it is
  ## before, within, or after standard hours

  WpA_classify <-
    signals_df %>%
    tidyr::gather(!!sym(sig_label), sent, -PersonId,-Date,-Active_Hours) %>%
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
    WpA_classify[, c("PersonId", "Date", "Active_Hours", "HourType", "sent")] %>%
    .[, .(sent = sum(sent)), by = c("PersonId", "Date", "Active_Hours", "HourType")] %>%
    tidyr::spread(HourType, sent)%>%
    left_join(WpA_classify %>%   ## Calculate first and last activity for day_span
                filter(sent > 0)%>%
                group_by(PersonId, Date)%>%
                summarise(First_signal = min(Start),
                          Last_signal = max(End)),
              by = c("PersonId","Date"))%>%
    mutate(Day_Span = Last_signal - First_signal,
           Signals_Break_hours = Day_Span - Active_Hours)


  ## Working patterns classification ---------------------------------------

  # # Level 1 with 7 personas
  # personas_levels <-
  #   c("0 < 3 hours on",
  #     "1 Standard with breaks workday",
  #     "2 Standard continuous workday",
  #     "3 Standard flexible workday",
  #     "4 Long flexible workday",
  #     "5 Long continuous workday",
  #     "6 Always on (13h+)")
  #
  # ptn_data_personas <- data.table::copy(WpA_classify)
  # ptn_data_personas[, Personas := "Unclassified"]
  # ptn_data_personas[Active_Hours > exp_hours & Active_Hours==Day_Span , Personas := "5 Long continuous workday"]
  # ptn_data_personas[Active_Hours > exp_hours & Active_Hours<Day_Span, Personas := "4 Long flexible workday"]
  # ptn_data_personas[Active_Hours <= exp_hours & (Before_start>0|After_end>0), Personas := "3 Standard flexible workday"] #do we want to split betwen block and non block?
  # ptn_data_personas[Active_Hours == exp_hours & Within_hours == exp_hours , Personas := "2 Standard continuous workday"]
  # ptn_data_personas[Active_Hours < exp_hours & Before_start==0 & After_end == 0, Personas := "1 Standard with breaks workday"]
  # ptn_data_personas[Active_Hours >= 13, Personas := "6 Always on (13h+)"]
  # ptn_data_personas[Active_Hours < 3, Personas := "0 < 3 hours on"]
  # ptn_data_personas[, Personas := factor(Personas, levels = personas_levels)]

  # Level 2 with 8 personas
  personas_levels <-
    c(
      "0 Low Activity (< 3 hours on)",
      "1.1 Standard continuous (expected schedule)",
      "1.2 Standard continuous (shifted schedule)",
      "2.1 Standard flexible (expected schedule)",
      "2.2 Standard flexible (shifted schedule)",
      "3 Long flexible workday",
      "4 Long continuous workday",
      "5 Always on (13h+)"
    )

  ptn_data_personas <- data.table::copy(WpA_classify)
  ptn_data_personas[, Personas := "Unclassified"]
  ptn_data_personas[Active_Hours > exp_hours & Active_Hours==Day_Span , Personas := "4 Long continuous workday"]
  ptn_data_personas[Active_Hours > exp_hours & Active_Hours<Day_Span, Personas := "3 Long flexible workday"]
  ptn_data_personas[Active_Hours <= exp_hours & (Before_start>0|After_end>0), Personas := "2.2 Standard flexible (shifted schedule)"]
  ptn_data_personas[Active_Hours <= exp_hours & Before_start == 0 & After_end == 0, Personas := "2.1 Standard flexible (expected schedule)"]
  ptn_data_personas[Active_Hours == exp_hours & (Before_start > 0 | After_end > 0) & Active_Hours == Day_Span, Personas := "1.2 Standard continuous (shifted schedule)"]
  ptn_data_personas[Active_Hours == exp_hours & Before_start == 0 & After_end == 0 & Active_Hours == Day_Span, Personas := "1.1 Standard continuous (expected schedule)"]
  ptn_data_personas[Active_Hours >= 13, Personas := "5 Always on (13h+)"]
  ptn_data_personas[Active_Hours < 3, Personas := "0 Low Activity (< 3 hours on)"]
  ptn_data_personas[, Personas := factor(Personas, levels = personas_levels)]



  # bind cut tree to data frame
  ptn_data_final <-
    ptn_data_personas %>%
    left_join(
      signals_df %>%
        select(-Active_Hours), # Avoid duplication
      by = c("PersonId","Date")) %>%
    left_join(
      data2 %>%
        select(PersonId, Date, hrvar_str), # Avoid duplication
      by = c("PersonId","Date"))

  ## Long caption -----------------------------------------------------------
  ## Parameters used in creating visualization

  ## Change first character to upper case
  firstup <- function(x){
    substr(x, start = 1, stop = 1) <- toupper(substr(x, start = 1, stop = 1))
    x
  }



  signals_str <- firstup(paste(signals, collapse = ", "))

  cap_long <-
    glue::glue(
      "Signals used: {signals_str}.
      The official hours are {start_hour}:00 and {end_hour}:00, with a total of {exp_hours} expected hours.
      \n"
    ) %>%
    paste(extract_date_range(data2, return = "text"))


  ## Return-chunks ----------------------------------------------------------

  return_data <- function(){
    dplyr::as_tibble(ptn_data_final) %>%
      dplyr::mutate(
        Start_hour = start_hour,
        End_hour = end_hour,
        Exp_hours = exp_hours
      )
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
      scale_fill_continuous(
        guide="legend",
        low = "white",
        high = "#1d627e",
        breaks = 0:1,
        name="",
        labels = c("", "Observed activity")
        ) +
      wpa::theme_wpa_basic() +
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
               fill = "gray50") +
      ggplot2::annotate("rect",
               xmin = end_hour + 0.5,
               xmax = 24.5,
               ymin = 0.5,
               ymax = 7.5,
               alpha = .1,
               fill = "gray50")
  }

  # Plot area chart over time -----------------------------------------------

  return_plot_area <- function(){
    ptn_data_final %>%
      dplyr::group_by(Date, Personas) %>%
      dplyr::summarise(n = dplyr::n_distinct(PersonId)) %>%
      dplyr::mutate(per = n / sum(n)) %>%
      dplyr::ungroup() %>%
      tidyr::complete(Date, Personas, fill = list(0)) %>%
      mutate(per = tidyr::replace_na(per, 0)) %>%
      ggplot(aes(x=Date, y=per, fill=Personas)) +
      geom_area() +
      theme_wpa_basic() +
      labs(title = "Distribution of Working Patterns over time",
           y = "Percentage",
           caption = cap_long) +
      theme(legend.position = "right") +
      scale_y_continuous(labels = scales::percent)
  }

  return_table <- function(hrvar = hrvar_str){

    if(hrvar == "Total"){

      ptn_data_final %>%
        data.table::as.data.table() %>%
        .[, .(n = .N), by = Personas] %>%
        .[, prop := n / sum(n)] %>%
        .[] %>%
        dplyr::as_tibble() %>%
        dplyr::arrange(Personas)

    } else {

      # Character containing groups above the minimum group threshold
      PersonCount <-
        ptn_data_final %>%
        hrvar_count(hrvar = hrvar, return = "table") %>%
        dplyr::filter(n >= mingroup) %>%
        dplyr::pull(hrvar)

      ptn_data_final %>%
        data.table::as.data.table() %>%
        .[, .(n = .N), by = c("Personas", hrvar)] %>%
        dplyr::as_tibble() %>%
        dplyr::filter(!!sym(hrvar) %in% PersonCount) %>%
        dplyr::group_by(!!sym(hrvar)) %>%
        dplyr::mutate(prop = n / sum(n, na.rm = TRUE)) %>%
        dplyr::ungroup() %>%
        dplyr::select(-n) %>% # Remove n
        tidyr::spread(!!sym(hrvar), prop) %>%
        dplyr::arrange(Personas)
    }
  }


  ## ReturnR

  if(return == "data"){

    return_data()

  } else if(return == "plot"){

    plot_workpatterns_classify_bw(
      ptn_data_final,
      range = exp_hours,
      caption = cap_long
      )

  } else if(return == "plot-dist"){

    return_plot()

  } else if(return == "plot-area"){

    return_plot_area()

  } else if(return == "plot-hrvar"){

    plot_wp_bw_hrvar(
      x = return_table(),
      caption = cap_long
    )

  } else if (return == "table"){

    return_table()

  } else if (return == "list"){

    list(data = return_data(),
         plot = plot_workpatterns_classify_bw(
           ptn_data_final,
           range = exp_hours,
           caption = cap_long
           ),
         plot_hrvar = plot_wp_bw_hrvar(
           x = return_table(),
           caption = cap_long
           ),
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
#' @param range Numeric. Accepts `exp_hours` from the main `workpatterns_classify_bw()`
#'   function. Used to update labels on main plot.
#' @param caption String to override plot captions.
#'
#' @noRd

plot_workpatterns_classify_bw <- function(data, range, caption){

  plot_table <-
    data %>%
    dplyr::mutate(
      PersonasNet =
        case_when(
          Personas == "0 Low Activity (< 3 hours on)" ~ "Low activity",
          Personas == "2.1 Standard flexible (expected schedule)" ~ "Flexible",
          Personas == "2.2 Standard flexible (shifted schedule)" ~ "Flexible",
          Personas == "1.1 Standard continuous (expected schedule)" ~ "Standard",
          Personas == "1.2 Standard continuous (shifted schedule)" ~ "Standard",
          Personas == "3 Long flexible workday" ~ "Long flexible",
          Personas == "4 Long continuous workday" ~ "Long continuous",
          Personas == "5 Always on (13h+)" ~ "Always On",
          TRUE ~ NA_character_
          )
    ) %>%
    dplyr::count(PersonasNet) %>%
    dplyr::mutate(Percent = n / sum(n, na.rm = TRUE))

  # Create base plot ----------------------------------------------------------

  base_df <-
    dplyr::tibble(id = 1:6,
               value = c("Always On",
                         "Long continuous",
                         "Long flexible",
                         "Standard",
                         "Flexible",
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

  # Vertical labels conditional --------------------------------------------

  if(range <= 12){

    v_labels <-
      c("",
        "< 3 hours",
        "",
        glue::glue("3 - {range} hours"),
        "",
        glue::glue("{range + 1} - 12 hours"),
        "",
        "13+ hours",
        ""
      )

  } else {

    v_labels <-
      c("",
        "< 3 hours",
        "",
        glue::glue("3 - {range} hours"),
        "",
        "",
        "",
        glue::glue("{range}+ hours"),
        ""
      )

  }



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
                       labels = v_labels) +
    scale_x_continuous(breaks = 0:4,
                       labels = c("", "No quiet hours", "",
                                  "Take quiet hours", "")) +
    labs(title = "Distribution of Working Patterns",
         subtitle = "Classification of employee-weeks",
         x = "Flexibility level (breaks)",
         y = "Average activity level",
         caption = caption) +
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
plot_wp_bw_hrvar <- function(x, caption){

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
    theme_wpa_basic() +
    labs(caption = caption)

}

