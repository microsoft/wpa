#' @title Classify working pattern week archetypes using a rule based algorithm,
#' using the binary week-based (bw) method.
#'
#' @description
#' Apply a rule based algorithm to emails sent by hour of day,
#' using the binary week-based (bw) method.
#'
#' @param data A data frame containing email by hours data.
#' @param hrvar A string specifying the HR attribute to cut the data by.
#' Defaults to NULL. This only affects the function when "table" is returned.
#' @param return Character vector to specify what to return.
#' Valid options include "plot" (default), "data", "table" and "plot-area".
#' "plot" returns a bar plot, whilst "plot-area" returns an overlapping area plot.
#' @param signals Character vector to specify which collaboration metrics to use:
#' You may use "email" for emails only, "IM" for Teams messages only,
#' or a combination of the two `c("email", "IM")` (default).
#' @param active_threshold A numeric value specifying the minimum number of signals to be greater than in
#' order to qualify as _active_. Defaults to 0.
#' @param start_hour A character vector specifying start hours,
#' e.g. "0900"
#' @param end_hour A character vector specifying finish hours,
#' e.g. "1700"
#'
#' @import ggplot2
#' @importFrom magrittr "%>%"
#' @importFrom data.table ":=" "%like%" "%between%"
#' @importFrom rlang "!!"
#' @importFrom rlang sym
#'
#' @examples
#' \dontrun{
#' workpatterns_classify_bw(em_data)
#' }
#'
#'
#' @family Work Patterns
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

  # ## Select input variable names
  # if("email" %in% signals & "IM" %in% signals){
  #
  #   ## Create 24 summed `Signals_sent` columns
  #   signal_cols <-
  #     purrr::map(0:23, ~combine_signals(data2, hr = .)) %>%
  #     dplyr::bind_cols()
  #
  #   ## Use names for matching
  #   input_var <- names(signal_cols)
  #
  #   ## Signals sent by Person and date
  #   signals_df <-
  #     data2 %>%
  #     .[, c("PersonId", "Date")] %>%
  #     cbind(signal_cols)
  #
  #   ## Signal label
  #   sig_label <- "Signals_sent"
  #
  # } else if(signals == "IM"){
  #
  #   match_index <- grepl(pattern = "^IMs_sent", x = names(data2))
  #   input_var <- names(data2)[match_index]
  #   input_var2 <- c("PersonId", "Date", input_var)
  #
  #   ## signals sent by Person and date
  #   signals_df <-
  #     data2 %>%
  #     .[, ..input_var2]
  #
  #   sig_label <- "IMs_sent"
  #
  #
  # } else if(signals == "email"){
  #
  #   match_index <- grepl(pattern = "^Emails_sent", x = names(data2))
  #   input_var <- names(data2)[match_index]
  #   input_var2 <- c("PersonId", "Date", input_var)
  #
  #   ## signals sent by Person and date
  #   signals_df <-
  #     data2 %>%
  #     .[, ..input_var2]
  #
  #   sig_label <- "Emails_sent"
  #
  # } else {
  #
  #   stop("Invalid input for `signals`.")
  #
  # }


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
    tidyr::gather(`!!`(sym(sig_label)), sent, -PersonId,-Date,-Signals_Total) %>%
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

  return_plot_area <- function(){
    ptn_data_final %>%
      dplyr::group_by(Date, Personas) %>%
      dplyr::summarise(n = dplyr::n_distinct(PersonId)) %>%
      dplyr::mutate(per = n / sum(n)) %>%
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

    return_plot()

  } else if(return == "plot-area"){

    return_plot_area()

  } else if (return == "table"){

    return_table()

  } else if (return == "list"){

    list(data = return_data(),
         plot = return_plot(),
         plot_area = return_plot_area(),
         table = return_table())

  } else {

    stop("Invalid input for `return`.")

  }
}
