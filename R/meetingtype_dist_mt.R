#' @title Meeting Type Distribution (Meeting Query)
#'
#' @description
#' Calculate the hour distribution of internal meeting types,
#' using a Meeting Query with core WpA variables as an input.
#'
#' @param data Meeting Query data frame. Must contain the variables `Attendee` and `DurationHours`
#' @param return Character vector specifying what to return, defaults to "plot".
#' Valid inputs are "plot" and "table".
#'
#' @import ggplot2
#' @import dplyr
#' @importFrom stats setNames
#'
#' @export
meetingtype_dist_mt <- function(data, return = "plot"){

  ## Date range data frame
  myPeriod <- extract_date_range(data)

  data_typed <-
    data %>%
    mutate(MeetingType = case_when(
      Attendees == 2 & DurationHours <= 1 ~ "One on one",
      Attendees >= 3 & Attendees <= 8 & DurationHours <= 1  ~ "Decision making",
      Attendees >= 9 & Attendees <= 18 & DurationHours <= 1  ~ "Status update",
      Attendees >= 19 & Attendees <= 50 & DurationHours <= 2  ~ "Bloated",
      Attendees <= 18 & DurationHours >= 1 & DurationHours <= 2 ~ "Lengthy",
      Attendees >= 51 ~ "All hands",
      Attendees <= 50 & DurationHours >= 2 ~ "Workshop",
      TRUE ~ NA_character_))

  myResultsTable <-
    data_typed %>%
    group_by(MeetingType) %>%
    summarise(AttendeeMeetingHours = sum(Attendee_meeting_hours),
              TotalMeetings = n()) %>%
    mutate(Prop = AttendeeMeetingHours / sum(AttendeeMeetingHours)) %>%
    mutate(Percent = paste(round(Prop * 100), "%"))

  myResultsTableTotal <-
    data_typed %>%
    summarise(AttendeeMeetingHours = sum(Attendee_meeting_hours),
              TotalMeetings = n()) %>%
    mutate(Prop = AttendeeMeetingHours / sum(AttendeeMeetingHours),
           Percent = paste(round(Prop * 100), "%"),
           MeetingType = "Total") %>%
    select(MeetingType, tidyselect::everything())

  base_df <-
    data.frame(id = 1:7,
               value = c("All hands",
                         "Bloated",
                         "Status update",
                         "Decision making",
                         "One on one",
                         "Lengthy",
                         "Workshop"))

  duration <-
    c(0, 0, 3, 3,
      0, 0, 2, 2,
      0, 0, 1, 1,
      0, 0, 1, 1,
      0, 0, 1, 1,
      1, 1, 2, 2,
      2, 2, 3, 3)

  attendees <-
    c(4, 5, 5, 4,
      3, 4, 4, 3,
      2, 3, 3, 2,
      1, 2, 2, 1,
      0, 1, 1, 0,
      0, 3, 3, 0,
      0, 4, 4, 0)

  main_plot_df <-
    rbind(base_df,
          base_df,
          base_df,
          base_df) %>%
    arrange(id) %>%
    mutate(Duration = duration,
           Attendees = attendees)


  label_df <-
    main_plot_df %>%
    group_by(id, value) %>%
    summarise(x = mean(Duration),
              y = mean(Attendees)) %>%
    mutate(value2 = sub(" ", "\n", value),
           text = ifelse(id %in% c(1, 2, 6, 7),"#FFFFFF", "black"),
           fill = ifelse(id %in% c(1, 2, 6, 7),"#1d627e", "#34b1e2")) %>%
    left_join(select(myResultsTable, MeetingType, Percent),
              by = c("value" = "MeetingType")) %>%
    mutate(Percent = ifelse(is.na(Percent), "0 %", Percent)) %>%
    mutate(value2 = paste(value2, Percent, sep = "\n"))

  colo_v <- setNames(label_df$fill, nm = base_df$value)
  text_v <- setNames(label_df$text, nm = label_df$value)

  plot_object <-
    main_plot_df %>%
    ggplot(aes(x = Duration, y = Attendees)) +
    geom_polygon(aes(fill = value, group = id),
                 colour = "#FFFFFF") +
    scale_fill_manual(values = colo_v) +
    geom_text(data = label_df, aes(x = x, y = y, label = value2, colour = value), size = 3) +
    scale_colour_manual(values = text_v) +
    scale_x_continuous(breaks = 0:3,
                       labels = c("0", "1", "2+", "")) +
    scale_y_continuous(breaks = 0:5,
                       labels = c("2", "3-8", "9-18",
                                  "19-50", "51+", "")) +
    labs(title = "Meeting types by attendees and duration",
         subtitle = "% of total time spent in meetings",
         ylab = "Attendees",
         xlab = "Duration (hours)",
         caption = paste("Data from week of",
                         myPeriod$Start,
                         "to week of",
                         myPeriod$End)) +
    theme_wpa_basic() +
    theme(legend.position = "none")


    if(return == "table"){

      rbind(myResultsTable,
            myResultsTableTotal)

    } else if(return == "plot"){

      return(plot_object)

    } else {

      stop("Please enter a valid input for `return`.")

    }
}
