#' @title Meeting Type Distribution (Ways of Working Assessment Query)
#'
#' @description
#' Calculate the hour distribution of internal meeting types,
#' using a Ways of Working Assessment Query with core WpA variables as an input.
#'
#' @param data Meeting Query data frame. Must contain the variables `Attendee` and `DurationHours`
#' @param hrvar Character string to specify the HR attribute to split the data by.
#' @param mingroup Numeric value setting the privacy threshold / minimum group size. Defaults to 5.
#' @param return String specifying what to return. This must be one of the
#'   following strings:
#'   - `"plot"`
#'   - `"table"`
#'
#' See `Value` for more information.
#'
#' @import ggplot2
#' @import dplyr
#' @importFrom stats setNames
#'
#' @inherit meetingtype_dist return
#'
#' @family Visualization
#' @family Meetings
#'
#' @export

meetingtype_dist_ca <- function(data,
                                hrvar = NULL,
                                mingroup = 5,
                                return = "plot"){

  mt_dist_str <- c("Bloated_meeting_hours",
                   "Lengthy_meeting_hours",
                   "Workshop_meeting_hours",
                   "All_hands_meeting_hours",
                   "Status_update_meeting_hours",
                   "Decision_making_meeting_hours",
                   "One_on_one_meeting_hours")

  mt_dist_str_clean <-
    mt_dist_str %>%
    gsub(pattern = "_meeting_hours", replacement = "", x = .) %>%
    us_to_space()

  ## Add dummy "Total" column if hrvar = NULL
  if(is.null(hrvar)){

    data <- mutate(data, Total = "Total")
    hrvar <- "Total"

  }

  ## No org splits
  if(hrvar == "Total"){

    myResultsTable <-
      data %>%
      summarise_at(vars(mt_dist_str), ~sum(., na.rm = TRUE)) %>%
      gather(MeetingType, AttendeeMeetingHours) %>%
      mutate(Prop = AttendeeMeetingHours / sum(AttendeeMeetingHours),
             Percent = paste(round(Prop * 100), "%")) %>%
      mutate(MeetingType = gsub(pattern = "_meeting_hours", replacement = "", x = MeetingType)) %>%
      mutate(MeetingType = us_to_space(MeetingType))

    ## Only for creating the bottom row data
    myResultsTableTotal <-
      data %>%
      summarise_at(vars(mt_dist_str), ~sum(., na.rm = TRUE)) %>%
      gather(MeetingType, AttendeeMeetingHours) %>%
      mutate(MeetingType = "Total") %>%
      group_by(MeetingType) %>%
      summarise(AttendeeMeetingHours = sum(AttendeeMeetingHours, na.rm = TRUE)) %>%
      mutate(Prop = AttendeeMeetingHours / sum(AttendeeMeetingHours),
             Percent = paste(round(Prop * 100), "%"))

    outputTable <-
      rbind(myResultsTable,
            myResultsTableTotal)


  } else {

    ## Group by hrvar
    myResultsTable <-
      data %>%
      group_by(!!sym(hrvar)) %>%
      summarise_at(vars(mt_dist_str), ~sum(., na.rm = TRUE)) %>%
      left_join(data %>% hrvar_count(hrvar = hrvar, return = "table"),
                by = hrvar) %>%
      filter(n >= mingroup) %>%
      gather(MeetingType, AttendeeMeetingHours, -!!sym(hrvar), -n) %>%
      group_by(!!sym(hrvar)) %>%
      mutate(Prop = AttendeeMeetingHours / sum(AttendeeMeetingHours),
             Percent = paste(round(Prop * 100), "%")) %>%
      mutate(MeetingType = gsub(pattern = "_meeting_hours", replacement = "", x = MeetingType)) %>%
      mutate(MeetingType = us_to_space(MeetingType))

    outputTable <-
      myResultsTable %>%
      ungroup() %>%
      select(!!sym(hrvar), MeetingType, Prop) %>%
      spread(MeetingType, Prop)

  }



  if(hrvar == "Total"){

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
           caption = extract_date_range(data, return = "text")) +
      theme_wpa_basic() +
      theme(legend.position = "none")

  } else {

    plot_object <-
    myResultsTable %>%
      mutate(MeetingType = factor(MeetingType, levels = rev(mt_dist_str_clean))) %>%
      mutate(Fill = case_when(MeetingType == "Bloated" ~ rgb2hex(180,180,180),
                              MeetingType == "Lengthy" ~ rgb2hex(120,120,120),
                              MeetingType == "Workshop" ~ rgb2hex(90,90,90),
                              MeetingType == "All hands" ~ rgb2hex(60,60,60),
                              MeetingType == "Status update" ~ rgb2hex(45,160,160),
                              MeetingType == "Decision making" ~ rgb2hex(25,120,140),
                              MeetingType == "One on one" ~ rgb2hex(5,85,100))) %>%
      ggplot(aes(x = !!sym(hrvar), y = Prop, group = MeetingType, fill = Fill)) +
      geom_bar(position = "stack", stat = "identity") +
      geom_text(aes(label = paste(round(Prop * 100), "%")),
                position = position_stack(vjust = 0.5),
                color = "#FFFFFF",
                fontface = "bold") +
      scale_fill_identity(name = "Coaching styles",
                          breaks = c(
                            rgb2hex(180,180,180),
                            rgb2hex(120,120,120),
                            rgb2hex(90,90,90),
                            rgb2hex(60,60,60),
                            rgb2hex(45,160,160),
                            rgb2hex(25,120,140),
                            rgb2hex(5,85,100)
                            ),
                          labels = mt_dist_str_clean,
                          guide = "legend") +
    coord_flip() +
    theme_wpa_basic() +
    labs(title = "Meeting types by attendees and duration",
         subtitle = "% of total time spent in meetings",
         y = "Percentage",
         caption = extract_date_range(data, return = "text"))


  }

  if(return == "table"){

    outputTable

    } else if(return == "plot"){

      return(plot_object)

    } else {

      stop("Please enter a valid input for `return`.")

    }
}
