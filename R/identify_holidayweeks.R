# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Identify Holiday Weeks
#'
#' @description
#' This function scans a standard query output for weeks where collaboration hours is far outside the mean.
#' Returns a list of weeks that appear to be holiday weeks and optionally an edited dataframe with outliers removed.
#' By default, missing values are excluded.
#'
#' As best practice, run this function prior to any analysis to remove atypical collaboration weeks from your dataset.
#'
#' @param data A Standard Person Query dataset in the form of a data frame.
#' @param sd The standard deviation below the mean for collaboration hours that should define an outlier week. Enter a positive number. Default is 1 standard deviation.
#' @param return String to specify what to return
#'
#' @import dplyr
#' @import ggplot2
#' @importFrom methods is
#'
#' @family Data Validation
#'
#' @return
#' Returns a message by default (`return` = "message").
#' When 'message' is passed, a message is printed identifying holiday weeks.
#' When 'data' is passed, a dataset with outlier weeks flagged in a new column is returned as a dataframe.
#' When 'data_cleaned' is passed, a dataset with outlier weeks removed is returned as a dataframe.
#' when 'data_dirty' is passed, a dataset with only outlier weeks is returned as a dataframe.
#' when 'plot' is passed, a pot with holiday weeks highlighted is returned as a dataframe.
#'
#' @export
identify_holidayweeks <- function(data, sd = 1, return = "message"){

  ## Ensure date is formatted
  if(all(is_date_format(data$Date))){
    data$Date <- as.Date(data$Date, format = "%m/%d/%Y")
  } else if(is(data$Date, "Date")){
  # Do nothing
  } else {
    stop("`Date` appears not to be properly formatted.\n
         It needs to be in the format MM/DD/YYYY.\n
         Also check for missing values or stray values with inconsistent formats.")
  }

  Calc <-
    data %>%
    group_by(Date) %>%
    summarize(mean_collab = mean(Collaboration_hours, na.rm = TRUE),.groups = 'drop') %>%
    mutate(z_score = (mean_collab - mean(mean_collab, na.rm = TRUE))/ sd(mean_collab, na.rm = TRUE))

  Outliers = (Calc$Date[Calc$z_score < -sd])

  mean_collab_hrs <- mean(Calc$mean_collab, na.rm = TRUE)

  Message <- paste0("The weeks where collaboration was ",
                   sd,
                   " standard deviations below the mean (",
                   round(mean_collab_hrs, 1),
                   ") are: \n",
                   paste(wrap(Outliers, wrapper = "`"),collapse = ", "))

  myTable_plot <-
    data %>%
    mutate(holidayweek = (Date %in% Outliers)) %>%
    select("Date", "holidayweek", "Collaboration_hours") %>%
    group_by(Date) %>%
    summarise(Collaboration_hours=mean(Collaboration_hours), holidayweek=first(holidayweek)) %>%
    mutate(Date=as.Date(Date, format = "%m/%d/%Y"))

  myTable_plot_shade <-
    myTable_plot %>%
    filter(holidayweek == TRUE) %>%
    mutate(min = Date - 3 , max = Date + 3 , ymin = -Inf, ymax = +Inf)

  plot <-
    myTable_plot %>%
    ggplot(aes(x = Date, y = Collaboration_hours, group = 1)) +
    geom_line(colour = "grey40") +
    theme_wpa_basic() +
    geom_rect(data = myTable_plot_shade,
              aes(xmin = min, xmax = max, ymin = ymin, ymax = ymax),
              color="transparent", fill="steelblue", alpha=0.3) +
    labs(title = "Holiday Weeks",
         subtitle = "Showing average collaboration hours over time")+
    ylab("Collaboration Hours") +
    xlab("Date")

  if(return == "text"){
    return(Message)
  } else if(return == "message"){
    message(Message)
  }else if(return %in% c("data_clean", "data_cleaned")){
    return(data %>% filter(!(Date %in% Outliers)) %>% data.frame())
  } else if(return == "data_dirty"){
    return(data %>% filter((Date %in% Outliers)) %>% data.frame())
  } else if(return == "data"){
    return(data %>% mutate(holidayweek = (Date %in% Outliers)) %>% data.frame())
  } else if(return == "plot"){
    return(plot)
  } else {
    stop("Error: please check inputs for `return`")
  }
}





