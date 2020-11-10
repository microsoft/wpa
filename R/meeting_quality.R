# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Run a meeting habits / meeting quality analysis
#'
#' @description Return an analysis of Meeting Quality with a bubble plot, using a Standard Person
#' Query as an input.
#'
#' @param data A Standard Query dataset in the form of a data frame.
#' @param hrvar HR Variable by which to split metrics. Accepts a character vector, e.g. "Organization"
#' @param mingroup Numeric value setting the privacy threshold / minimum group size. Defaults to 5.
#' @param return Character vector specifying what to return, defaults to "plot".
#' Valid inputs are "plot" or "table".
#'
#' @import dplyr
#' @import ggplot2
#' @import reshape2
#' @family Meeting Culture
#'
#' @return
#' Returns a ggplot object by default, where 'plot' is passed in `return`.
#' When 'table' is passed, a summary table is returned as a data frame.
#'
#' @export

meeting_quality <- function(data,
                            hrvar = "Organization",
                            mingroup = 5,
                            return = "plot"){

  ## Date range data frame
  myPeriod <- extract_date_range(data)

  ## Prepare Table
  data %>%
    rename(group = !!sym(hrvar)) %>% # Rename hrvar to `group`
    group_by(PersonId, group) %>%
    summarize(Meeting_count = mean(Meetings),
              Meeting_hours = mean(Meeting_hours),
              Low_quality_meeting_hours = mean(Low_quality_meeting_hours),
              perc_after_hours_m = 100*mean(After_hours_meeting_hours)/mean(Meeting_hours),
              perc_low_quality = 100*mean(Low_quality_meeting_hours)/mean(Meeting_hours),
              perc_Multitasking = 100*mean(Multitasking_meeting_hours)/mean(Meeting_hours),
              perc_Redundant = 100*mean(Redundant_meeting_hours__organizational_)/mean(Meeting_hours),
              perc_conflicting  = 100*mean(Conflicting_meeting_hours)/mean(Meeting_hours)) %>%
    group_by(group) %>%
    summarise_at(vars(Meeting_count,
                      Meeting_hours,
                      Low_quality_meeting_hours,
                      perc_after_hours_m,
                      perc_low_quality,
                      perc_Multitasking,
                      perc_Redundant,
                      perc_conflicting),
                 ~mean(.)) %>%
    left_join(data %>%
                rename(group = !!sym(hrvar)) %>%
                group_by(group) %>%
                summarise(Employee_Count = n_distinct(PersonId)),
              by = "group") %>%
    filter(Employee_Count >= mingroup) -> myTable

 myTable_wide <- myTable %>%
    reshape2::melt(id.vars = "group") %>%
    reshape2::dcast(variable ~ group)

  myTable_long  <-  reshape2::melt(myTable, id.vars=c("group"))
  myTable_plot <- myTable  %>% select(group, perc_low_quality, Meeting_hours)

## Bar plot
  plot_object <-
    myTable_plot %>%
    ggplot(aes(x = perc_low_quality, y = Meeting_hours, size=2)) +
    geom_point(stat = "identity",
             fill = "#203864", alpha=0.1) +
    geom_text(aes(label = group),
              hjust = 0.2,
              color = "black",
              fontface = "bold",
              size = 4)+
    ylim(min(myTable_plot$Meeting_hours),max(myTable_plot$Meeting_hours) + 2) +
    xlim(min(myTable_plot$perc_low_quality),max(myTable_plot$perc_low_quality) + 2) +
    theme_wpa_basic() +
    theme(axis.text=element_text(size=12),
          plot.title = element_text(color="grey40", face="bold", size=18),
          plot.subtitle = element_text(size=14),
          legend.position = "none",
          legend.justification = "right",
          legend.title=element_text(size=14),
          legend.text=element_text(size=14)) +
    labs(title = "Meeting Quality",
         subtitle = paste("Meeting time and low-quality percentage by", tolower(hrvar))) +
    ylab("Average weekly meeting hours") +
    xlab("Average weekly percentage of low-quality meeting hours") +
    labs(caption = paste("Data from week of", myPeriod$Start, "to week of", myPeriod$End))

  if(return == "table"){

    myTable %>%
      as_tibble() %>%
      return()

  } else if(return == "plot"){

    return(plot_object)

  } else {

    stop("Please enter a valid input for `return`.")

  }
}
