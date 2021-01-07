# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Run a meeting habits / meeting quality analysis
#'
#' @description Return an analysis of Meeting Quality with a bubble plot, using a Standard Person
#' Query as an input.
#'
#' @inheritParams create_bubble
#'
#' @import dplyr
#'
#' @examples
#' meeting_quality(sq_data, return = "plot")
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

  ## Wrapper around summary table
  if(return == "table"){

    meeting_chr <-
      c("After_hours_meeting_hours",
        "Low_quality_meeting_hours",
        "Conflicting_meeting_hours",
        "Multitasking_meeting_hours",
        "Meetings",
        "Meeting_hours")


    data %>%
      rename(group = !!sym(hrvar)) %>% # Rename hrvar to `group`
      group_by(PersonId, group) %>%
      summarise_at(vars(meeting_chr), ~mean(., na.rm = TRUE)) %>%
      group_by(group) %>%
      summarise_at(vars(meeting_chr), ~mean(., na.rm = TRUE)) %>%
      left_join(hrvar_count(data, hrvar, return = "table"), by = c("group" = hrvar)) %>%
      filter(n >= mingroup)

  } else {

    data %>%
      mutate(Percentage_of_Low_quality_meeting_hours = Low_quality_meeting_hours / Meeting_hours) %>%
      create_bubble(hrvar = hrvar,
                    mingroup = mingroup,
                    metric_x = "Percentage_of_Low_quality_meeting_hours",
                    metric_y = "Meeting_hours",
                    return = return)
  }
}
