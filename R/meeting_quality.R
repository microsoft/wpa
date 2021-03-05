# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Run a meeting habits / meeting quality analysis
#'
#' @description Return an analysis of Meeting Quality with a bubble plot, using
#'   a Standard Person Query as an input.
#'
#' @inheritParams create_bubble
#'
#' @param metric_x String specifying which variable to show in the x-axis when
#' returning a plot. Must be one of the following:
#'   - `"Low_quality_meeting_hours"` (default)
#'   - `"After_hours_meeting_hours"`
#'   - `"Conflicting_meeting_hours"`
#'   - `"Multitasking_meeting_hours"`
#'   - Any _meeting hour_ variable that can be divided by `Meeting_hours`
#'
#' @inherit create_bubble return
#'
#' @import dplyr
#'
#' @family Visualization
#' @family Meetings
#'
#' @examples
#' # Return plot
#' meeting_quality(sq_data, return = "plot")
#'
#' # Return plot - showing multi-tasking %
#' meeting_quality(sq_data,
#'                 metric_x = "Multitasking_meeting_hours",
#'                 return = "plot")
#'
#' # Return summary table
#' meeting_quality(sq_data, return = "table")
#'
#' @export

meeting_quality <- function(data,
                            hrvar = "Organization",
                            metric_x = "Low_quality_meeting_hours",
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
        "Meeting_hours",
        metric_x) %>%
      unique()


    data %>%
      rename(group = !!sym(hrvar)) %>% # Rename hrvar to `group`
      group_by(PersonId, group) %>%
      summarise_at(vars(meeting_chr), ~mean(., na.rm = TRUE)) %>%
      group_by(group) %>%
      summarise_at(vars(meeting_chr), ~mean(., na.rm = TRUE)) %>%
      left_join(hrvar_count(data, hrvar, return = "table"), by = c("group" = hrvar)) %>%
      filter(n >= mingroup)

  } else {

    percent_str <- NULL
    percent_str <- paste0("Percentage_of_", metric_x)

    data %>%
      mutate(!!sym(percent_str) := !!sym(metric_x) / Meeting_hours) %>%
      create_bubble(hrvar = hrvar,
                    mingroup = mingroup,
                    metric_x = percent_str,
                    metric_y = "Meeting_hours",
                    return = return)
  }
}
