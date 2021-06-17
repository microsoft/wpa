# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Classify working pattern personas using a rule based algorithm
#'
#' @description `r lifecycle::badge('experimental')`
#'
#' Apply a rule based algorithm to emails or instant messages sent by hour of
#' day. Uses a binary week-based ('bw') method by default, with options to use
#' the the person-average volume-based ('pav') method.
#'
#' @author Ainize Cidoncha <ainize.cidoncha@@microsoft.com>
#' @author Carlos Morales Torrado <carlos.morales@@microsoft.com>
#' @author Martin Chan <martin.chan@@microsoft.com>
#'
#' @details
#' The working patterns archetypes are a set of segments created based on the
#' aggregated hourly activity of employees. A motivation of creating these
#' archetypes is to capture the diversity in working patterns, where for
#' instance employees may choose to take multiple or extended breaks throughout
#' the day, or choose to start or end earlier/later than their standard working
#' hours. Two methods have been developed to capture the different working
#' patterns.
#'
#' This function is a wrapper around `workpatterns_classify_bw()` and
#' `workpatterns_classify_pav()`, and calls each function depending on what is
#' supplied to the `method` argument. Both methods implement a rule-based
#' classification of either **person-weeks** or **persons** that pull apart
#' different working patterns.
#'
#' See individual sections below for details on the two different
#' implementations.
#'
#' @section Binary Week method:
#'
#'   This method classifies each **person-week** into one of the seven
#'   archetypes:
#'   - **0 < 3 hours on**: fewer than 3 hours of active hours
#'   - **1 Standard with breaks workday**: active for fewer than _expected
#'   hours_, with no activity outside working hours
#'   - **2 Standard continuous workday**: number of active hours equal _expected
#'   hours_, with no activity outside working hours
#'   - **3 Standard flexible workday**: number of active hours are less than or
#'   equal to _expected hours_, with some activity outside working hours
#'   - **4 Long flexible workday**: number of active hours exceed _expected
#'   hours_, with breaks occurring throughout
#'   - **5 Long continuous workday**: number of active hours exceed _expected
#'   hours_, with activity happening in a continuous block (no breaks)
#'   - **6 Always on (13h+)**: number of active hours greater than or equal to
#'   13
#'
#'   This is the recommended method over `pav` for several reasons:
#'   1. `bw` ignores _volume effects_, where activity volume can still bias the
#'   results towards the 'standard working hours'.
#'   2. It captures the intuition that each individual can have 'light' and
#'   'heavy' weeks with respect to workload.
#'
#' In the standard plot output, the archetypes have been abbreviated to show the
#' following:
#'   - **Low Activity** - archetype 0
#'   - **Standard** - archetype 2
#'   - **Flexible** - archetypes 1 and 3
#'   - **Long continuous** - archetype 5
#'   - **Long flexible** - archetype 4
#'   - **Always On** - archetype 6
#'
#' @section Person Average method:
#'
#'   This method classifies each **person** (based on unique `PersonId`) into
#'   one of the six archetypes:
#' - **Absent**: Fewer than 10 signals over the week.
#'
#' - **Extended Hours - Morning:** 15%+ of collaboration before start hours and
#' less than 70% within standard hours, and less than 15% of collaboration after
#' end hours
#'
#' - **Extended Hours - Evening**: Less than 15% of collaboration before start
#' hours and less than 70% within standard hours, and 15%+ of collaboration
#' after end hours
#'
#' - **Overnight workers**: less than 30% of collaboration happens within
#' standard hours
#'
#' - **Standard Hours**: over 70% of collaboration within standard hours
#'
#' - **Always On**: over 15% of collaboration happens before starting hour and
#' end hour (both conditions must satisfy) and less than 70% of collaboration
#' within standard hours
#'
#'
#' @section Flexibility Index: The Working Patterns archetypes as calculated
#'   using the binary-week method shares many similarities with the Flexibility
#'   Index (see `flex_index()`):
#'
#'   - Both are computed directly from the Hourly Collaboration Flexible Query.
#'   - Both apply the same binary conversion of activity on the signals from the
#'   Hourly Collaboration Flexible Query.
#'
#'
#' @param data A data frame containing data from the Hourly Collaboration query.
#'
#' @param hrvar A string specifying the HR attribute to cut the data by.
#'   Defaults to NULL. This only affects the function when "table" is returned,
#'   and is only applicable for method = "bw".
#'
#' @param return String specifying what to return. This must be one of the
#'   following strings:
#'   - `"plot"`
#'   - `"data"`
#'   - `"table"`
#'   - `"plot-area"`
#'   - `"plot-hrvar"` (only for `bw` method)
#'
#' See `Value` for more information.
#'
#' @param method String to pass through specifying which method to use for
#'   classification. By default, a binary week-based (bw) method is used, with
#'   options to use the the person-average volume-based (pav) method.
#'
#' @param values Only valid if using `pav` method. Character vector to specify
#'   whether to return percentages or absolute values in "data" and "plot".
#'   Valid values are "percent" (default) and "abs".
#'
#' @param signals Character vector to specify which collaboration metrics to
#'   use:
#'   - `"email"` (default) for emails only
#'   - `"IM"` for Teams messages only
#'   - `"unscheduled_calls"` for Unscheduled Calls only
#'   - `"meetings"` for Meetings only
#'   - or a combination of signals, such as `c("email", "IM")`
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
#' @param mingroup Numeric value setting the privacy threshold / minimum group
#'   size. Defaults to 5.
#'
#' @param active_threshold A numeric value specifying the minimum number of
#'   signals to be greater than in order to qualify as _active_. Defaults to 0.
#'   Only applicable for the binary-week method.
#'
#' @import dplyr
#' @import tidyselect
#' @import ggplot2
#'
#' @return Character vector to specify what to return. Valid options
#'   include:
#'   - `"plot"`: returns a heatmap plot of signal distribution by hour
#'   and archetypes (default)
#'   - `"data"`: returns the raw data with the classified archetypes
#'   - `"table"`: returns a summary table of the archetypes
#'   - `"plot-area"`: returns an area plot of the percentages of archetypes
#'   shown over time
#'   - `"plot-hrvar"`: returns a bar plot showing the count of archetypes,
#'   faceted by the supplied HR attribute. This is only available for the `bw`
#'   method.
#'
#' @examples
#'
#' # Returns a plot by default
#' em_data %>% workpatterns_classify(method = "bw")
#'
#' # Return an area plot
#' em_data %>% workpatterns_classify(method = "bw", return = "plot-area")
#'
#' \donttest{
#'
#' em_data %>% workpatterns_classify(method = "bw", return = "table")
#'
#' em_data %>% workpatterns_classify(method = "pav")
#'
#' em_data %>% workpatterns_classify(method = "pav", return = "plot-area")
#'
#' }
#'
#' @family Clustering
#' @family Working Patterns
#'
#' @export
workpatterns_classify <- function(data,
                                  hrvar = "Organization",
                                  values = "percent",
                                  signals = c("email", "IM"),
                                  start_hour = "0900",
                                  end_hour = "1700",
                                  mingroup = 5,
                                  active_threshold = 0,
                                  method = "bw",
                                  return = "plot"){

  if(method == "bw"){
    workpatterns_classify_bw(data = data,
                             hrvar = hrvar,
                             signals = signals,
                             start_hour = start_hour,
                             end_hour = end_hour,
                             mingroup = mingroup,
                             active_threshold = active_threshold,
                             return = return)
  } else if(method == "pav"){

    workpatterns_classify_pav(data = data,
                              values = values,
                              signals = signals,
                              start_hour = start_hour,
                              end_hour = end_hour,
                              return = return)
  } else {
    stop("Invalid method: please check input for `method`")
  }
}





