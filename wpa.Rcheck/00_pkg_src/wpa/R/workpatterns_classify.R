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
#'   This method classifies each **person-week** into one of the eight
#'   archetypes:
#'   - **0 Low Activity (< 3 hours on)**: fewer than 3 hours of active hours
#'   - **1.1 Standard continuous (expected schedule)**: active hours equal to
#'   _expected hours_, with all activity confined within the expected start and
#'   end time
#'   - **1.2 Standard continuous (shifted schedule)**: active hours equal to
#'   _expected hours_, with activity occurring beyond either the expected start
#'   or end time.
#'   - **2.1 Standard flexible (expected schedule)**: active hours less than or
#'   equal to _expected hours_, with all activity confined within the expected
#'   start and end time
#'   - **2.2 Standard flexible (shifted schedule)**: active hours less than or
#'   equal to _expected hours_, with activity occurring beyond either the
#'   expected start or end time.
#'   - **3 Long flexible workday**: number of active hours exceed _expected
#'   hours_, with breaks occurring throughout
#'   - **4 Long continuous workday**: number of active hours exceed _expected
#'   hours_, with activity happening in a continuous block (no breaks)
#'   - **5 Always on (13h+)**: number of active hours greater than or equal to
#'   13
#'
#'  _Standard_ here denotes the behaviour of not exhibiting total number of
#'  active hours which exceed the expected total number of hours, as supplied by
#'  `exp_hours`. _Continuous_ refers to the behaviour of _not_ taking breaks,
#'  i.e. no inactive hours between the first and last active hours of the day,
#'  where _flexible_ refers to the contrary.
#'
#'  This is the recommended method over `pav` for several reasons:
#'   1. `bw` ignores _volume effects_, where activity volume can still bias the
#'   results towards the 'standard working hours'.
#'   2. It captures the intuition that each individual can have 'light' and
#'   'heavy' weeks with respect to workload.
#'
#' The notion of 'breaks' in the 'binary-week' method is best understood as
#' 'recurring disconnection time'. This denotes an hourly block where there is
#' consistently no activity occurring throughout the week. Note that this
#' applies a stricter criterion compared to the common definition of a break,
#' which is simply a time interval where no active work is being done, and thus
#' the more specific terminology 'recurring disconnection time' is preferred.
#'
#' In the standard plot output, the archetypes have been abbreviated to show the
#' following:
#'   - **Low Activity** - archetype 0
#'   - **Standard** - archetypes 1.1 and 1.2
#'   - **Flexible** - archetypes 2.1 and 2.2
#'   - **Long continuous** - archetype 4
#'   - **Long flexible** - archetype 3
#'   - **Always On** - archetype 5
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
#'   Defaults to `NULL`. This only affects the function when `"table"` is
#'   returned, and is only applicable for `method = "bw"`.
#'
#' @param return String specifying what to return. This must be one of the
#'   following strings:
#'   - `"plot"`
#'   - `"data"`
#'   - `"table"`
#'   - `"plot-area"`
#'   - `"plot-hrvar"` (only for `bw` method)
#'   - `"plot-dist"` (only for `bw` method)
#'
#' See `Value` for more information.
#'
#' @param method String to pass through specifying which method to use for
#'   classification. By default, a binary week-based (`bw`) method is used, with
#'   options to use the the person-average volume-based (`pav`) method.
#'
#' @param values Only valid if using `pav` method. Character vector to specify
#'   whether to return percentages or absolute values in `"data"` and `"plot"`.
#'   Valid values are `"percent"` (default) and `"abs"`.
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
#' @param exp_hours Numeric value representing the number of hours the
#'   population is expected to be active for throughout the workday. By default,
#'   this uses the difference between `end_hour` and `start_hour`. Only
#'   applicable with the 'bw' method.
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
#'   - `"plot"`: ggplot object. With the `bw` method, this returns a grid
#'   showing the distribution of archetypes by 'breaks' and number of active
#'   hours (default). With the `pav` method, this returns a faceted bar plot
#'   which shows the percentage of signals sent in each hour, with each facet
#'   representing an archetype.
#'   - `"data"`: data frame. The raw data with the classified archetypes.
#'   - `"table"`: data frame. A summary table of the archetypes.
#'   - `"plot-area"`: ggplot object. With the `bw` method, this returns an area
#'   plot of the percentages of archetypes shown over time. With the `pav`
#'   method, this returns an area chart which shows the percentage of signals
#'   sent in each hour, with each line representing an archetype.
#'   - `"plot-hrvar"`: ggplot object. A bar plot showing the count of archetypes,
#'   faceted by the supplied HR attribute. This is only available for the `bw`
#'   method.
#'   - `"plot-dist"`: returns a heatmap plot of signal distribution by hour and
#'   archetypes. This is only available for the `bw` method.
#'
#' @examples
#' \donttest{
#' # Returns a plot by default
#' em_data %>% workpatterns_classify(method = "bw")
#'
#' # Return an area plot
#' # With custom expected hours
#' em_data %>%
#'   workpatterns_classify(
#'     method = "bw",
#'     return = "plot-area",
#'     exp_hours = 7
#'       )
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
                                  exp_hours = NULL,
                                  mingroup = 5,
                                  active_threshold = 0,
                                  method = "bw",
                                  return = "plot"){

  # Test the format of `start_hour` and `end_hour` --------------------------

  test_hour <- function(x){

    if(nchar(x) != 4){

      stop("Input to `start_hour` or `end_hour` must be of the form 'hhmm'.")

    } else if(substr(x, start = 3, stop = 4) != "00"){

      stop("Input to `start_hour` or `end_hour` must be of the form 'hhmm'. ",
           "Only whole hour increments are allowed.")
    }
  }

  test_hour(start_hour)
  test_hour(end_hour)

  # Method flow -------------------------------------------------------------

  if(method == "bw"){

    workpatterns_classify_bw(data = data,
                             hrvar = hrvar,
                             signals = signals,
                             start_hour = start_hour,
                             end_hour = end_hour,
                             exp_hours = exp_hours,
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





