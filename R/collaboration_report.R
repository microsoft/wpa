# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Generate a Collaboration Report in HTML
#'
#' @description
#' The function generates an interactive HTML report using
#' Standard Person Query data as an input. The report contains a series
#' of summary analysis and visualisations relating to key **collaboration**
#' metrics in Workplace Analytics,including email and meeting hours.
#'
#' @param data A Standard Person Query dataset in the form of a data frame.
#' @param hrvar HR Variable by which to split metrics, defaults to "Organization"
#'  but accepts any character vector, e.g. "LevelDesignation"
#' @param mingroup Numeric value setting the privacy threshold / minimum group size. Defaults to 5.
#' @param path Pass the file path and the desired file name, _excluding the file extension_.
#' For example, "collaboration report".
#' @param timestamp Logical vector specifying whether to include a timestamp in the file name.
#' Defaults to TRUE.
#'
#' @importFrom purrr map_if
#' @importFrom dplyr `%>%`
#'
#' @family Reports
#'
#' @inherit generate_report return
#'
#' @export
collaboration_report <- function(data,
                                 hrvar = "Organization",
                                 mingroup = 5,
                                 path = "collaboration report",
                                 timestamp = TRUE){

  ## Create timestamped path (if applicable)
  if(timestamp == TRUE){
    newpath <- paste(path, wpa::tstamp())
  } else {
    newpath <- path
  }

  # Set outputs
  output_list <-
    list(data %>% check_query(return = "text") %>% md2html(),
         md2html(text = read_preamble("collaboration_section.md")), # Header
         data %>% collaboration_sum(hrvar = hrvar, mingroup = mingroup, return = "plot"),
         data %>% collaboration_sum(hrvar = hrvar, mingroup = mingroup, return = "table"),

         data %>% afterhours_sum(hrvar = hrvar, mingroup = mingroup, return = "plot"),
         data %>% afterhours_sum(hrvar = hrvar, mingroup = mingroup, return = "table"),

         data %>% collaboration_dist(hrvar = hrvar, mingroup = mingroup, return = "plot"),
         data %>% collaboration_dist(hrvar = hrvar, mingroup = mingroup, return = "table"),
         data %>% collaboration_trend(hrvar = hrvar, mingroup = mingroup, return = "plot"),
         data %>% collaboration_trend(hrvar = hrvar, mingroup = mingroup, return = "table"),
         data %>% collaboration_line(hrvar = hrvar, mingroup = mingroup, return = "plot"),
         data %>% collaboration_line(hrvar = hrvar, mingroup = mingroup, return = "table"),

         md2html(text = read_preamble("email_section.md")), # Header
         data %>% email_summary(hrvar = hrvar, mingroup = mingroup, return = "plot"),
         data %>% email_summary(hrvar = hrvar, mingroup = mingroup, return = "table"),
         data %>% email_dist(hrvar = hrvar, mingroup = mingroup, return = "plot"),
         data %>% email_dist(hrvar = hrvar, mingroup = mingroup, return = "table"),
         data %>% email_trend(hrvar = hrvar, mingroup = mingroup, return = "plot"),
         data %>% email_trend(hrvar = hrvar, mingroup = mingroup, return = "table"),

         md2html(text = read_preamble("meeting_section.md")), # Header
         data %>% meeting_summary(hrvar = hrvar, mingroup = mingroup, return = "plot"),
         data %>% meeting_summary(hrvar = hrvar, mingroup = mingroup, return = "table"),
         data %>% meeting_dist(hrvar = hrvar, mingroup = mingroup, return = "plot"),
         data %>% meeting_dist(hrvar = hrvar, mingroup = mingroup, return = "table"),
         data %>% meeting_trend(hrvar = hrvar, mingroup = mingroup, return = "plot"),
         data %>% meeting_trend(hrvar = hrvar, mingroup = mingroup, return = "table"),
         data %>% meeting_quality(hrvar = hrvar, mingroup = mingroup, return = "plot"),
         data %>% meeting_quality(hrvar = hrvar, mingroup = mingroup, return = "table")) %>%
    purrr::map_if(is.data.frame, create_dt)

  # Set header titles
  title_list <-
    c("Data Overview",

      "Collaboration", # Section header
      "Collaboration Summary - Plot",
      "Collaboration Summary - Table",

      "Afterhours Summary - Plot",
      "Afterhours Summary - Table",

      "Collaboration Distribution - Plot",
      "Collaboration Distribution - Table",
      "Collaboration Trend - Plot",
      "Collaboration Trend - Table",
      "Collaboration over time - Plot",
      "Collaboration over time - Table",

      "Email", # Section header
      "Email Summary - Plot",
      "Email Summary - Table",
      "Email Distribution - Plot",
      "Email Distribution - Table",
      "Email Trend - Plot",
      "Email Trend - Table",

      "Meeting", # Section header
      "Meeting Summary - Plot",
      "Meeting Summary - Table",
      "Meeting Distribution - Plot",
      "Meeting Distribution - Table",
      "Meeting Trend - Plot",
      "Meeting Trend - Table",
      "Meeting Quality - Plot",
      "Meeting Quality - Table")

  # Set header levels
  n_title <- length(title_list)
  levels_list <- rep(3, n_title)
  levels_list[c(1, 2, 13, 20)] <- 2 # Section header

  # Generate report
  generate_report(title = "Collaboration Report",
                  filename = newpath,
                  outputs = output_list,
                  titles = title_list,
                  subheaders = rep("", n_title),
                  echos = rep(FALSE, n_title),
                  levels = levels_list,
                  theme = "cosmo",
                  preamble = read_preamble("collaboration_report.md"))

}
