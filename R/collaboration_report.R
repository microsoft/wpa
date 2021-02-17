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
#' @param hrvar HR Variable by which to split metrics. Defaults to AUTO - in this case the HR variable with most collaboration variance is automatically selected.
#' Also accepts any character vector, e.g. "LevelDesignation"
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
                                 hrvar = "AUTO",
                                 mingroup = 5,
                                 path = "collaboration report",
                                 timestamp = TRUE){

  ## Create timestamped path (if applicable)
  if(timestamp == TRUE){
    newpath <- paste(path, wpa::tstamp())
  } else {
    newpath <- path
  }

  if(hrvar == "AUTO"){
  myrank <- data %>% collaboration_rank(mingroup = mingroup, return = "table")
  hrvar <- myrank[[1,1]]
  }
  
  # Set outputs
  output_list <-
    list(data %>% check_query(return = "text") %>% md2html(),
		 paste("---"),

         md2html(text = read_preamble("collaboration_section.md")), # Collaboration Header 
         data %>% collaboration_rank(mingroup = mingroup, return = "plot"),
         data %>% collaboration_rank(mingroup = mingroup, return = "table"),
		 data %>% keymetrics_scan(hrvar = hrvar, mingroup = mingroup,
                                  metrics = c("Collaboration_hours",
                                              "Meetings",
                                              "Meeting_hours",
                                              "Low_quality_meeting_hours",
                                              "Time_in_self_organized_meetings",
                                              "Emails_sent",
                                              "Email_hours",
                                              "Generated_workload_email_hours",
                                              "Total_emails_sent_during_meeting",
                                              "Total_focus_hours"),
                                  textsize = 3,
                                  return = "plot"),

         data %>% collaboration_sum(hrvar = hrvar, mingroup = mingroup, return = "plot"),
         data %>% collaboration_sum(hrvar = hrvar, mingroup = mingroup, return = "table"),
         data %>% collab_area(hrvar = hrvar, mingroup = mingroup, return = "plot"),
		 paste("---"),

         md2html(text = read_preamble("meeting_section.md")), # Meeting Header 
         data %>% meeting_rank(mingroup = mingroup, return = "plot"),
         data %>% meeting_rank(mingroup = mingroup, return = "table"),
         data %>% meeting_dist(hrvar = hrvar, mingroup = mingroup, return = "plot"),
         data %>% meeting_dist(hrvar = hrvar, mingroup = mingroup, return = "table"),
         data %>% mutate(Percentage_of_self_organized_meetings = replace_na(Time_in_self_organized_meetings / Meeting_hours,0))  %>%  create_bar(metric = "Percentage_of_self_organized_meetings", hrvar = hrvar, mingroup = mingroup, return = "plot"),
         data %>% meeting_quality(hrvar = hrvar, mingroup = mingroup, return = "plot"),
         data %>% meeting_trend(hrvar = hrvar, mingroup = mingroup, return = "plot"),
		 paste("---"),

         md2html(text = read_preamble("email_section.md")), # Email Header
         data %>% email_rank(mingroup = mingroup, return = "plot"),
         data %>% email_rank(mingroup = mingroup, return = "table"),
         data %>% email_dist(hrvar = hrvar, mingroup = mingroup, return = "plot"),
         data %>% email_dist(hrvar = hrvar, mingroup = mingroup, return = "table"),
         data %>% email_trend(hrvar = hrvar, mingroup = mingroup, return = "plot"),
		 paste("---"),
		 paste(">", "[Note] This report was generated on ", format(Sys.time(), "%b %d %Y"), ". Data is split by ", hrvar ,".")) %>%
    purrr::map_if(is.data.frame, create_dt) %>%
    purrr::map_if(is.character, md2html)
 
  # Set header titles
  title_list <-
    c("Data Overview",
	  "",
	  "Collaboration Time", # Section header
	  "",
	  "",
      "",
      "",
      "",
      "",
	  "",
      "Deep Dive: Meeting Hours", # Section header
      "",
      "",
      "",
      "",
	  "",
      "",
      "",
	  "",
      "Deep Dive: Email Hours", # Section header
      "",
      "",
      "",
      "",
      "",
	  "",
	  "")

  # Set header levels
  n_title <- length(title_list)
  levels_list <- rep(4, n_title)
  levels_list[c(1, 3, 11, 20)] <- 2 # Section header

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
