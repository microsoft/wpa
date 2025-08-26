# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Generate a Capacity report in HTML
#'
#' @description
#' The function generates an interactive HTML report using the Standard Person
#' Query data as an input. The report contains a series of summary analysis and
#' visualisations relating to key **capacity** metrics in Viva
#' Insights,including length of week and time in after-hours collaboration.
#'
#' @template spq-params
#' @param path Pass the file path and the desired file name, _excluding the file extension_.
#' For example, `"capacity report"`.
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
capacity_report <- function(data,
                            hrvar = "Organization",
                            mingroup = 5,
                            path = "capacity report",
                            timestamp = TRUE){

  ## Create timestamped path (if applicable)
  if(timestamp == TRUE){
    newpath <- paste(path, wpa::tstamp())
  } else {
    newpath <- path
  }

  # Set outputs
  output_list <-
    list(data %>% check_query(return = "text", validation = TRUE),
         read_preamble("workloads_section.md"), # Header
         data %>% workloads_summary(hrvar = hrvar, mingroup = mingroup, return = "plot"),
         data %>% workloads_summary(hrvar = hrvar, mingroup = mingroup, return = "table"),
         data %>% workloads_dist(hrvar = hrvar, mingroup = mingroup, return = "plot"),
         data %>% workloads_dist(hrvar = hrvar, mingroup = mingroup, return = "table"),
         data %>% workloads_trend(hrvar = hrvar, mingroup = mingroup, return = "plot"),
         data %>% workloads_trend(hrvar = hrvar, mingroup = mingroup, return = "table"),
         data %>% workloads_line(hrvar = hrvar, mingroup = mingroup, return = "plot"),
         data %>% workloads_line(hrvar = hrvar, mingroup = mingroup, return = "table"),

         read_preamble("afterhours_section.md"), # Header
         data %>% afterhours_summary(hrvar = hrvar, mingroup = mingroup, return = "plot"),
         data %>% afterhours_summary(hrvar = hrvar, mingroup = mingroup, return = "table"),
         data %>% afterhours_dist(hrvar = hrvar, mingroup = mingroup, return = "plot"),
         data %>% afterhours_dist(hrvar = hrvar, mingroup = mingroup, return = "table"),
         data %>% afterhours_trend(hrvar = hrvar, mingroup = mingroup, return = "plot"),
         data %>% afterhours_trend(hrvar = hrvar, mingroup = mingroup, return = "table")) %>%
    purrr::map_if(is.data.frame, create_dt) %>%
    purrr::map_if(is.character, md2html)

  # Set header titles
  title_list <-
    c("Data Overview",

      "Workloads",
      "Workloads Summary - Plot",
      "Workloads Summary - Table",
      "Workloads Distribution - Plot",
      "Workloads Distribution - Table",
      "Workloads Trend - Plot",
      "Workloads Trend - Table",
      "Workloads over time - Plot",
      "Workloads over time - Table",

      "After hours",
      "After hours Summary - Plot",
      "After hours Summary - Table",
      "After hours Distribution - Plot",
      "After hours Distribution - Table",
      "After hours Trend - Plot",
      "After hours Trend - Table")

  # Set header levels
  n_title <- length(title_list)
  levels_list <- rep(3, n_title)
  levels_list[c(1, 2, 11)] <- 2 # Section header

  generate_report(title = "Capacity Report",
                  filename = newpath,
                  outputs = output_list,
                  titles = title_list,
                  subheaders = rep("", n_title),
                  echos = rep(FALSE, n_title),
                  levels = rep(3, n_title),
                  theme = "cosmo",
                  preamble = read_preamble("capacity_report.md"))

}




