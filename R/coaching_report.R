# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Generate a Coaching report in HTML
#'
#' @description
#' The function generates an interactive HTML report using Standard Person Query
#' data as an input. The report contains a series of summary analysis and
#' visualisations relating to key **coaching** metrics in Viva Insights,
#' specifically relating to the time spent between managers and their direct
#' reports.
#'
#' @template spq-params
#' @param path Pass the file path and the desired file name, _excluding the file
#'   extension_. For example, `"coaching report"`.
#' @param timestamp Logical vector specifying whether to include a timestamp in
#'   the file name. Defaults to `TRUE`.
#'
#' @importFrom purrr map_if
#' @importFrom dplyr `%>%`
#'
#' @family Reports
#'
#' @inherit generate_report return
#'
#' @export
coaching_report <- function(data,
                            hrvar = "LevelDesignation",
                            mingroup = 5,
                            path = "coaching report",
                            timestamp = TRUE){

  ## Create timestamped path (if applicable)
  if(timestamp == TRUE){
    newpath <- paste(path, wpa::tstamp())
  } else {
    newpath <- path
  }

  output_list <-
    list(data %>% check_query(return = "text", validation = TRUE),

         data %>% mgrrel_matrix(hrvar = hrvar, return = "plot"), # no mingroup arg
         data %>% mgrrel_matrix(hrvar = hrvar, return = "table"), # no mingroup arg

         data %>% one2one_sum(hrvar = hrvar, mingroup = mingroup, return = "plot"),
         data %>% one2one_sum(hrvar = hrvar, mingroup = mingroup, return = "table"),
         data %>% one2one_dist(hrvar = hrvar, mingroup = mingroup, return = "plot"),
         data %>% one2one_dist(hrvar = hrvar, mingroup = mingroup, return = "table"),
         data %>% one2one_trend(hrvar = hrvar, mingroup = mingroup, return = "plot"),
         data %>% one2one_trend(hrvar = hrvar, mingroup = mingroup, return = "table")) %>%
    purrr::map_if(is.data.frame, create_dt) %>%
    purrr::map_if(is.character, md2html)

  title_list <-
    c("Data Overview",

      "Manager Relation Style - Plot",
      "Manager Relation Style  - Table",

      "1-to-1 Summary - Plot",
      "1-to-1 Summary - Table",
      "1-to-1 Distribution - Plot",
      "1-to-1 Distribution - Table",
      "1-to-1 Trend - Plot",
      "1-to-1 Trend - Table")

  n_title <- length(title_list)

  generate_report(title = "Coaching Report",
                  filename = newpath,
                  outputs = output_list,
                  titles = title_list,
                  subheaders = rep("", n_title),
                  echos = rep(FALSE, n_title),
                  levels = rep(3, n_title),
                  theme = "cosmo",
                  preamble = read_preamble("coaching_report.md"))

}




