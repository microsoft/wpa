# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Generate a Connectivity report in HTML
#'
#' @description
#' The function generates an interactive HTML report using Standard Person Query
#' data as an input. The report contains a series of summary analysis and
#' visualisations relating to key **connectivity** metrics, including
#' external/internal network size vs breadth
#' (`Networking_outside_organization`, `Networking_outside_domain`).
#'
#' @template spq-params
#' @param path Pass the file path and the desired file name, _excluding the file
#'   extension_. For example, `"connectivity report"`.
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
connectivity_report <- function(data,
                                hrvar = "LevelDesignation",
                                mingroup = 5,
                                path = "connectivity report",
                                timestamp = TRUE){

  ## Create timestamped path (if applicable)
  if(timestamp == TRUE){
    newpath <- paste(path, wpa::tstamp())
  } else {
    newpath <- path
  }

  output_list <-
    list(data %>% check_query(return = "text", validation = TRUE),

         data %>% external_network_plot(hrvar = hrvar, mingroup = mingroup, return = "plot"),
         data %>% external_network_plot(hrvar = hrvar, mingroup = mingroup, return = "table"),

         data %>% internal_network_plot(hrvar = hrvar, mingroup = mingroup, return = "plot"),
         data %>% internal_network_plot(hrvar = hrvar, mingroup = mingroup, return = "table")) %>%
    purrr::map_if(is.data.frame, create_dt) %>%
    purrr::map_if(is.character, md2html)

  title_list <-
    c("Data Overview",

      "External network - Plot",
      "External network  - Table",

      "Internal network - Plot",
      "Internal network - Table")

  n_title <- length(title_list)

  generate_report(title = "Connectivity Report",
                  filename = newpath,
                  outputs = output_list,
                  titles = title_list,
                  subheaders = rep("", n_title),
                  echos = rep(FALSE, n_title),
                  levels = rep(3, n_title),
                  theme = "cosmo",
                  preamble = read_preamble("connectivity_report.md"))

}




