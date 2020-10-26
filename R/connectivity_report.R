#' @title Generate a Connectivity report in HTML
#'
#' @description
#' The function generates an interactive HTML report using
#' Standard Query data as an input. The report contains a series
#' of summary analysis and visualisations relating to key **connectivity**
#' metrics in Workplace Analytics, including external/internal network size
#' vs breadth.
#'
#' @param data A Standard Query dataset in the form of a data frame.
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
    list(data %>% check_query(return = "text") %>% md2html(),

         data %>% external_network_plot(hrvar = hrvar, mingroup = mingroup, return = "plot"),
         data %>% external_network_plot(hrvar = hrvar, mingroup = mingroup, return = "table"),

         data %>% internal_network_plot(hrvar = hrvar, mingroup = mingroup, return = "plot"),
         data %>% internal_network_plot(hrvar = hrvar, mingroup = mingroup, return = "table")) %>%
    purrr::map_if(is.data.frame, create_dt)

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




