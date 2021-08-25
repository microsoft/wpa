#' @title Generate a Wellbeing Report in HTML
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' This function takes a custom Wellbeing Query and an Hourly Collaboration
#' query, generating a HTML report on wellbeing.
#'
#' @param wbq Data frame. A custom Wellbeing Query dataset based on the Person
#'   Query. If certain metrics are missing from the Wellbeing / Person Query,
#'   the relevant visual will show up with an indicative message.
#' @param hcq Data frame. An Hourly Collaboration Query dataset.
#' @param hrvar String specifying HR attribute to cut by archetypes. Defaults to
#'   `Organization`.
#' @param mingroup Numeric value setting the privacy threshold / minimum group
#'   size. Defaults to 5.
#' @param start_hour A character vector specifying starting hours, e.g.
#'   `"0900"`. Note that this currently only supports **hourly** increments. If
#'   the official hours specifying checking in and 9 AM and checking out at 5
#'   PM, then `"0900"` should be supplied here.
#' @param end_hour A character vector specifying starting hours, e.g. `"1700"`.
#'   Note that this currently only supports **hourly** increments. If the
#'   official hours specifying checking in and 9 AM and checking out at 5 PM,
#'   then `"1700"` should be supplied here.
#' @param path Pass the file path and the desired file name, _excluding the file
#'   extension_. Defaults to `"wellbeing_report"`.
#'
#' @export
wellbeing_report <- function(wbq,
                             hcq,
                             hrvar = "Organization",
                             mingroup = 5,
                             start_hour = "0900",
                             end_hour = "1700",
                             path = "wellbeing_report"
                             ){

  ## Check if dependencies are installed
  check_pkg_installed(pkgname = "flexdashboard")


  ## Generate report from RMarkdown
  generate_report2(
    wbq = wbq,
    hcq = hcq,
    hrvar = hrvar,
    mingroup = mingroup,
    output_file = paste0(path, ".html"),
    report_title = "Org Insights | Employee Wellbeing Report",
    rmd_dir = system.file("rmd_template/wellbeing/wellbeing_report.rmd", package = "wpa"),
    output_format =
      flexdashboard::flex_dashboard(orientation = "columns",
                                    vertical_layout = "fill",
                                    css = system.file("rmd_template/wellbeing/custom.css", package = "wpa")),
    # Additional arguments to param
    start_hour = start_hour,
    end_hour = end_hour
  )
}
