#' @title Wellbeing Report
#'
#' @description Generate a wellbeing report
#'
#' @export
wellbeing_report <- function(wbq,
                             hcq,
                             hrvar = "Organization",
                             mingroup = 5,
                             output_file = "wellbeing_report.html"
                             ){
  generate_report2(
    wbq = wbq,
    hcq = hcq,
    hrvar = hrvar,
    mingroup = mingroup,
    report_title = "Org Insights | Employee Wellbeing Report",
    rmd_dir = system.file("rmd_template/wellbeing/wellbeing_report.rmd", package = "wpa"),
    output_format =
      flexdashboard::flex_dashboard(orientation = "columns",
                                    vertical_layout = "fill")
  )
}
