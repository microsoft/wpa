# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Generate HTML report based on existing RMarkdown documents
#'
#' @description
#' This is a support function that accepts parameters and creates a HTML
#' document based on an RMarkdown template. This is an alternative to
#' `generate_report()` which instead creates an RMarkdown document from scratch
#' using individual code chunks.
#'
#' @note
#' The implementation of this function was inspired by the 'DataExplorer'
#' package by boxuancui, with credits due to the original author.
#'
#' @param output_format output format in `rmarkdown::render()`. Default is
#'  `rmarkdown::html_document(toc = TRUE, toc_depth = 6, theme = "cosmo")`.
#' @param output_file output file name in `rmarkdown::render()`. Default is
#'   `"report.html"`.
#' @param output_dir output directory for report in `rmarkdown::render()`.
#'   Default is user's current directory.
#' @param report_title report title. Default is `"Report"`.
#' @param rmd_dir string specifying the path to the directory containing the
#'   RMarkdown template files.
#' @param \dots other arguments to be passed to `params`. For instance, pass
#'   `hrvar` if the RMarkdown document requires a 'hrvar' parameter.
#' @export

generate_report2 <- function(output_format = rmarkdown::html_document(toc = TRUE, toc_depth = 6, theme = "cosmo"),
                             output_file = "report.html",
                             output_dir = getwd(),
                             report_title = "Report",
                             rmd_dir = system.file("rmd_template/minimal.rmd", package = "wpa"),
                             ...) {

  ## Render report into html
  suppressWarnings(
    rmarkdown::render(
      input = rmd_dir,
      output_format = output_format,
      output_file = output_file,
      output_dir = output_dir,
      intermediates_dir = output_dir,
      params = list(set_title = report_title, ...)
  ))

  ## Open report
  report_path <- path.expand(file.path(output_dir, output_file))
  utils::browseURL(report_path)
}
