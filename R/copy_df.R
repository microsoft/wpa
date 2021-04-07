# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Copy a data frame to clipboard for pasting in Excel
#'
#' @description
#' This is a pipe-optimised function, that feeds into `wpa::export()`,
#' but can be used as a stand-alone function.
#'
#' Based on the original function from
#' <https://github.com/martinctc/surveytoolbox>.
#'
#' @param x Data frame to be passed through. Cannot contain list-columns or
#'   nested data frames.
#' @param row.names A logical vector for specifying whether to allow row names.
#'   Defaults to `FALSE`.
#' @param col.names A logical vector for specifying whether to allow column
#'   names. Defaults to `FALSE`.
#' @param quietly Set this to TRUE to not print data frame on console
#' @param ... Additional arguments for write.table().
#'
#' @importFrom utils write.table
#'
#' @family Import and Export
#'
#' @return
#' Copies a data frame to the clipboard with no return value.
#'
#' @export

copy_df <-function(x,
                   row.names = FALSE,
                   col.names = TRUE,
                   quietly = FALSE,...) {


    utils::write.table(x,"clipboard-50000",
                sep="\t",
                row.names=row.names,
                col.names=col.names,...)

  if(quietly==FALSE) print(x)

}
