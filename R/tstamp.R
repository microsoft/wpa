# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Generate a time stamp
#'
#' @description
#' This function generates a time stamp of the format `'yymmdd_hhmmss'`.
#' This is a support function and is not intended for direct use.
#'
#' @family Support
#'
#' @return
#' String containing the timestamp in the format `'yymmdd_hhmmss'`.
#'
#' @export
tstamp <- function(){
  stamp <- Sys.time()
  stamp <- gsub(pattern = "[[:punct:]]", replacement = "", x = stamp)
  stamp <- gsub(pattern = " ", replacement = "_", x = stamp)

  return(stamp)
}
