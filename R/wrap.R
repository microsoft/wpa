# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Add a character at the start and end of a character string
#'
#' @description This function adds a character at the start and end of a character
#' string, where the default behaviour is to add a double quote.
#'
#' @param string Character string to be wrapped around
#' @param wrapper Character to wrap around `string`
#'
#' @family Support
#'
#' @return
#' Character vector containing the modified string.
#'
#' @export
wrap <- function(string, wrapper = '"'){
  paste0(wrapper, string, wrapper)
}
