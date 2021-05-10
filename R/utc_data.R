# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title UTC offset dataset based on Workplace Analytics time zones
#'
#' @description
#' A dataset containing the UTC offset values based on time zones used in
#' Workplace Analytics.
#'
#' @family Data
#'
#' @return data frame.
#'
#' @format A data frame with 128 rows and 4 variables:
#' \describe{
#'   \item{WpA_TimeZone}{ }
#'   \item{UTC}
#'   \item{UTC_offset}
#'   \item{UTC_offset_raw}
#'
#'   ...
#' }
#' @source \url{https://docs.microsoft.com/en-us/windows-hardware/manufacture/desktop/default-time-zones/}
#' @source \url{https://docs.microsoft.com/en-us/workplace-analytics/use/timezones-for-workplace-analytics}
"utc_data"
