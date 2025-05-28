# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Sample Group-to-Group dataset
#'
#' @description
#' A demo dataset representing a Group-to-Group Query. The grouping
#' organizational attribute used here is `Organization`, where the variable have
#' been prefixed with `TimeInvestors_` and `Collaborators_` to represent the
#' direction of collaboration.
#'
#' @family Data
#' @family Network
#'
#' @return data frame.
#'
#' @format A data frame with 1417 rows and 7 variables:
#' \describe{
#'   \item{TimeInvestors_Organization}{ }
#'   \item{Collaborators_Organization}{ }
#'   \item{Date}{ }
#'   \item{Meetings}{ }
#'   \item{Meeting_hours}{ }
#'   \item{Email_hours}{ }
#'   \item{Collaboration_hours}{ }
#'
#'   ...
#' }
"g2g_data"
