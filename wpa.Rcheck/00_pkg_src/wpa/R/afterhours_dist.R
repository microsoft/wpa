# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Distribution of After-hours Collaboration Hours as a 100% stacked bar
#' @name afterhours_dist
#'
#' @description Analyse the distribution of weekly after-hours collaboration time.
#' Returns a stacked bar plot by default.
#' Additional options available to return a table with distribution elements.
#'
#' @details
#' Uses the metric \code{After_hours_collaboration_hours}.
#' See `create_dist()` for applying the same analysis to a different metric.
#'
#' @inheritParams create_dist
#' @inherit create_dist return
#'
#' @param cut A vector specifying the cuts to use for the data,
#' accepting "default" or "range-cut" as character vector,
#' or a numeric value of length three to specify the exact breaks to use. e.g. c(1, 3, 5)
#'
#' @import dplyr
#' @import ggplot2
#' @import reshape2
#' @import scales
#' @importFrom tidyr spread
#' @importFrom stats median
#' @importFrom stats sd
#'
#' @family Visualization
#' @family After-hours Collaboration
#'
#' @examples
#' # Return plot
#' afterhours_dist(sq_data, hrvar = "Organization")
#'
#' # Return summary table
#' afterhours_dist(sq_data, hrvar = "Organization", return = "table")
#'
#' # Return result with a custom specified breaks
#' afterhours_dist(sq_data, hrvar = "LevelDesignation", cut = c(4, 7, 9))
#' @export
afterhours_dist <- function(data,
                            hrvar = "Organization",
                            mingroup = 5,
                            return = "plot",
                            cut = c(1, 2, 3)) {

  create_dist(data = data,
              metric = "After_hours_collaboration_hours",
              hrvar = hrvar,
              mingroup = mingroup,
              return = return,
              cut = cut)


}
