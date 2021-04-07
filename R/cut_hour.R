# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Convert a numeric variable for hours into categorical
#'
#' @description
#' Supply a numeric variable, e.g. `Collaboration_hours`, and return a character
#' vector.
#'
#' @details
#' This is used within `create_dist()` for numeric to categorical conversion.
#'
#' @param metric A numeric variable representing hours.
#' @param cuts A numeric vector of minimum length 3 to represent the
#' cut points required. The minimum and maximum values provided in the vector
#' are inclusive.
#' @param unit String to specify the unit of the labels. Defaults to "hours".
#'
#' @family Support
#'
#' @return
#' Character vector representing a converted categorical variable, appended
#' with the label of the unit. See `examples` for more information.
#'
#' @examples
#' # Direct use
#' cut_hour(1:30, cuts = c(15, 20, 25))
#'
#' # Use on a query
#' cut_hour(sq_data$Collaboration_hours, cuts = c(10, 15, 20))
#'
#' @export

cut_hour <- function(metric, cuts, unit = "hours"){


  cuts <- unique(cuts) # No duplicates allowed
  ncuts <- length(cuts)

  if(ncuts < 3){
    stop("Please provide a numeric vector of at least length 3 to `cuts`")
  }


  # Extract min, max, and middle values
  mincut <- min(cuts, na.rm = TRUE)
  maxcut <- max(cuts, na.rm = TRUE)
  midcut <- cuts[!cuts %in% mincut] # Excludes mincut only
  midcut_min_1 <- cuts[match(midcut, cuts) - 1] # one value smaller

  # Individual labels
  label_mincut <- paste0("< ", mincut, " ", unit)
  label_maxcut <- paste0(maxcut, "+ ", unit)
  label_midcut <- paste0(midcut_min_1, " - ", midcut, " ", unit)

  # All labels
  all_labels <- c(label_mincut, label_midcut, label_maxcut)


  cut(metric,
      breaks = c(0, cuts, 100),
      include.lowest = TRUE,
      labels = all_labels)
}


