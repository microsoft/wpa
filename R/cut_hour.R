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
#' @param lbound Numeric. Specifies the lower bound (inclusive) value for the
#'   minimum label. Defaults to 0.
#' @param ubound Numeric. Specifies the upper bound (inclusive) value for the
#'   maximum label. Defaults to 100.
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

cut_hour <- function(metric,
                     cuts,
                     unit = "hours",
                     lbound = 0,
                     ubound = 100){


  cuts <- unique(cuts) # No duplicates allowed
  ncuts <- length(cuts)

  if(ncuts < 2){
    stop("Please provide a numeric vector of at least length 2 to `cuts`")
  }

  # Extract min, max, and middle values
  mincut <- min(cuts, na.rm = TRUE)
  maxcut <- max(cuts, na.rm = TRUE)

  midcut <- cuts[!cuts %in% mincut] # Excludes mincut only
  midcut_min_1 <- cuts[match(midcut, cuts) - 1] # one value smaller
  mincut_2 <- midcut_min_1[[1]] # second smallest cut

  # Min and max values of `metric`
  minval <- min(metric, na.rm = TRUE)
  maxval <- max(metric, na.rm = TRUE)

  # Warn if smaller lbound or larger ubound
  if(minval < lbound){
    warning("`lbound` does not capture the smallest value in `metric`. ",
            "Values smaller than `lbound` will be classified as NA. ",
            "Adjusting `lbound` is recommended.")
  }

  if(maxval > ubound){
    warning("`ubound` does not capture the largest value in `metric`. ",
            "Values larger than `ubound` will be classified as NA. ",
            "Adjusting `ubound` is recommended.")
  }

  # Take smallest or largest of both values
  lbound <- min(c(mincut, lbound), na.rm = TRUE)
  ubound <- max(c(maxcut, ubound), na.rm = TRUE)

  # Individual labels
  label_mincut <- paste0("< ", mincut, " ", unit)
  label_maxcut <- paste0(maxcut, "+ ", unit)
  label_midcut <- paste0(midcut_min_1, " - ", midcut, " ", unit)

  # All labels
  all_labels <- unique(c(label_mincut, label_midcut, label_maxcut))

  # If `lbound` or `ubound` conflict with cuts
  if(lbound == mincut){

    all_labels <- all_labels[all_labels != label_mincut]

  }

  if(ubound == maxcut){

    all_labels <- all_labels[all_labels != label_maxcut]

  }

  # Debugging chunk ---------------------------------------------------------
  # list(
  #   breaks = unique(c(lbound, cuts, ubound)),
  #   lbound,
  #   ubound,
  #   all_labels
  # )

  # Return result
  cut(metric,
      breaks = unique(c(lbound, cuts, ubound)),
      include.lowest = TRUE,
      labels = all_labels)
}


