# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Frequency of Manager 1:1 Meetings as bar or 100% stacked bar chart
#'
#' @description `r lifecycle::badge('experimental')`
#'
#' This function calculates the average number of weeks (cadence) between of 1:1
#' meetings between an employee and their manager. Returns a  distribution plot
#' for typical cadence of 1:1 meetings. Additional options available to return a
#' bar plot, tables, or a data frame with a cadence of 1 on 1 meetings metric.
#'
#' @template spq-params
#' @param return String specifying what to return. This must be one of the
#'   following strings:
#'   - `"plot"`
#'   - `"table"`
#' @param mode String specifying what method to use. This must be one of the
#'   following strings:
#'   - `"dist"`
#'   - `"sum"`
#'
#' @inheritParams create_dist
#' @inherit create_dist return
#'
#' @section Distribution view:
#' For this view, there are four categories of cadence:
#'   - Weekly (once per week)
#'   - Twice monthly or more (up to 3 weeks)
#'   - Monthly (3 - 6 weeks)
#'   - Every two months (6 - 10 weeks)
#'   - Quarterly or less (> 10 weeks)
#'
#' In the occasion there are zero 1:1 meetings with managers, this is included
#' into the last category, i.e. 'Quarterly or less'. Note that when `mode` is
#' set to `"sum"`, these rows are simply excluded from the calculation.
#'
#'
#' @family Visualization
#' @family Managerial Relations
#'
#' @examples
#' # Return plot, mode dist
#' one2one_freq(sq_data,
#'              hrvar = "Organization",
#'              return = "plot",
#'              mode = "dist")
#'
#' # Return plot, mode sum
#' one2one_freq(sq_data,
#'              hrvar = "Organization",
#'              return = "plot",
#'              mode = "sum")
#'
#' # Return summary table
#' one2one_freq(sq_data, hrvar = "Organization", return = "table")
#'
#' @export

one2one_freq <- function(data,
                         hrvar = "Organization",
                         mingroup = 5,
                         return = "plot",
                         mode = "dist",
                         sort_by = "Quarterly or less\n(>10 weeks)") {

  ## Handling NULL values passed to hrvar
  if(is.null(hrvar)){
    data <- totals_col(data)
    hrvar <- "Total"
  }

  expanded_data <-
    data %>%
    mutate(
      Meetings_with_manager_1_on_1 = Meetings_with_manager_1_on_1 > 0
      ) %>%
    group_by(PersonId)  %>%
    mutate(
      Cadence_of_1_on_1_meetings_with_manager =
        1 / (sum(Meetings_with_manager_1_on_1) / n())
      )


  # Arbitrarily large constant -----------------------------------------------

  arb_const <- 99999

  # `breaks_to_labels()` -----------------------------------------------------

  breaks_to_labels <- function(x){

    lookup <-
      c(
        "Weekly\n(once per week)" = "< 1 Weeks",
        "Twice monthly or more\n(up to 3 weeks)" = "1 - 3 Weeks",
        "Monthly\n(3 - 6 weeks)" = "3 - 6 Weeks",
        "Every two months\n(6 - 10 weeks)" = "6 - 10 Weeks",
        "Quarterly or less\n(>10 weeks)" = "10+ Weeks"
      )

    ifelse(
      is.na(names(lookup[match(x, lookup)])),
      x,
      names(lookup[match(x, lookup)])
    )
  }

  # Return outputs -----------------------------------------------------------

  if(return == "data"){

   expanded_data

  }

  else if(mode == "sum" & return == "plot"){

    expanded_data %>%
      filter_all(all_vars(!is.infinite(.))) %>%
      create_bar(
        metric = "Cadence_of_1_on_1_meetings_with_manager",
        hrvar = hrvar,
        return = "table",
        mingroup = mingroup) %>%
      ggplot(aes(x = Cadence_of_1_on_1_meetings_with_manager, y = group)) +
      geom_point(colour = "#FE7F4F") +
      scale_x_continuous("Manager 1:1 -  Frequency in Weeks") +
      labs(
        title = "Cadence of 1:1 Meetings with manager",
        subtitle = paste("Averages values, by ", hrvar),
        caption = paste(
          "Excludes individuals with no 1:1 meetings. ",
          extract_date_range(data, return = "text")
        ),
        x = camel_clean(hrvar)
      )  +
      theme_wpa_basic() +
      scale_size(guide = "none", range = c(1, 15)) +
      theme(
        axis.line = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "#D9E7F7", size = 3),
        # lightblue bar
        panel.grid.minor.x = element_line(color = "gray"),
        strip.placement = "outside",
        strip.background = element_blank(),
        strip.text = element_blank(
        )
        )

  } else if(mode == "sum" & return == "table"){

    expanded_data %>%
      filter_all(all_vars(!is.infinite(.))) %>%
      create_bar(
        metric = "Cadence_of_1_on_1_meetings_with_manager",
        hrvar = hrvar,
        return = "table",
        mingroup = mingroup
      )

  } else if(mode == "dist"){

    plot_data <-
      expanded_data %>%
      mutate(
        across(
          .cols = Cadence_of_1_on_1_meetings_with_manager,
          .fns = ~ifelse(!is.finite(.), arb_const, .)
        )
      )

    create_dist(
      plot_data,
      metric = "Cadence_of_1_on_1_meetings_with_manager",
      hrvar = hrvar,
      mingroup = mingroup,
      cut = c(
        1, # Once a week
        3, # Twice monthly or more
        6, # Monthly
        10 # Bi-monthly
        ),
      ubound = arb_const + 1, # Bigger than arbitrary constant
      dist_colours = c(
        "#F1B8A1", # Reddish orange
        "#facebc", # Orangeish
        "#fcf0eb", # Light orange
        # "grey90",  # Light grey
        "#bfe5ee", # Light teal
        "#b4d5dd" # Dark teal
        ),
      unit = "Weeks",
      labels = breaks_to_labels,
      return = return,
      sort_by = sort_by
    )
  } else {

    stop("Invalid value passed to `mode` or `return`")

  }
}
