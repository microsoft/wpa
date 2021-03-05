# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Rank all groups across HR attributes on a selected Workplace Analytics metric
#'
#' @description
#' This function scans a standard Person query output for groups with high
#' levels of a given Workplace Analytics Metric. Returns a table with all groups
#' (across multiple HR attributes) ranked by the specified metric.
#'
#' @param data A Standard Person Query dataset in the form of a data frame.
#' @param metric Character string containing the name of the metric,
#' e.g. "Collaboration_hours"
#' @param hrvar A list of HR Variables to consider in the scan.
#' Defaults to all HR attributes identified.
#' @param mingroup Numeric value setting the privacy threshold / minimum group size.
#' Defaults to 5.
#'
#' @param return String specifying what to return. This must be one of the
#'   following strings:
#'   - `"table"` (default)
#'   - `"plot"`
#'
#' See `Value` for more information.
#'
#' @param plot_mode Numeric vector to determine which plot mode to return. Must
#' be either `1` or `2`, and is only used when `return = "plot"`.
#'   - `1`: Top and bottom five groups across the data population are highlighted
#'   - `2`: Top and bottom groups _per_ organizational attribute are highlighted
#'
#' @import dplyr
#' @import ggplot2
#' @import reshape2
#' @import scales
#' @importFrom stats reorder
#'
#' @family Visualization
#' @family Flexible
#'
#' @examples
# Plot mode 1 - show top and bottom five groups
#' create_rank(
#'   data = sq_data,
#'   hrvar = c("FunctionType", "LevelDesignation"),
#'   metric = "Emails_sent",
#'   return = "plot",
#'   plot_mode = 1
#' )
#'
#' # Plot mode 2 - show top and bottom groups per HR variable
#' create_rank(
#'   data = sq_data,
#'   hrvar = c("FunctionType", "LevelDesignation"),
#'   metric = "Emails_sent",
#'   return = "plot",
#'   plot_mode = 2
#' )
#'
#' # Return a table
#' create_rank(
#'   data = sq_data,
#'   metric = "Emails_sent",
#'   return = "table"
#' )
#'
#'
#' @return
#' A different output is returned depending on the value passed to the `return`
#' argument:
#'   - `"plot"`: ggplot object. A bubble plot where the x-axis represents the
#'   metric, the y-axis represents the HR attributes, and the size of the
#'   bubbles represent the size of the organizations.
#'   - `"table"`: data frame. A summary table for the metric.
#'
#' @export

create_rank <- function(data,
                        metric,
                        hrvar = extract_hr(data),
                        mingroup = 5,
                        return = "table",
                        plot_mode = 1){

  results <-
    create_bar(data,
               metric = metric,
               hrvar = hrvar[1],
               mingroup = mingroup,
               return = "table")

  ## Create a blank column
  results$hrvar <- ""

  ## Empty table
  results <- results[0,]

  ## Loop through each HR attribute supplied in argument
  for (p in hrvar) {
  	table1 <-
  	  data %>%
  	  create_bar(metric = metric,
  	             hrvar = p,
  	             mingroup = mingroup,
  	             return = "table")

  	table1$hrvar <- p

  	results <- rbind(results,table1)
  	}

  output <-
    results %>%
    arrange(desc(get(metric))) %>%
    select(hrvar, everything()) %>%
    mutate(group = as.character(group)) # text fails when not string

  if(return == "table"){

    return(output)

  } else if(return == "plot"){

    # Company average
    avg_ch <-
      data %>%
      create_bar(hrvar = NULL, metric = metric, return = "table") %>%
      pull(metric)

    if(plot_mode == 1){

      # Main plot
      output %>%
        mutate(Rank = rev(rank(!!sym(metric), ties.method = "max"))) %>%
        mutate(Group =
                 case_when(Rank %in% 1:5 ~ "Top 5",
                           Rank %in% nrow(.):(nrow(.) - 5) ~ "Bottom 5",
                           TRUE ~ "Middle")) %>%
        group_by(hrvar) %>%
        mutate(OrgGroup =
                 case_when(!!sym(metric) == max(!!sym(metric), na.rm = TRUE) ~ "Top",
                           !!sym(metric) == min(!!sym(metric), na.rm = TRUE) ~ "Bottom",
                           TRUE ~ "Middle")) %>%
        mutate(top_group = max(!!sym(metric), na.rm = TRUE)) %>%
        ungroup() %>%
        ggplot(aes(x = !!sym(metric),
                   y = reorder(hrvar, top_group))) + # Sort by top group
        geom_point(aes(fill = Group,
                       size = n),
                   colour = "black",
                   pch = 21,
                   alpha = 0.8) +
        labs(title = us_to_space(metric),
             subtitle = "Lowest and highest group averages, by org. attribute",
             y = "",
             x = "") +
        ggrepel::geom_text_repel(aes(x = !!sym(metric),
                                     y = hrvar,
                                     label = ifelse(Group %in% c("Top 5", "Bottom 5"), group, "")),
                                 size = 3) +
        scale_x_continuous(position = "top") +
        scale_fill_manual(name = "Group",
                          values = c(rgb2hex(68,151,169),
                                     "white",
                                     "#FE7F4F"),
                          guide = "legend") +
        theme_wpa_basic() +
        scale_size(guide = "none", range = c(1, 15)) +
        theme(
          axis.line=element_blank(),
		  panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(colour = "#D9E7F7", size = 3), # lightblue bar
		  panel.grid.minor.x = element_line(color="gray"),
          strip.placement = "outside",
          strip.background = element_blank(),
          strip.text = element_blank()
        ) +
        geom_vline(xintercept = avg_ch, colour = "red")

    } else if(plot_mode == 2){

      output %>%
        group_by(hrvar) %>%
        mutate(OrgGroup =
                 case_when(!!sym(metric) == max(!!sym(metric), na.rm = TRUE) ~ "Top",
                           !!sym(metric) == min(!!sym(metric), na.rm = TRUE) ~ "Bottom",
                           TRUE ~ "Middle")) %>%
        mutate(top_group = max(!!sym(metric), na.rm = TRUE)) %>%
        ungroup() %>%
        ggplot(aes(x = !!sym(metric),
                   y = reorder(hrvar, top_group))) + # Sort by top group
        geom_point(aes(fill = OrgGroup,
                       size = n),
                   colour = "black",
                   pch = 21,
                   alpha = 0.8) +
        labs(title = us_to_space(metric),
             subtitle = "Group averages by organizational attribute",
             y = "Organizational attributes",
             x = us_to_space(metric)) +
        ggrepel::geom_text_repel(aes(x = !!sym(metric),
                                     y = hrvar,
                                     label = ifelse(OrgGroup %in% c("Top", "Bottom"), group, "")),
                                 size = 3) +
        scale_x_continuous(position = "top") +
        scale_fill_manual(name = "Group",
                          values = c(rgb2hex(68,151,169),
                                     "white",
                                     "#FE7F4F"),
                          guide = "legend") +
        theme_wpa_basic() +
        scale_size(guide = "none", range = c(1, 8)) +
        theme(
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(colour = "#D9E7F7", size = 3), # lightblue bar
          strip.placement = "outside",
          strip.background = element_blank(),
          strip.text = element_blank()
        ) +
        geom_vline(xintercept = avg_ch, colour = "red")

    } else {

      stop("Invalid plot_mode argument.")

    }

  } else {
    stop("Invalid `return` argument.")
  }
}
