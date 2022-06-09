# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title
#' Rank all groups across HR attributes on a selected Viva Insights metric
#'
#' @description
#' This function scans a standard Person query output for groups with high
#' levels of a given Viva Insights Metric. Returns a plot by default, with an
#' option to return a table with all groups (across multiple HR attributes)
#' ranked by the specified metric.
#'
#' @author Carlos Morales Torrado <carlos.morales@@microsoft.com>
#' @author Martin Chan <martin.chan@@microsoft.com>
#'
#' @template spq-params
#' @param metric Character string containing the name of the metric,
#' e.g. "Collaboration_hours"
#'
#' @param return String specifying what to return. This must be one of the
#'   following strings:
#'   - `"plot"` (default)
#'   - `"table"`
#'
#' See `Value` for more information.
#'
#' @param mode String to specify calculation mode. Must be either:
#'   - `"simple"`
#'   - `"combine"`
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
# Use a small sample for faster runtime
#' sq_data_small <- dplyr::slice_sample(sq_data, prop = 0.1)
#'
#' # Plot mode 1 - show top and bottom five groups
#' create_rank(
#'   data = sq_data_small,
#'   hrvar = c("FunctionType", "LevelDesignation"),
#'   metric = "Emails_sent",
#'   return = "plot",
#'   plot_mode = 1
#' )
#'
#' # Plot mode 2 - show top and bottom groups per HR variable
#' create_rank(
#'   data = sq_data_small,
#'   hrvar = c("FunctionType", "LevelDesignation"),
#'   metric = "Emails_sent",
#'   return = "plot",
#'   plot_mode = 2
#' )
#'
#' # Return a table
#' create_rank(
#'   data = sq_data_small,
#'   metric = "Emails_sent",
#'   return = "table"
#' )
#'
#' \donttest{
#' # Return a table - combination mode
#' create_rank(
#'   data = sq_data_small,
#'   metric = "Emails_sent",
#'   mode = "combine",
#'   return = "table"
#' )
#' }
#'
#' @return
#' A different output is returned depending on the value passed to the `return`
#' argument:
#'   - `"plot"`: 'ggplot' object. A bubble plot where the x-axis represents the
#'   metric, the y-axis represents the HR attributes, and the size of the
#'   bubbles represent the size of the organizations. Note that there is no
#'   plot output if `mode` is set to `"combine"`.
#'   - `"table"`: data frame. A summary table for the metric.
#'
#' @export

create_rank <- function(data,
                        metric,
                        hrvar = extract_hr(data, exclude_constants = TRUE),
                        mingroup = 5,
                        return = "table",
                        mode = "simple",
                        plot_mode = 1){


  if(mode == "simple"){

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

      output

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
          ggrepel::geom_text_repel(
            aes(x = !!sym(metric),
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

  } else if(mode == "combine"){

    create_rank_combine(
      data = data,
      hrvar = hrvar,
      metric = metric,
      mingroup = mingroup
    )

  } else {

    stop("Invalid `mode` argument.")

  }
}

#' @title Create combination pairs of HR variables and run 'create_rank()'
#'
#' @description Create pairwise combinations of HR variables and compute an
#' average of a specified advanced insights metric.
#'
#' @details
#' This function is called when the `mode` argument in `create_rank()` is
#' specified as `"combine"`.
#'
#' @inheritParams create_rank
#'
#' @examples
#' # Use a small sample for faster runtime
#' sq_data_small <- dplyr::slice_sample(sq_data, prop = 0.1)
#'
#' create_rank_combine(
#'   data = sq_data_small,
#'   metric = "Email_hours"
#' )
#'
#' @return Data frame containing the following variables:
#'   - `hrvar`: placeholder column that denotes the output as `"Combined"`.
#'   - `group`: pairwise combinations of HR attributes with the HR attribute
#'   in square brackets followed by the value of the HR attribute.
#'   - Name of the metric (as passed to `metric`)
#'   - `n`
#'
#' @export
create_rank_combine <- function(data,
                                hrvar = extract_hr(data),
                                metric,
                                mingroup = 5){

  hrvar_iter_grid <-
    tidyr::expand_grid(var1 = hrvar,
                var2 = hrvar) %>%
    dplyr::filter(var1 != var2)

  hrvar_iter_grid %>%
    purrr::pmap(function(var1, var2){

      data %>%
        dplyr::mutate(Combined =
                 paste0(
                   "[",var1, "] ",
                   !!sym(var1),
                   " [",var2, "] ",
                   !!sym(var2))) %>%
        create_rank(
          metric = metric,
          hrvar = "Combined",
          mode = "simple",
          mingroup = mingroup
        )
    }) %>%
    dplyr::bind_rows()
}
