#' @title
#' Create an incidence analysis reflecting proportion of population scoring above
#' or below a threshold for a metric
#'
#' @description
#' An incidence analysis is generated, with each value in the table reflecting
#' the proportion of the population that is above or below a threshold for a
#' specified metric. There is an option to only provide a single `hrvar` in
#' which a bar plot is generated, or two `hrvar` values where an incidence table
#' (heatmap) is generated.
#'
#'
#' @param data A Standard Person Query dataset in the form of a data frame.
#' @param metric Character string containing the name of the metric,
#' e.g. "Collaboration_hours"
#' @param hrvar Character vector of at most length 2 containing the name of the
#'   HR Variable by which to split metrics.
#' @param mingroup Numeric value setting the privacy threshold / minimum group
#'   size. Defaults to 5.
#' @param threshold Numeric value specifying the threshold.
#' @param position String containing the below valid values:
#'   - `"above"`: show incidence of those equal to or above the threshold
#'   - `"below"`: show incidence of those equal to or below the threshold
#' @param return String specifying what to return. This must be one of the
#'   following strings:
#'   - `"plot"`
#'   - `"table"`
#'
#' See `Value` for more information.
#'
#' @return
#' A different output is returned depending on the value passed to the `return` argument:
#'   - `"plot"`: 'ggplot' object. A heat map.
#'   - `"table"`: data frame. A summary table.
#'
#' @import dplyr
#' @import ggplot2
#' @importFrom scales percent
#'
#' @family Visualization
#' @family Flexible
#'
#' @examples
#' # Only a single HR attribute
#' create_inc(
#'   data = sq_data,
#'   metric = "After_hours_collaboration_hours",
#'   hrvar = "Organization",
#'   threshold = 4,
#'   position = "above"
#' )
#'
#' # Two HR attributes
#' create_inc(
#'   data = sq_data,
#'   metric = "Collaboration_hours",
#'   hrvar = c("LevelDesignation", "Organization"),
#'   threshold = 20,
#'   position = "below"
#' )
#'
#' @export

create_inc <- function(
  data,
  metric,
  hrvar,
  mingroup = 5,
  threshold,
  position,
  return = "plot"
){

  if(length(hrvar) == 1){

    create_inc_bar(
      data = data,
      metric = metric,
      hrvar = hrvar,
      mingroup = mingroup,
      threshold = threshold,
      position = position,
      return = return
    )

  } else if(length(hrvar) == 2){

    create_inc_grid(
      data = data,
      metric = metric,
      hrvar = hrvar,
      mingroup = mingroup,
      threshold = threshold,
      position = position,
      return = return
    )


  } else {

    stop("`hrvar` can only accept a character vector of length 2.")

  }

}

#' @rdname create_inc
#' @export
create_incidence <- create_inc


#' Run `create_inc` with only single `hrvar`
#' Returning a bar chart
#'
#' @noRd
create_inc_bar <- function(
  data,
  metric,
  hrvar,
  mingroup = 5,
  threshold,
  position,
  return = "plot"
){

  # Transform data so that metrics become proportions
  data_t <-
    data %>%
    { if (position == "above"){
      mutate(., !!sym(metric) := !!sym(metric) >= threshold)
    } else if (position == "below"){
      mutate(., !!sym(metric) := !!sym(metric) <= threshold)
    }
    }

  # Set title text
  title_text <-
    paste(
      "Incidence of",
      tolower(us_to_space(metric)),
      position,
      threshold
    )

  # Set subtitle text
  subtitle_text <-
    paste(
      "Percentage and number of employees by",
      hrvar
    )

  # Pipe result to `create_bar()`
  create_bar(
    data = data_t,
    metric = metric,
    hrvar = hrvar,
    mingroup = mingroup,
    return = return,
    plot_title = title_text,
    plot_subtitle = subtitle_text,
    legend_lab = paste("% with",
                       tolower(us_to_space(metric)),
                       position,
                       threshold),
    percent = TRUE
  )
}

#' Run `create_inc` with only two `hrvar`
#' Returning a heatmap
#'
#' @noRd
create_inc_grid <- function(
  data,
  metric,
  hrvar,
  mingroup = 5,
  threshold,
  position,
  return = "plot"
){

  # Create table of proportions
  myTable <-
    data %>%
    { if (position == "above"){
      mutate(., !!sym(metric) := !!sym(metric) >= threshold)
    } else if (position == "below"){
      mutate(., !!sym(metric) := !!sym(metric) <= threshold)
    }
    } %>%
    group_by(!!sym(hrvar[1]), !!sym(hrvar[2]), PersonId) %>%
    summarise(
      !!sym(metric) := mean(!!sym(metric), na.rm = TRUE)
    ) %>%
    group_by(!!sym(hrvar[1]), !!sym(hrvar[2])) %>%
    summarise(
      !!sym(metric) := mean(!!sym(metric), na.rm = TRUE),
      n = n_distinct(PersonId),
      .groups = "drop"
    ) %>%
    filter(n >= mingroup) %>%
    arrange(desc(!!sym(metric)))

  if(return == "table"){

    myTable

  } else if(return == "plot"){

    # Set title text
    title_text <-
      paste(
        "Incidence of",
        tolower(us_to_space(metric)),
        position,
        threshold
      )

    # Set subtitle text
    subtitle_text <-
      paste(
        "Percentage and number of employees by",
        hrvar[1],
        "and",
        hrvar[2]
      )

    metric_text <- NULL

    myTable %>%
      mutate(metric_text = paste0(
        scales::percent(!!sym(metric), accuracy = 1),
        " (", n, ")")) %>%
      ggplot(aes(x = !!sym(hrvar[1]),
                 y = !!sym(hrvar[2]),
                 fill = !!sym(metric))) +
      geom_tile() +
      geom_text(aes(label = metric_text),
                colour = "black",
                size = 3)+
      scale_fill_gradient2(low = rgb2hex(7, 111, 161),
                           mid = rgb2hex(241, 204, 158),
                           high = rgb2hex(216, 24, 42),
                           midpoint = 0.5,
                           breaks = c(0, 0.5, 1),
                           labels = c("0%", "", "100%"),
                           limits = c(0, 1)) +
      scale_x_discrete(position = "top", labels = us_to_space) +
      scale_y_discrete(labels = us_to_space) +
      theme_wpa_basic() +
      labs(
        title = title_text,
        subtitle = subtitle_text,
        caption = paste(
          extract_date_range(data, return = "text"),
          "\n",
          "Percentages reflect incidence with respect to population in cell."),
        fill = "Incidence"
      )
  }
}
