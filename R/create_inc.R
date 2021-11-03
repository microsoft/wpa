#' @title
#' Create an incidence table reflecting proportion of population scoring above
#' or below a threshold for a metric
#'
#' @description
#' An incidence table is generated, with each value in the table reflecting the
#' proportion of the population that is above or below a threshold for a
#' specified metric. Two values are required for `hrvar` in order to generate
#' this incidence table.
#'
#' @param data A Standard Person Query dataset in the form of a data frame.
#' @param metric Character string containing the name of the metric,
#' e.g. "Collaboration_hours"
#' @param hrvar Character vector of length 2 containing the name of the HR
#'   Variable by which to split metrics.
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
#'
#' @examples
#' create_inc(
#'   data = sq_data,
#'   metric = "Collaboration_hours",
#'   hrvar = c("Organization", "LevelDesignation"),
#'   threshold = 20,
#'   position = "above"
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

  if(length(hrvar) != 2){
    stop("`hrvar` can only accept a character vector of length 2.")
  }


  myTable <-
    data %>%
    { if (position == "above"){
        mutate(., !!sym(metric) := !!sym(metric) >= threshold)
      } else if (position == "below"){
        mutate(., !!sym(metric) := !!sym(metric) <= threshold)
      }
      } %>%
    group_by(!!sym(hrvar[1]), !!sym(hrvar[2])) %>%
    summarise(
      across(
        .cols = !!sym(metric),
        .fns = ~mean(., na.rm = TRUE)
      )
    )
  #TODO: mingroup size

  if(return == "table"){

    myTable

  } else if(return == "plot"){

    myTable %>%
      ggplot(aes(x = !!sym(hrvar[1]),
                 y = !!sym(hrvar[2]),
                 fill = !!sym(metric))) +
      geom_tile() +
      geom_text(aes(label = scales::percent(!!sym(metric), accuracy = 1)),
                colour = "black",
                size = 2) +
      scale_fill_gradient(
        low = "black",
        high = "green"
      )

  } else {

    stop("please enter a valid value for `return`.")

  }
}


