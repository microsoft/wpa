#' @title Collaboration Time Trend
#'
#' @description
#' Provides a week by week view of collaboration time.
#' By default returns a week by week heatmap, highlighting the points in time with most activity.
#' Additional options available to return a summary table.
#'
#' @details
#' Uses the metric `Collaboration_hours`.
#'
#' @param data A Standard Query dataset in the form of a data frame.
#' @param hrvar HR Variable by which to split metrics, defaults to "Organization"
#'  but accepts any character vector, e.g. "LevelDesignation"
#' @param mingroup Numeric value setting the privacy threshold / minimum group size. Defaults to 5.
#' @param return Character vector specifying what to return, defaults to "plot".
#' Valid inputs are "plot" and "table".
#'
#' @import dplyr
#' @import ggplot2
#' @import reshape2
#' @import scales
#'
#' @family Collaboration
#'
#' @return
#' Returns a ggplot object by default, where 'plot' is passed in `return`.
#' When 'table' is passed, a summary table is returned as a data frame.
#'
#' @export

collaboration_trend <- function(data,
                                hrvar = "Organization",
                                mingroup=5,
                                return = "plot"){

  ## Check inputs
  required_variables <- c("Date",
                          "Collaboration_hours",
                          "PersonId")

  ## Error message if variables are not present
  ## Nothing happens if all present
  data %>%
    check_inputs(requirements = required_variables)

  myTable <-
    data %>%
    mutate(Date = as.Date(Date, "%m/%d/%Y")) %>%
    rename(group = !!sym(hrvar)) %>% # Rename HRvar to `group`
    select(PersonId, Date, group, Collaboration_hours) %>%
    group_by(group) %>%
    mutate(Employee_Count = n_distinct(PersonId)) %>%
    filter(Employee_Count >= mingroup)  # Keep only groups above privacy threshold

  myTable <-
    myTable %>%
    group_by(Date, group) %>%
    summarize(Employee_Count = mean(Employee_Count),
              Collaboration_hours = mean(Collaboration_hours))

  myTable_plot <- myTable %>% select(Date, group, Collaboration_hours)

  myTable_return <-  myTable_plot %>% spread(Date, Collaboration_hours)

  plot_object <-
    myTable_plot %>%
    ggplot(aes(x = Date , y = group , fill = Collaboration_hours)) +
    geom_tile(height=.5) +
    scale_fill_gradient(name="Hours", low = "white", high = "red") +
    theme_classic() +
    theme(axis.text=element_text(size=12),
          plot.title = element_text(color="grey40", face="bold", size=18),
          plot.subtitle = element_text(size=14),
          legend.position = "right",
          legend.justification = "right",
          legend.title=element_text(size=14),
          legend.text=element_text(size=14)) +
    labs(title = "Collaboration Hours",
         subtitle = paste("Total meeting and email time by", tolower(hrvar))) +
    xlab("Date") +
    ylab(hrvar) +
    labs(caption = extract_date_range(data, return = "text"))

  if(return == "table"){

    myTable_return %>%
	  as_tibble()  %>%
      return()

  } else if(return == "plot"){

    return(plot_object)

  } else {

    stop("Please enter a valid input for `return`.")

  }

}
