#' @title Plot the external network metrics for a HR variable
#'
#' @description
#' Plot the external network metrics for a HR variable.
#'
#' @param data Person Query as a dataframe including date column named "Date"
#' This function assumes the data format is MM/DD/YYYY as is standard in a WpA query output.
#' @param hrvar WpA variable for an HR variable to group networks by
#'  For example, "Layer"
#' @param mingroup Numeric vector for minimum group size for aggregation
#' @param return Character vector specifying what to return, defaults to "plot".
#' Valid inputs are "plot" and "table".
#' @param bubble_size A numeric vector of length two to specify the size range of the bubbles
#'
#' @import dplyr
#' @import reshape2
#' @import ggplot2
#' @import ggrepel
#'
#' @examples
#' \dontrun{external_network_plot(sq_data)
#
#' }
#'
#' @family Connectivity
#'
#' @export

external_network_plot <- function(data,
                                  hrvar = "Organization",
                                  mingroup = 5,
                                  return = "plot",
                                  bubble_size = c(1, 5)){
  plot_data <-
    data %>%
    rename(group = !!sym(hrvar))

  plot_data <-
    plot_data %>%
    group_by(group) %>%
    summarize(Ext_network_size = mean(External_network_size),
              Ext_network_breadth = mean(Networking_outside_company),
              Employee_count = n_distinct(PersonId)
    ) %>%
    filter(Employee_count >= mingroup)

  summary_table <- plot_data %>% arrange(., desc(Ext_network_size))

  ext_network_plot <- ggplot(plot_data,
         aes(x=Ext_network_size,
             y=Ext_network_breadth)
  ) +
    geom_point(aes(size=Employee_count),
               color = rgb2hex(0, 120, 212),
               alpha = 0.5) +
    geom_text_repel(label=plot_data$group) +
    scale_x_continuous(name = "External Network Size") +
    scale_y_continuous(name = "External Network Breadth") +
    scale_size(range = bubble_size) +
    theme_classic() +
    theme(
      axis.text = element_text(size = 10),
      axis.text.x = element_text(
        angle = 90,
        hjust = 1,
        vjust = 0.5
      ),
      plot.title = element_text(
        color = "grey40",
        face = "bold",
        size = 18
      ),
      plot.subtitle = element_text(size = 14)
    ) +
    labs(
      size = "Size"
    ) +
    labs(
      title = paste("External network metrics by", hrvar),
      subtitle = paste(
        "Network size is number of people, breadth is number of organizations"
      )
    ) +
    labs(
      caption = paste(
        "Total employees =",
        sum(plot_data$Employee_count),
        "| Data from",
        min(as.Date(data$Date, "%m/%d/%Y")),
        "to",
        max(as.Date(data$Date, "%m/%d/%Y"))
      )
    )

  if(return == "table"){

    summary_table %>%
      as_tibble() %>%
      return()

  } else if(return == "plot"){

    return(ext_network_plot)

  } else {

    stop("Please enter a valid input for `return`.")

  }

}
