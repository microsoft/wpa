#' @title Create a network plot with the G2G query
#'
#' @description
#' Pass a data frame containing a G2G query and return a network
#' plot.
#' Automatically handles "Collaborators_within_group" and "Other_collaborators" within query data.
#'
#' @param data Data frame containing a G2G query.
#' @param time_investor String containing the variable name for the Time Investor column.
#' @param collaborator String containing the variable name for the Collaborator column.
#' @param metric String containing the variable name for metric.
#' @param exc_threshold Exclusion threshold to apply.
#' @param subtitle String to override default plot subtitle.
#' @param ... Additional arguments to pass to `GGally::ggnet2()`
#' @param return Character vector specifying what to return, defaults to "plot".
#' Valid inputs are "plot" and "table".
#'
#'
#' @import ggplot2
#' @import dplyr
#'
#' @examples
#' \dontrun{
#' g2g_data %>%
#'   g2g_network(time_investor = "TimeInvestors_Organization",
#'               collaborator = "Collaborators_Organization",
#'               metric = "Collaboration_hours")
#' }
#'
#' @export
g2g_network <- function(data,
                        time_investor,
                        collaborator,
                        metric,
                        exc_threshold = 0.1,
                        subtitle = "Collaboration Across Organizations",
                        return = "plot",
                        ...){

  plot_data <-
    data %>%
    rename(TimeInvestorOrg = time_investor,
           CollaboratorOrg = collaborator,
           Metric = metric) %>%
    mutate(CollaboratorOrg = case_when(CollaboratorOrg == "Collaborators Within Group" ~ TimeInvestorOrg,
                                     TRUE ~ CollaboratorOrg)) %>%
    group_by(TimeInvestorOrg, CollaboratorOrg) %>%
    filter(TimeInvestorOrg != "Other_Collaborators" &
             CollaboratorOrg!="Other_Collaborators") %>%
    summarise_at("Metric", ~mean(.)) %>%
    group_by(TimeInvestorOrg) %>%
    mutate(metric_prop = Metric / sum(Metric, na.rm = TRUE)) %>%
    select(TimeInvestorOrg, CollaboratorOrg, metric_prop) %>%
    ungroup()

  if(return == "table"){

    plot_data

  } else if(return == "plot"){

    mynet_em <-
      plot_data %>%
      filter(metric_prop > exc_threshold) %>%
      mutate_at(vars(TimeInvestorOrg, CollaboratorOrg), ~sub(pattern = " ", replacement = "\n", x = .)) %>%
      mutate(metric_prop = metric_prop * 10) %>%
      network::network(matrix.type = "edgelist",
                       ignore.eval = FALSE,
                       names.eval = "weights")

    mynet_em %>%
      GGally::ggnet2(size = 12,
                     color = "lightblue",
                     label = TRUE,
                     label.size = 4,
                     label.color = "black",
                     edge.size = "weights",
                     edge.alpha = .5,
                     ...) +
      ggtitle("Group to Group Collaboration",
              subtitle = subtitle) +
      xlab(label = "") +
      ylab(label = "") +
      theme_wpa_basic() +
      labs(caption = paste("Displays only collaboration above ", exc_threshold * 100, "% of node's total collaboration", sep = "")) +
      theme(axis.line = element_blank())

  } else {

    stop("Please enter a valid input for `return`.")

  }
}
