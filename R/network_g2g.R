# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Create a network plot with the group-to-group query
#'
#' @description
#' Pass a data frame containing a group-to-group query and return a network
#' plot. Automatically handles `"Collaborators_within_group"` and
#' `"Other_collaborators"` within query data.
#'
#' @param data Data frame containing a G2G query.
#' @param time_investor String containing the variable name for the Time
#'   Investor column.
#' @param collaborator String containing the variable name for the Collaborator
#'   column.
#' @param metric String containing the variable name for metric. Defaults to
#'   `Collaboration_hours`.
#' @param algorithm String to specify the node placement algorithm to be used.
#'   Defaults to `"fr"` for the force-directed algorithm of Fruchterman and
#'   Reingold. See
#'   <https://rdrr.io/cran/ggraph/man/layout_tbl_graph_igraph.html> for a full
#'   list of options.
#' @param node_colour String to specify the colour to be used for displaying
#' nodes. Defaults to `"lightblue"`. If `"vary"` is supplied, a different colour
#' is shown for each node at random.
#' @param exc_threshold Numeric value between 0 and 1 specifying the exclusion
#'   threshold to apply. Defaults to 0.1, which means that the plot will only
#'   display collaboration above 10% of a node's total collaboration. This
#'   argument has no impact on `"data"` or `"table"` return.
#' @param org_count Optional data frame to provide the size of each organization
#' in the `collaborator` attribute. The data frame should contain only two
#' columns:
#'   - Name of the `collaborator` attribute excluding any prefixes, e.g.
#'   `"Organization"`. Must be of character or factor type.
#'   - `"n"`. Must be of numeric type.
#' Defaults to `NULL`, where node sizes will be fixed.
#'
#' @param subtitle String to override default plot subtitle.
#' @param return String specifying what to return. This must be one of the
#'   following strings:
#'   - `"plot"`
#'   - `"table"`
#'   - `"network"`
#'   - `"data"`
#'
#' See `Value` for more information.
#'
#' @return
#' A different output is returned depending on the value passed to the `return`
#' argument:
#'   - `"plot"`: 'ggplot' object. A group-to-group network plot.
#'   - `"table"`: data frame. An interactive matrix of the network.
#'   - `"network`: 'igraph' object used for creating the network plot.
#'   - `"data"`: data frame. A long table of the underlying data.
#'
#' @import ggplot2
#' @import dplyr
#'
#' @family Network
#'
#' @examples
#' # Return a network plot
#' g2g_data %>% network_g2g()
#'
#' # Return a network plot - Meeting hours and 5% threshold
#' g2g_data %>%
#'   network_g2g(time_investor = "TimeInvestors_Organization",
#'               collaborator = "Collaborators_Organization",
#'               metric = "Meeting_hours",
#'               exc_threshold = 0.05)
#'
#' # Return a network plot with circle layout
#' # Vary node colours and add org sizes
#' org_tb <- hrvar_count(
#'   sq_data,
#'   hrvar = "Organization",
#'   return = "table"
#' )
#'
#' g2g_data %>%
#'   network_g2g(algorithm = "circle",
#'               node_colour = "vary",
#'               org_count = org_tb)
#'
#' # Return an interaction matrix
#' # Minimum arguments specified
#' g2g_data %>%
#'   network_g2g(return = "table")
#'
#'
#' @export
network_g2g <- function(data,
                        time_investor = NULL,
                        collaborator = NULL,
                        metric = "Collaboration_hours",
                        algorithm = "fr",
                        node_colour = "lightblue",
                        exc_threshold = 0.1,
                        org_count = NULL,
                        subtitle = "Collaboration Across Organizations",
                        return = "plot"){

  if(is.null(time_investor)){

    # Only return first match
    time_investor <-
      names(data)[grepl(pattern = "^TimeInvestors_", names(data))][1]

    message(
      paste("`time_investor` field not provided.",
            "Assuming", wrap(time_investor, wrapper = "`"),
            "as the `time_investor` variable.")
    )

  }

  if(is.null(collaborator)){

    # Only return first match
    collaborator <-
      names(data)[grepl(pattern = "^Collaborators_", names(data))][1]

    message(
      paste("`collaborator` field not provided.",
            "Assuming", wrap(collaborator, wrapper = "`"),
            "as the `collaborator` variable.")
      )

  }

  ## Get string of HR variable
  hrvar_string <- gsub(pattern = "Collaborators_",
                       replacement = "",
                       x = collaborator)

  ## Warn if 'Collaborators Within Group' is not present in data
  if(! "Collaborators Within Group" %in% unique(data[[collaborator]])){

    warning(
      "`Collaborators Within Group` is not found in the collaborator variable.
      The analysis may be excluding in-group collaboration."
      )

  }


  ## Run plot_data
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

    ## Return a 'tidy' matrix
    plot_data %>%
      tidyr::pivot_wider(names_from = CollaboratorOrg,
                         values_from = metric_prop)

  } else if(return == "data"){

    ## Return long table
    plot_data

  } else if(return %in% c("plot", "network")){

    ## Network object
    mynet_em <-
      plot_data %>%
      filter(metric_prop > exc_threshold) %>%
      mutate_at(vars(TimeInvestorOrg, CollaboratorOrg), ~sub(pattern = " ", replacement = "\n", x = .)) %>%
      mutate(metric_prop = metric_prop * 10) %>%
      igraph::graph_from_data_frame(directed = FALSE)

    # Org count vary by size -------------------------------------------

    if(!is.null(org_count)){

      igraph::V(mynet_em)$org_size <-
        tibble(id = igraph::get.vertex.attribute(mynet_em)$name) %>%
        mutate(id = gsub(pattern = "\n", replacement = " ", x = id)) %>%
        left_join(org_count, by = c("id" = hrvar_string)) %>%
        pull(n)

    } else {

      # Imputed size if not specified
      igraph::V(mynet_em)$org_size <-
        tibble(id = igraph::get.vertex.attribute(mynet_em)$name) %>%
        mutate(id = gsub(pattern = "\n", replacement = " ", x = id)) %>%
        mutate(n = 20) %>%
        pull(n)
    }

    ## Plot object
    plot_obj <-
      mynet_em %>%
      ggraph::ggraph(layout = algorithm) +
      ggraph::geom_edge_link(aes(edge_width = metric_prop * 1),
                             edge_alpha = 0.5,
                             edge_colour = "grey")

    if(return == "network"){

      mynet_em # Return 'igraph' object

    } else {

      # Custom node colours ----------------------------------------------
      if(node_colour == "vary"){

        plot_obj <-
          plot_obj +
          ggraph::geom_node_point(
            aes(color = name,
                size = org_size),
            alpha = 0.9
            )

      } else {

        plot_obj <-
          plot_obj +
          ggraph::geom_node_point(
            aes(size = org_size),
            colour = node_colour,
            alpha = 0.9
            )

      }

      plot_obj +
        ggraph::geom_node_text(aes(label = name), size = 3, repel = FALSE) +
        ggplot2::theme(
          panel.background = ggplot2::element_rect(fill = 'white'),
          legend.position = "none") +
        theme_wpa_basic() +
        scale_size(range = c(1, 30)) +
        labs(title = "Group to Group Collaboration",
             subtitle = subtitle,
             x = "",
             y = "",
             caption = paste("Displays only collaboration above ", exc_threshold * 100, "% of node's total collaboration", sep = "")) +
        theme(axis.line = element_blank(),
              axis.text = element_blank(),
              legend.position = "none")

    }

  } else {

    stop("Please enter a valid input for `return`.")

  }
}

#' @rdname network_g2g
#' @export
g2g_network <- network_g2g
