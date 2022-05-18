# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------
#' @title Summarise node centrality statistics with an igraph object
#'
#' @description
#' Pass an igraph object to the function and obtain centrality statistics for
#' each node in the object as a data frame. This function works as a wrapper of
#' the centralization functions in 'igraph'.
#'
#' @param graph 'igraph' object that can be returned from `network_g2g()` or
#'   `network_p2p()`when the `return` argument is set to `"network"`.
#'
#' @param hrvar String containing the name of the HR Variable by which to split
#'   metrics. Defaults to `NULL`.
#'
#' @param return String specifying what output to return. Valid inputs include:
#'   - `"table"`
#'   - `"network"`
#'   - `"plot"`
#'
#' See `Value` for more information.
#'
#' @return
#' By default, a data frame containing centrality statistics. Available
#' statistics include:
#'   - betweenness: number of shortest paths going through a node.
#'   - closeness: number of steps required to access every other node from a
#'   given node.
#'   - degree: number of connections linked to a node.
#'   - eigenvector: a measure of the influence a node has on a network.
#' Please refer to the igraph package documentation for the detailed technical
#' definition.
#'
#' When `"network"` is passed to `"return"`, an 'igraph' object is returned with
#' additional node attributes containing centrality scores.
#'
#' When `"plot"` is passed to `"return"`, a summary table is returned showing
#' the average centrality scores by HR attribute. This is currently available if
#' there is a valid HR attribute.
#'
#' @family Network
#'
#' @examples
#' # Simulate a p2p network
#' p2p_data <- p2p_data_sim()
#' g <- network_p2p(data = p2p_data, return = "network")
#'
#' # Return summary table
#' network_summary(graph = g, return = "table")
#'
#' # Return network with node centrality statistics
#' network_summary(graph = g, return = "network")
#'
#' # Return summary plot
#' network_summary(graph = g, return = "plot", hrvar = "Organization")
#'
#' # Simulate a g2g network and return table
#' g2 <- g2g_data %>% network_g2g(return = "network")
#' network_summary(graph = g2, return = "table")
#'
#' @export

network_summary <- function(graph, hrvar = NULL, return = "table"){

  ## NULL variables
  node_id <- NULL


  ## Calculate summary table
  sum_tb <-
    dplyr::tibble(
      node_id = igraph::vertex.attributes(graph = graph)$name,
      betweenness = igraph::centralization.betweenness(graph = graph)$res,
      closeness = igraph::centralization.closeness(graph = graph)$res,
      degree = igraph::centralization.degree(graph = graph)$res,
      eigenvector = igraph::centralization.evcent(graph = graph)$vector
    )

  if(!is.null(hrvar)){

    sum_tb <-
      cbind(
        sum_tb,
        hrvar = igraph::get.vertex.attribute(graph = graph, name = hrvar)
      )

    sum_tb <- dplyr::rename(sum_tb, !!sym(hrvar) := "hrvar")
  }

  if(return == "table"){

    sum_tb

  } else if(return == "network"){

    graph <- igraph::set_vertex_attr(
      graph = graph,
      name = "betweenness",
      value = sum_tb$betweenness
      ) %>%
      igraph::set_vertex_attr(
        name = "closeness",
        value = sum_tb$closeness
        ) %>%
      igraph::set_vertex_attr(
        name = "degree",
        value = sum_tb$degree
      ) %>%
      igraph::set_vertex_attr(
        name = "eigenvector",
        value = sum_tb$eigenvector
      )

    graph

  } else if(return == "plot"){

    if(is.null(hrvar)){

      message("Visualisation options currently only available when a valid
              HR attribute is supplied.")

    } else {

      plot_obj <-
        sum_tb %>%
        mutate(PersonId = node_id) %>%
        mutate(Date = Sys.Date()) %>%
        keymetrics_scan(
          hrvar = hrvar,
          metrics = c(
            "betweenness",
            "closeness",
            "degree",
            "eigenvector"
          )
        )
    }

    plot_obj +
      labs(
        title = "Network centrality statistics",
        subtitle = paste("By", hrvar),
        caption = paste(
          c(
            "betweenness: number of shortest paths going through a node.",
            "closeness: number of steps required to access every other node from a given node.",
            "degree: number of connections linked to a node.",
            "eigenvector: a measure of the influence a node has on a network."
          ),
          collapse = "\n"
        )
      )

  } else {

    stop("Invalid input to `return`")

  }

}


