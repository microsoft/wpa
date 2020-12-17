# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Implement the Louvain community detection on a Person to Person network query
#'
#' @description
#' Take a P2P network query and implement the Louvain community detection method. The
#' **igraph** implementation of the Louvain method is used.
#'
#' @param data Data frame containing a Person to Person query.
#' @param hrvar String containing the HR attribute to be matched in the dataset.
#' @param return String specifying what output to return.
#'
#' @import ggraph
#' @import dplyr
#'
#' @export
network_louvain <- function(data, hrvar, return){

  ## Set variables
  TO_hrvar <- paste0("TieOrigin_", hrvar)
  TD_hrvar <- paste0("TieDestination_", hrvar)

  ## Set edges df
  edges <-
    data %>%
    select(from = "TieOrigin_PersonId",
           to = "TieDestination_PersonId",
           weight = "StrongTieScore")

  ## Vertices data frame to provide meta-data
  vert_ft <-
    rbind(
      # TieOrigin
      edges %>%
        left_join(select(data, TieOrigin_PersonId, TO_hrvar),
                  by = c("from"  = "TieOrigin_PersonId")) %>%
        select(node = "from", !!sym(hrvar) := TO_hrvar),

      # TieDestination
      edges %>%
        left_join(select(data, TieDestination_PersonId, TD_hrvar),
                  by = c("to"  = "TieDestination_PersonId")) %>%
        select(node = "to", !!sym(hrvar) := TD_hrvar)
    )

  ## Create igraph object
  g_raw <-
    igraph::graph_from_data_frame(edges,
                                  directed = FALSE, # Set to undirected for clustering
                                  vertices = unique(vert_ft)) # remove duplicates

  ## Return a numeric vector of partitions / clusters / modules
  ## Set a low resolution parameter to have fewer groups
  lc <- igraph::cluster_louvain(g_raw)

  ## Add cluster
  g <-
    g_raw %>%
    # Add louvain partitions to graph object
    igraph::set_vertex_attr("cluster", value = igraph::membership(lc)) # Return membership - diff from Leiden

  ## Create vertex table
  vertex_tb <-
    g %>%
    igraph::get.vertex.attribute() %>%
    as_tibble() %>%
    count(!!sym(hrvar), cluster)

  ## Return
  if(return == "plot-louvain"){
    g %>%
      ggraph::ggraph(layout = "stress") +
      ggraph::geom_edge_link(colour = "lightgrey", edge_width = 0.1) +
      ggraph::geom_node_point(aes(colour = cluster), alpha = 0.8) +
      theme_void() +
      theme(legend.position = "bottom",
            # legend.background = element_rect(fill = "black"),
            # plot.background = element_rect(fill = "black"),
            # text = element_text(colour = "white"),
            axis.line = element_blank()) +
      labs(title = "Person to person collaboration with Community Detection",
           subtitle = "Based on louvain algorithm and Strong Tie Score",
           y = "",
           x = "")
  } else if(return == "plot-hrvar"){
    g %>%
      ggraph::ggraph(layout = "stress") +
      ggraph::geom_edge_link(colour = "lightgrey", edge_width = 0.1) +
      ggraph::geom_node_point(aes(colour = !!sym(hrvar)), alpha = 0.8) +
      theme_void() +
      theme(legend.position = "bottom",
            axis.line = element_blank()) +
      labs(title = "Person to person collaboration",
           subtitle = paste0("Showing ", hrvar),
           y = "",
           x = "")

  } else if(return == "table"){

    vertex_tb

  } else if(return == "plot-sankey"){

    create_sankey(data = vertex_tb,
                  var1 = hrvar,
                  var2 = "cluster",
                  count = "n")

  }
}
