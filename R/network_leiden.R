# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Implement the Leiden community detection on a Person to Person network query
#'
#' @description
#' Take a P2P network query and implement the Leiden community detection method. To run
#' this function, you will require all the pre-requisites of the **leiden** package installed,
#' which includes Python and **reticulate**.
#'
#' @param data Data frame containing a Person to Person query.
#' @param hrvar String containing the HR attribute to be matched in the dataset.
#' @param res Resolution parameter to be passed to `leiden::leiden()`. Defaults to 0.5.
#' @param return String specifying what output to return. Valid return options include:
#'   - 'plot-leiden': return a network plot coloured by leiden communities.
#'   - 'plot-hrvar': return a network plot coloured by HR attribute.
#'   - 'plot-sankey': return a sankey plot combining communities and HR attribute.
#'   - 'table': return a vertex summary table with counts in communities and HR attribute.
#'   - 'data': return a vertex data file that matches vertices with communities and HR attributes.
#'
#' @import dplyr
#'
#' @export
network_leiden <- function(data, hrvar, res = 0.5, return){

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
                                  directed = TRUE, # Directed, but FALSE for visualization
                                  vertices = unique(vert_ft)) # remove duplicates

  ## Return a numeric vector of partitions / clusters / modules
  ## Set a low resolution parameter to have fewer groups
  ld <- leiden::leiden(g_raw, resolution_parameter = res) # create partitions

  ## Add cluster
  g <-
    g_raw %>%
    # Add leiden partitions to graph object
    igraph::set_vertex_attr("cluster", value = as.character(ld))

  ## Create vertex table
  vertex_tb <-
    g %>%
    igraph::get.vertex.attribute() %>%
    as_tibble()

  ## Return
  if(return == "plot-leiden"){
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
           subtitle = "Based on Leiden algorithm and Strong Tie Score",
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

    vertex_tb %>%
      count(!!sym(hrvar), cluster)

  } else if(return == "data"){

    vertex_tb

  } else if(return == "plot-sankey"){

    create_sankey(data = vertex_tb %>% count(!!sym(hrvar), cluster),
                  var1 = hrvar,
                  var2 = "cluster",
                  count = "n")

  } else {

    stop("Please enter a valid input for `return`.")

  }
}
