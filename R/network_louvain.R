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
#' @param bg_fill String to specify background fill colour.
#' @param font_col String to specify font and link colour.
#' @param node_alpha A numeric value between 0 and 1 to specify the transparency of the nodes.
#' @param algorithm String to specify the node placement algorithm to be used. Defaults to "mds" to perform
#' a multidimensional scaling of nodes using a shortest path, which is also a deterministic method.
#' See <https://rdrr.io/cran/ggraph/man/layout_tbl_graph_igraph.html> for a full list of options.
#' @param path File path for saving the PDF output. Defaults to "network_p2p_louvain".
#' Since the network outputs are computationally intensive, the default behaviour is to save time by
#' saving the plot output directly as a PDF in the specified path. To override this behaviour and return
#' a plot object instead, you can pass `NULL` to `path`. What is passed to `path` makes no difference
#' if returning anything other than "plot-louvain" or "plot-hrvar".
#' @param desc_hrvar Character vector of length 3 containing the HR attributes to use when returning the
#' "describe" output. See `network_describe()`.
#'
#' @param return String specifying what output to return.Valid return options include:
#'   - 'plot-louvain': return a network plot coloured by louvain communities, saving a PDF to `path`.
#'   - 'plot-hrvar': return a network plot coloured by HR attribute, saving a PDF to `path`.
#'   - 'plot-sankey': return a sankey plot combining communities and HR attribute.
#'   - 'table': return a vertex summary table with counts in communities and HR attribute.
#'   - 'data': return a vertex data file that matches vertices with communities and HR attributes.
#'   - 'describe': returns a list of data frames which describe each of the identified communities.
#'   - 'network': return igraph object.
#'
#' @import ggraph
#' @import dplyr
#'
#' @export
network_louvain <- function(data,
                            hrvar,
                            bg_fill = "#000000",
                            font_col = "#FFFFFF",
                            node_alpha = 0.8,
                            algorithm = "mds",
                            path = "network_p2p_louvain",
                            desc_hrvar = c("Organization", "LevelDesignation", "FunctionType"),
                            return){

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
        select(from) %>% # Single column
        unique() %>% # Remove duplications
        left_join(select(data, TieOrigin_PersonId, TO_hrvar),
                  by = c("from"  = "TieOrigin_PersonId")) %>%
        select(node = "from", !!sym(hrvar) := TO_hrvar),

      # TieDestination
      edges %>%
        select(to) %>% # Single column
        unique() %>% # Remove duplications
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
    igraph::set_vertex_attr("cluster", value = as.character(igraph::membership(lc))) %>% # Return membership - diff from Leiden
    igraph::simplify()

  ## Create vertex table
  vertex_tb <-
    g %>%
    igraph::get.vertex.attribute() %>%
    as_tibble()

  g_layout <-
    g %>%
    ggraph::ggraph(layout = "igraph", algorithm = algorithm)

  ## Return
  if(return == "plot-louvain"){

    plot_output <-
      g_layout +
      ggraph::geom_edge_link(colour = "lightgrey", edge_width = 0.01, alpha = 0.15) +
      ggraph::geom_node_point(aes(colour = cluster),
                              alpha = node_alpha,
                              pch = 16) +
      theme_void() +
      theme(legend.position = "bottom",
            legend.background = element_rect(fill = bg_fill),
            plot.background = element_rect(fill = bg_fill),
            text = element_text(colour = font_col),
            axis.line = element_blank()) +
      labs(title = "Person to person collaboration with Community Detection",
           subtitle = "Based on Louvain algorithm and Strong Tie Score",
           y = "",
           x = "")

    # Default PDF output unless NULL supplied to path
    if(is.null(path)){

      plot_output

    } else {

      ggsave(paste0(path, tstamp(), ".pdf"),
             plot = plot_output,
             width = 16,
             height = 9)

    }

  } else if(return == "plot-hrvar"){

    plot_output <-
      g_layout +
      ggraph::geom_edge_link(colour = "lightgrey", edge_width = 0.01, alpha = 0.15) +
      ggraph::geom_node_point(aes(colour = !!sym(hrvar)),
                              alpha = node_alpha,
                              pch = 16) +
      theme_void() +
      theme(legend.position = "bottom",
            legend.background = element_rect(fill = bg_fill),
            plot.background = element_rect(fill = bg_fill),
            text = element_text(colour = font_col),
            axis.line = element_blank()) +
      labs(title = "Person to person collaboration",
           subtitle = paste0("Showing ", hrvar),
           y = "",
           x = "")

    # Default PDF output unless NULL supplied to path
    if(is.null(path)){

      plot_output

    } else {

      ggsave(paste0(path, tstamp(), ".pdf"),
             plot = plot_output,
             width = 16,
             height = 9)

    }

  } else if(return == "table"){

    vertex_tb %>%
      count(!!sym(hrvar), cluster)

  } else if(return == "data"){

    vertex_tb

  } else if(return == "network"){

    g


  } else if(return == "plot-sankey"){

    create_sankey(data = vertex_tb %>% count(!!sym(hrvar), cluster),
                  var1 = hrvar,
                  var2 = "cluster",
                  count = "n")

  } else if(return == "describe"){

    describe_tb <-
      vertex_tb %>%
      left_join(select(data, starts_with("TieOrigin_")),
                by = c("name" = "TieOrigin_PersonId"))

    desc_str <-
      describe_tb %>%
      pull(cluster) %>%
      unique()

    desc_str %>%
      purrr::map(function(x){
        describe_tb %>%
          filter(cluster == x) %>%
          network_describe(hrvar = desc_hrvar)
      }) %>%
      setNames(nm = desc_str)

  } else {

    stop("Please enter a valid input for `return`.")

  }
}
