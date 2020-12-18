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
#' @param bg_fill String to specify background fill colour.
#' @param font_col String to specify font and link colour.
#' @param node_alpha A numeric value between 0 and 1 to specify the transparency of the nodes.
#' @param algorithm String to specify the node placement algorithm to be used. Defaults to "fr" for the force-directed
#' algorithm of Fruchterman and Reingold. See <https://rdrr.io/cran/ggraph/man/layout_tbl_graph_igraph.html> for a
#' full list of options.
#' @param res Resolution parameter to be passed to `leiden::leiden()`. Defaults to 0.5.
#' @param return String specifying what output to return. Valid return options include:
#'   - 'plot-leiden': return a network plot coloured by leiden communities.
#'   - 'plot-hrvar': return a network plot coloured by HR attribute.
#'   - 'plot-sankey': return a sankey plot combining communities and HR attribute.
#'   - 'table': return a vertex summary table with counts in communities and HR attribute.
#'   - 'data': return a vertex data file that matches vertices with communities and HR attributes.
#'   - 'describe': return a list of data frames which describe each of the identified communities.
#'   - 'network': return igraph object.
#'
#' @import dplyr
#'
#' @export
network_leiden <- function(data,
                           hrvar,
                           bg_fill = "#000000",
                           font_col = "#FFFFFF",
                           algorithm = "mds",
                           node_alpha = 0.8,
                           res = 0.5,
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
    igraph::set_vertex_attr("cluster", value = as.character(ld)) %>%
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
  if(return == "plot-leiden"){
    g_layout +
      ggraph::geom_edge_link(colour = "lightgrey", edge_width = 0.01, alpha = 0.15) +
      ggraph::geom_node_point(aes(colour = cluster), alpha = node_alpha) +
      theme_void() +
      theme(legend.position = "bottom",
            legend.background = element_rect(fill = bg_fill),
            plot.background = element_rect(fill = bg_fill),
            text = element_text(colour = font_col),
            axis.line = element_blank()) +
      labs(title = "Person to person collaboration with Community Detection",
           subtitle = "Based on Leiden algorithm and Strong Tie Score",
           y = "",
           x = "")
  } else if(return == "plot-hrvar"){
    g_layout +
      ggraph::geom_edge_link(colour = "lightgrey", edge_width = 0.01, alpha = 0.15) +
      ggraph::geom_node_point(aes(colour = !!sym(hrvar)), alpha = node_alpha) +
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
