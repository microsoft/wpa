# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Create a network plot with the person-to-person query
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#'
#' Analyse a person-to-person (P2P) network query, with multiple visualisation
#' and analysis output options. Pass a data frame containing a person-to-person
#' query and return a network visualization. Options are available for community
#' detection using either the Louvain or the Leiden algorithms.
#'
#'
#'
#' @family Network
#'
#' @export
network_p2p_test <- function(
    data,
    hrvar,
    centrality = NULL,
    community = NULL,
    return,
    bg_fill = "#FFFFFF",
    font_col = "grey20",
    legend_pos = "bottom",
    palette = "rainbow",
    node_alpha = 0.7,
    edge_alpha = 1,
    res = 0.5,
    seed = 1,
    algorithm = "mds",
    size_threshold = 5000,
    weight = "StrongTieScore"
    ){

  ## valid values for centrality -------------------------------------------

  valid_cen <- c(
    "betweenness",
    "closeness",
    "degree",
    "eigenvector",
    "pagerank"
  )

  ## valid values for community --------------------------------------------

  valid_com <- c(
    "leiden",
    "louvain"
  )

  ## Set data frame for `edges` --------------------------------------------

  if(is.null(weight)){

    edges <-
      data %>%
      mutate(NoWeight = 1) %>% # No weight
      select(from = "TieOrigin_PersonId",
             to = "TieDestination_PersonId",
             weight = "NoWeight")

  } else {

    edges <-
      data %>%
      select(from = "TieOrigin_PersonId",
             to = "TieDestination_PersonId",
             weight = weight)

  }

  ## Set variables ---------------------------------------------------------

  TO_hrvar <- paste0("TieOrigin_", hrvar)
  TD_hrvar <- paste0("TieDestination_", hrvar)

  ## Vertices data frame to provide meta-data ------------------------------

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

  ## Create 'igraph' object -----------------------------------------------

  g_raw <-
    igraph::graph_from_data_frame(edges,
                                  directed = TRUE, # Directed, but FALSE for visualization
                                  vertices = unique(vert_ft)) # remove duplicates

  ## Assign weights --------------------------------------------------------

  g_raw$weight <- edges$weight

  ## Main algorithm --------------------------------------------------------

  if(is.null(centrality) & is.null(community)){

    # PLOT -> Returns basic plot with HR attribute
    # PDF -> Exports plot as pdf file
    # Table -> HR Var count
    # Data -> Returns person dataset with HR attributes
    # Network -> Returns network object

    g <- g_raw %>% igraph::simplify()

    ## Name of vertex attribute
    v_attr <- hrvar


  } else if(centrality %in% valid_cen & is.null(community)){

    # PLOT -> Returns basic plot with HR attribute AND vertices proportional to centrality
    # PDF -> Exports plot as pdf file
    # Table -> HR Var count and average centrality
    # Data -> Returns person dataset with HR attributes and centrality scores (ALL)
    # Network -> Returns network object with centrality scores (ALL)

  } else if(is.null(centrality) & community %in% valid_com){

    # PLOT -> Returns basic plot with community (no hrvar)
    # PDF -> Exports plot as pdf file
    # Table -> HR Var x community count
    # Data -> Returns person dataset with HR attributes and community attribute
    # Network -> Returns network object with community attribute


    # TODO - modularise louvain and leiden?
    if(community == "louvain"){

      set.seed(seed = seed)

      ## Convert to undirected
      g_ud <- igraph::as.undirected(g_raw)

      ## Return a numeric vector of partitions / clusters / modules
      ## Set a low resolution parameter to have fewer groups
      ## weights = NULL means that if the graph as a `weight` edge attribute, this
      ## will be used by default.
      lc <- igraph::cluster_louvain(g_ud, weights = NULL)

      ## Add cluster
      g <-
        g_ud %>%
        # Add louvain partitions to graph object
        igraph::set_vertex_attr("cluster", value = as.character(igraph::membership(lc))) %>% # Return membership - diff from Leiden
        igraph::simplify()

      ## Name of vertex attribute
      v_attr <- "cluster"

    } else if(community == "leiden"){

      # Check package installation
      check_pkg_installed(pkgname = "leiden")

      ## Return a numeric vector of partitions / clusters / modules
      ## Set a low resolution parameter to have fewer groups
      ld <- leiden::leiden(
        g_raw,
        resolution_parameter = res,
        seed = seed,
        weights = g_raw$weight) # create partitions

      ## Add cluster
      g <-
        g_raw %>%
        # Add leiden partitions to graph object
        igraph::set_vertex_attr("cluster", value = as.character(ld)) %>%
        igraph::simplify()

      ## Name of vertex attribute
      v_attr <- "cluster"

    }


  } else if(centrality %in% valid_cen & community %in% valid_com){

    # PLOT -> Returns basic plot with community AND vertices proportional to centrality
    # PDF -> Exports plot as pdf file
    # Table -> HR Var x community count and average centrality
    # Data -> Returns person dataset with HR attributes, community attribute and centrality scores (ALL)
    # Network -> Returns network object with community attribute and centrality scores (ALL)


  } else {

    stop(
      "Invalid inputs to `centrality` or `community`."
    )

  }


}





