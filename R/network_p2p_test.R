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
    centrality = FALSE,
    community = FALSE,
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

  ## Main algorithm --------------------------------------------------------

  if(centrality == FALSE & community == FALSE){

    # PLOT -> Returns basic plot with HR attribute
    # PDF -> Exports plot as pdf file
    # Table -> HR Var count
    # Data -> Returns person dataset with HR attributes
    # Network -> Returns network object


  } else if(centrality %in% valid_cen & community == FALSE){

    # PLOT -> Returns basic plot with HR attribute AND vertices proportional to centrality
    # PDF -> Exports plot as pdf file
    # Table -> HR Var count and average centrality
    # Data -> Returns person dataset with HR attributes and centrality scores (ALL)
    # Network -> Returns network object with centrality scores (ALL)

  } else if(centrality == FALSE & community %in% valid_com){

    # PLOT -> Returns basic plot with community (no hrvar)
    # PDF -> Exports plot as pdf file
    # Table -> HR Var x community count
    # Data -> Returns person dataset with HR attributes and community attribute
    # Network -> Returns network object with community attribute


  } else if(centrality %in% valid_cen & community %in% valid_com){

    # PLOT -> Returns basic plot with community AND vertices proportional to centrality
    # PDF -> Exports plot as pdf file
    # Table -> HR Var x community count and average centrality
    # Data -> Returns person dataset with HR attributes, community attribute and centrality scores (ALL)
    # Network -> Returns network object with community attribute and centrality scores (ALL)


  } else {

    stop(
      "Invalid inputs to `centrality` or `community`".
    )

  }


}





