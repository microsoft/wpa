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


}





