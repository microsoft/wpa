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
#' @inheritParams network_p2p
#'
#' @examples
#' ## Simulate a small person-to-person dataset
#' p2p_data <- p2p_data_sim(size = 50)
#'
#' ## Return louvain, console, plot
#' p2p_data %>%
#'   network_louvain(path = NULL,
#'                   return = "plot")
#'
#' @export
network_louvain <- function(data,
                            hrvar = "Organization",
                            bg_fill = "#000000",
                            font_col = "#FFFFFF",
                            node_alpha = 0.8,
                            algorithm = "mds",
                            path = "network_p2p_louvain",
                            desc_hrvar = c("Organization", "LevelDesignation", "FunctionType"),
                            return = "plot-louvain",
                            size_threshold = 5000){

  ## Default value for display
  display <- "louvain"

  ## Make code backward compatible

  if(grepl(pattern = "plot-", x = return)){

    return <- gsub(pattern = "-",
                   replacement = "",
                   x = return)

    if(return %in% c("louvain", "hrvar")){

      display <- return # Either "louvain" or "hrvar"
      return <- "network"

    }

  }

  ## Wrapper
  network_p2p(data = data,
              hrvar = hrvar,
              display = display,
              return = return,
              path = path,
              desc_hrvar = desc_hrvar,
              bg_fill = bg_fill,
              font_col = font_col,
              node_alpha = node_alpha,
              algorithm = algorithm,
              size_threshold = size_threshold)

}
