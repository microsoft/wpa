# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Create a network plot with the person-to-person query
#'
#' @description
#' Pass a data frame containing a person-to-person query and save a network
#' plot as a PDF file.
#'
#' @param data Data frame containing a person-to-person query.
#' @param hrvar String containing the label for the HR attribute.
#' @param return Character vector specifying what to return, defaults to "pdf".
#' Valid inputs are:
#'   - "pdf": saves the network plot as a PDF in the specified path. See `path`. This is the recommended
#'   output format as large networks can be slow in other formats.
#'   - "plot": returns a ggplot object of the network plot. It is not recommended that you run this without
#'   assigning the output to an object as plotting to the R console can be slow for large networks.
#'   - "table": returns the edgelist data frame used in the network.
#'   - "network": returns the igraph object used to create the network plot.
#' @param path File path for saving the PDF output. Defaults to "network_p2p".
#' @param bg_fill String to specify background fill colour.
#' @param font_col String to specify font and link colour.
#' @param legend_pos String to specify position of legend. Defaults to "bottom". See `ggplot2::theme()`.
#' @param palette Function for generating a colour palette with a single argument `n`. Uses "rainbow" by default.
#' @param node_alpha A numeric value between 0 and 1 to specify the transparency of the nodes.
#' @param algorithm String to specify the node placement algorithm to be used. Defaults to "fr" for the force-directed
#' algorithm of Fruchterman and Reingold. See <https://rdrr.io/cran/ggraph/man/layout_tbl_graph_igraph.html> for a
#' full list of options.
#'
#' @examples
#' ## Simulate simple P2P network
#' sim_net <-
#'   data.frame(TieOrigin_PersonId = sample(seq(1, 100), size = 100, replace = TRUE),
#'              TieDestination_PersonId = sample(seq(1, 100), size = 100, replace = TRUE)) %>%
#'   dplyr::mutate(TieOrigin_Organization = ifelse(TieOrigin_PersonId >= 50, "A", "B"),
#'                 TieDestination_Organization = ifelse(TieDestination_PersonId >= 50, "A", "B"),
#'                 StrongTieScore = rep(1, 100))
#'
#' ## Run plot
#' ## ONLY return 'plot' instead of 'pdf' when data size is small
#' network_p2p(data = sim_net,
#'             hrvar = "Organization",
#'             return = "plot")
#'
#' @import ggplot2
#' @import dplyr
#' @importFrom grDevices rainbow
#'
#' @export
network_p2p <- function(data,
                        hrvar,
                        return = "pdf",
                        path = "network_p2p",
                        bg_fill = "#000000",
                        font_col = "#FFFFFF",
                        legend_pos = "bottom",
                        palette = "rainbow",
                        node_alpha = 0.7,
                        algorithm = "fr"){

  ## No filtering
  tieorigin_var <- paste0("TieOrigin_", hrvar)
  tiedestin_var <- paste0("TieDestination_", hrvar)

  ## Set edges df
  edges <-
    data %>%
    select(from = "TieOrigin_PersonId",
           to = "TieDestination_PersonId",
           weight = "StrongTieScore") %>%
    select(-weight) # Overwrite - no info on edge

  ## Vertices data frame to provide meta-data
  vert_ft <-
    rbind(
      # TieOrigin
      edges %>%
        left_join(select(data, TieOrigin_PersonId, tieorigin_var),
                  by = c("from"  = "TieOrigin_PersonId")) %>%
        select(node = "from", !!sym(hrvar) := tieorigin_var),

      # TieDestination
      edges %>%
        left_join(select(data, TieDestination_PersonId, tiedestin_var),
                  by = c("to"  = "TieDestination_PersonId")) %>%
        select(node = "to", !!sym(hrvar) := tiedestin_var)
    )

  ## Create igraph object
  g <-
    igraph::graph_from_data_frame(edges,
                                  directed = FALSE, # Directed, but FALSE for visualization
                                  vertices = unique(vert_ft)) %>% # remove duplicates
    igraph::simplify()

  ## Palette
  ## Create tibble
  pal <-
    tibble(!!sym(hrvar) := g %>%
             igraph::get.vertex.attribute(hrvar) %>%
             unique())

  ## Apply palette function
  col_pal <- do.call(what = palette, args = list(nrow(pal)))

  ## named character vector
  pal <-
    pal %>%
    mutate(Colours = col_pal) %>%
    tibble::deframe()

  if(return == "table"){

    edges

  } else if(return == "network"){

    g

  } else if(return %in% c("plot", "pdf")){

    outputPlot <-
      g %>%
      ggraph::ggraph(layout = "igraph", algorithm = algorithm) +
      ggraph::geom_edge_link(colour = "lightgrey", edge_width = 0.01, alpha = 0.15) +
      ggraph::geom_node_point(aes(colour = !!sym(hrvar)), alpha = node_alpha) +
      scale_colour_discrete(type = pal) +
      theme_void() +
      theme(legend.position = legend_pos,
            legend.background = element_rect(fill = bg_fill),
            plot.background = element_rect(fill = bg_fill),
            text = element_text(colour = font_col),
            axis.line = element_blank()) +
      labs(y = "",
           x = "")

    ## Inner conditional
    if(return == "pdf"){

      fn <- paste0(path, "_", tstamp(), ".pdf")
      ggsave(filename = fn,
             plot = outputPlot,
             width = 12,
             height = 9)


    } else if(return == "plot"){

      outputPlot

    }

  } else {

    stop("Please enter a valid input for `return`.")

  }
}
