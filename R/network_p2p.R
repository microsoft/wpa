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
#' Other valid inputs are "plot", "network" and "table". "network" returns the `network`
#' object used to generate the network plot. The "pdf" option is highly recommended over the "plot"
#' option as the PDF export format has significantly higher performance. The "plot" option is highly
#' computationally intensive and is not generally recommended.
#' @param path File path for saving the PDF output. Defaults to "network_p2p".
#' @param bg_fill String to specify background fill colour.
#' @param font_col String to specify font and link colour.
#' @param legend_pos String to specify position of legend. Defaults to "bottom". See `ggplot2::theme()`.
#' @param palette Function for generating a colour palette with a single argument `n`. Uses `rainbow()` by default.
#' @param ... Additional arguments to pass to `GGally::ggnet2()`.
#' For instance, you may specify the argument `mode` to change the node placement algorithm.
#'
#' @details
#' For specifying the node placement algorithm, please see the `gplot.layout` documentation for the **sna**
#' package, which provides a full list of the valid functions that can be passed into the `mode` argument.
#'
#' @import ggplot2
#' @import dplyr
#' @import network
#'
#' @export
network_p2p <- function(data,
                        hrvar,
                        return = "pdf",
                        path = "network_p2p",
                        bg_fill = "#000000",
                        font_col = "#FFFFFF",
                        legend_pos = "bottom",
                        palette = rainbow,
                        ...){

  data <-
    data %>%
    filter(StrongTieType > 0)

  tieorigin_var <- paste0("TieOrigin_", hrvar)
  tiedestin_var <- paste0("TieDestination_", hrvar)


  ## Extract edge list
  strong_edgelist <-
    data %>%
    select(TieOrigin_PersonId, TieDestination_PersonId)

  ## Create basic network object
  mynet <-
    network::network(strong_edgelist, matrix.type="edgelist")

  ## Extract list of nodes from network object
  myedges <- data.frame(PersonId = network.vertex.names(mynet))

  ## Extract attributes
  totalAttributes <-
    bind_rows(select(data, PersonId = "TieOrigin_PersonId", !!sym(hrvar) := tieorigin_var),
              select(data, PersonId = "TieOrigin_PersonId", !!sym(hrvar) := tiedestin_var)) %>%
    unique()

  ## Merge list of nodes and attributes
  myedges <- merge(myedges, totalAttributes, by = "PersonId", all.x = TRUE)

  ## Add attributes to network object
  mynet %v% hrvar <- myedges[[hrvar]]

  ## Palette

  pal <- tibble(!!sym(hrvar) := unique(myedges[[hrvar]]))

  col_pal <- do.call(what = palette, args = list(nrow(pal)))

  ## named character vector
  pal <-
    pal %>%
    mutate(Colours = col_pal) %>%
    tibble::deframe()

  if(return == "table"){

    data

  } else if(return == "network"){

    mynet

  } else if(return %in% c("plot", "pdf")){

    outputPlot <-
      mynet %>%
      GGally::ggnet2(size = 1,
                     color = hrvar,
                     label = FALSE,
                     edge.size = .01,
                     edge.alpha = .1,
                     node.alpha =  .8,
                     palette = pal,
                     ...) +
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
