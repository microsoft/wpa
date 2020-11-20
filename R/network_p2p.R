# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Create a network plot with the person-to-person query
#'
#' @description
#' Pass a data frame containing a person-to-person query and return a network
#' plot.
#' Automatically handles "Collaborators_within_group" and "Other_collaborators" within query data.
#'
#' @param data Data frame containing a person-to-person query.
#' @param hrvar String containing the label for the HR attribute.
#' @param return Character vector specifying what to return, defaults to "plot".
#' Valid inputs are "plot", "network" and "table". "network" returns the `network`
#' object used to generate the network plot.
#' @param bg_fill String to specify background fill colour.
#' @param font_col String to specify font colour
#' @param legend_pos String to specify position of legend. Defaults to "bottom". See `ggplot2::theme()`.
#' @param ... Additional arguments to pass to `GGally::ggnet2()`.
#' For instance, you may specify the argument `mode` to change the node placement algorithm.
#'
#'
#' @import ggplot2
#' @import dplyr
#' @import network
#'
#' @export
network_p2p <- function(data,
                        hrvar,
                        return = "plot",
                        bg_fill = "#000000",
                        font_col = "#FFFFFF",
                        legend_pos = "bottom",
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
  pal <-
    tibble(!!sym(hrvar) := unique(myedges[[hrvar]])) %>%
    mutate(Colours = rainbow(nrow(.), alpha = 1)) %>%
    tibble::deframe()

  if(return == "table"){

    data

  } else if(return == "network"){

    mynet

  } else if(return == "plot"){

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


  } else {

    stop("Please enter a valid input for `return`.")

  }
}
