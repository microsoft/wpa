# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Implement the Leiden community detection on a Person to Person network query
#'
#' @description
#' `r lifecycle::badge('experimental')`
#' Take a P2P network query and implement the Leiden community detection method. To run
#' this function, you will require all the pre-requisites of the **leiden** package installed,
#' which includes Python and **reticulate**.
#'
#' @inheritParams network_p2p
#'
#' @param return
#' String specifying what output to return. Defaults to "plot-leiden". Valid return options include:
#'   - 'plot-leiden': return a network plot coloured by leiden communities, saving a PDF to path.
#'   - 'plot-hrvar': return a network plot coloured by HR attribute, saving a PDF to path.
#'   - 'plot-sankey': return a sankey plot combining communities and HR attribute.
#'   - 'table': return a vertex summary table with counts in communities and HR attribute.
#'   - 'data': return a vertex data file that matches vertices with communities and HR attributes.
#'   - 'describe': return a list of data frames which describe each of the identified communities.
#'     The first data frame is a summary table of all the communities.
#'   - 'network': return igraph object.
#'
#' @examples
#' \donttest{
#' # Simulate a small person-to-person dataset
#' p2p_data <- p2p_data_sim(size = 50)
#'
#' # Return leiden, console, plot
#' p2p_data %>%
#'   network_leiden(path = NULL,
#'                  return = "plot")
#' }
#'
#'
#' @export
network_leiden <- function(data,
                           hrvar = "Organization",
                           bg_fill = "#000000",
                           font_col = "#FFFFFF",
                           algorithm = "mds",
                           path = "network_p2p_leiden",
                           node_alpha = 0.8,
                           res = 0.5,
                           seed = 1,
                           desc_hrvar = c("Organization", "LevelDesignation", "FunctionType"),
                           return = "plot-leiden",
                           size_threshold = 5000){

  ## Default value for display
  display <- "leiden"

  ## Make code backward compatible

  if(grepl(pattern = "plot-", x = return)){

    return <- gsub(pattern = "plot-",
                   replacement = "",
                   x = return)

    if(return %in% c("leiden", "hrvar")){

      display <- return # Either "leiden" or "hrvar"
      return <- "plot"

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
              res = res, # Leiden specific
              seed = seed, # Leiden specific
              algorithm = algorithm,
              size_threshold = size_threshold)

}
