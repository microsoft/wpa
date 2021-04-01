#' @title Simulate a person-to-person query using a Watts-Strogatz model
#'
#' @description Generate an person-to-person query / edgelist based on the graph
#'   according to the Watts-Strogatz small-world network model. Organizational
#'   data fields are also simulated for `Organization`, `LevelDesignation`, and
#'   `City`.
#'
#' @param dim Integer constant, the dimension of the starting lattice.
#' @param size Integer constant, the size of the lattice along each dimension.
#' @param nei Integer constant, the neighborhood within which the vertices of
#'  the lattice will be connected.
#' @param p Real constant between zero and one, the rewiring probability.
#'
#' @details
#' This is a wrapper around `igraph::watts.strogatz.game()`. See igraph
#' documentation for details on methodology. Loop edges and multiple edges are
#' disabled. Size of the network can be changing the arguments `size` and `nei`.
#'
#' @examples
#' # Simulate a p2p dataset with 800 edges
#' p2p_data_sim(size = 200, nei = 4)
#'
#' @return
#' data frame with the same column structure as a person-to-person flexible
#' query. This has an edgelist structure and can be used directly as an input
#' to `network_p2p()`.
#'
#' @family Data
#' @family Network
#'
#' @export
p2p_data_sim <- function(dim = 1,
                         size = 300,
                         nei = 5,
                         p = 0.05){

  igraph::watts.strogatz.game(dim = dim,
                              size = size,
                              nei = nei,
                              p = p) %>%
    igraph::as_edgelist() %>%
    as.data.frame() %>%
    dplyr::rename(TieOrigin_PersonId = "V1",
           TieDestination_PersonId = "V2") %>%
    dplyr::mutate(TieOrigin_Organization = add_cat(TieOrigin_PersonId, "Organization"),
           TieDestination_Organization = add_cat(TieDestination_PersonId, "Organization"),
           TieOrigin_LevelDesignation = add_cat(TieOrigin_PersonId, "LevelDesignation"),
           TieDestination_LevelDesignation = add_cat(TieDestination_PersonId, "LevelDesignation"),
           TieOrigin_City = add_cat(TieOrigin_PersonId, "City"),
           TieDestination_City = add_cat(TieDestination_PersonId, "City")) %>%
    dplyr::mutate_at(dplyr::vars(dplyr::ends_with("PersonId")),
                     ~paste0("SIM_ID_", .)) %>%
    dplyr::mutate(StrongTieScore = 1)
}

#' Add organizational data to the simulated p2p data
#' @param x Numeric vector to be assigned simulated organizational attributes
#' @param type String with three valid options:
#'   - `Organization`
#'   - `LevelDesignation`
#'   - `City`
add_cat <- function(x, type){

  if(type == "Organization"){

    dplyr::case_when((x %% 7 == 0) ~ "Org A",
                     (x %% 6 == 0) ~ "Org B",
                     (x %% 5 == 0) ~ "Org C",
                     (x %% 4 == 0) ~ "Org D",
                     (x %% 3 == 0) ~ "Org E",
                     x < 100 ~ "Org F",
                     (x %% 2 == 0) ~ "Org G", # Even number
                     TRUE ~ "Org H")

  } else if(type == "LevelDesignation"){

    paste("Level", substr(x, 1, 1)) # Extract first digit

  } else if(type == "City"){

    dplyr::case_when((x %% 3 == 0) ~ "City A", # Divisible by 3
                     (x %% 2 == 0) ~ "City B",
                     TRUE ~ "City C")

  }
}
