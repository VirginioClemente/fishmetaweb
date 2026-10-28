#' Assemble a food web for a list of species
#'
#' Scores every ordered pair that can be formed from the species supplied and
#' keeps the pairs whose score reaches the threshold. This is the intended way
#' to obtain a local food web: give the species present in an area, from an
#' occurrence database, a survey programme or a checklist, and the released
#' model returns the fish-fish component of a plausible web for that assemblage.
#'
#' Species the model does not cover are dropped before scoring and reported in a
#' warning. The remaining species are scored as usual, so a list that is partly
#' outside the model still returns a web, built from the species that are
#' inside it.
#'
#' @param species Character vector of species names. Duplicates are ignored.
#' @param threshold Score at or above which a pair is kept. Defaults to the
#'   operational threshold of the released network; raise it for a smaller and
#'   more confident web, lower it to favour completeness over precision.
#' @param self_links Whether to score a species against itself, which the model
#'   treats as intraspecific predation. Defaults to \code{FALSE}.
#' @param as_igraph Whether to return an \code{igraph} object. If \code{FALSE},
#'   or if \pkg{igraph} is not installed, a data frame edge list is returned.
#' @param model A \code{FishMetaweb} object; defaults to the shipped model.
#' @return An \code{igraph} directed graph whose edges run from prey to predator
#'   and carry the predicted score as the edge attribute \code{probability}, or
#'   a data frame with the columns \code{prey}, \code{predator} and
#'   \code{probability}. Species with no retained link are kept as isolated
#'   vertices in the graph.
#' @examples
#' \donttest{
#' assemblage <- c("Engraulis encrasicolus", "Sardina pilchardus",
#'                 "Thunnus thynnus", "Xiphias gladius")
#' web <- build_metaweb(assemblage, as_igraph = FALSE)
#' head(web[order(-web$probability), ])
#' }
#' @export
build_metaweb <- function(species,
                          threshold = link_threshold(model),
                          self_links = FALSE,
                          as_igraph = TRUE,
                          model = fish_metaweb()) {
  species <- unique(.clean_names(species))
  if (!length(species)) {
    stop("'species' is empty, so there is nothing to assemble", call. = FALSE)
  }
  unknown <- setdiff(species, model@traits$species)
  .warn_unknown(unknown, "the assembled web")
  usable <- setdiff(species, unknown)
  if (length(usable) < 2L) {
    stop("fewer than two of the species supplied are covered by the model, ",
         "so no pair can be scored", call. = FALSE)
  }

  grid <- expand.grid(prey = usable, predator = usable,
                      stringsAsFactors = FALSE)
  if (!self_links) grid <- grid[grid$prey != grid$predator, , drop = FALSE]
  grid$probability <- interaction_probability(grid$prey, grid$predator,
                                              model = model)
  edges <- grid[!is.na(grid$probability) & grid$probability >= threshold, ,
                drop = FALSE]
  rownames(edges) <- NULL

  if (!as_igraph) return(edges)
  if (!requireNamespace("igraph", quietly = TRUE)) {
    warning("the 'igraph' package is not installed, so an edge list is ",
            "returned instead of a graph", call. = FALSE)
    return(edges)
  }
  igraph::graph_from_data_frame(edges, directed = TRUE,
                                vertices = data.frame(name = usable))
}
