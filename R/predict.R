## Internal helpers -----------------------------------------------------------

.metaweb_cache <- new.env(parent = emptyenv())

# Pairs scored per block; see interaction_probability().
.block_size <- 20000L

#' Load the fitted metaweb model
#'
#' Returns the \code{FishMetaweb} object shipped with the package. The object is
#' large, so it is read once and kept for the rest of the session.
#'
#' @return An object of class \code{\linkS4class{FishMetaweb}}.
#' @examples
#' model <- fish_metaweb()
#' model
#' @export
fish_metaweb <- function() {
  if (is.null(.metaweb_cache$model)) {
    environment <- new.env(parent = emptyenv())
    utils::data("fish_metaweb_model", package = "fishmetaweb",
                envir = environment)
    .metaweb_cache$model <- get("fish_metaweb_model", envir = environment)
  }
  .metaweb_cache$model
}

#' The operational link threshold
#'
#' The score above which a pair is called a link in the released network. It is
#' the mean of the per-fold thresholds that maximise the true skill statistic in
#' cross-validation, so it weighs a missed link and a spurious link equally.
#' Users who prefer one kind of error over the other may pass their own cut to
#' \code{\link{build_metaweb}}.
#'
#' @param model A \code{FishMetaweb} object; defaults to the shipped model.
#' @return A single number.
#' @examples
#' link_threshold()
#' @export
link_threshold <- function(model = fish_metaweb()) {
  model@threshold
}

#' Species covered by the model
#'
#' @param model A \code{FishMetaweb} object; defaults to the shipped model.
#' @return A character vector of accepted binomials, sorted alphabetically.
#' @examples
#' head(metaweb_species())
#' @export
metaweb_species <- function(model = fish_metaweb()) {
  sort(model@traits$species)
}

#' Check which species the model knows
#'
#' @param species Character vector of species names.
#' @param model A \code{FishMetaweb} object; defaults to the shipped model.
#' @return A logical vector, named by \code{species}, which is \code{TRUE} where
#'   the species has a row in the trait table.
#' @examples
#' has_species(c("Gadus morhua", "Homo sapiens"))
#' @export
has_species <- function(species, model = fish_metaweb()) {
  species <- .clean_names(species)
  stats::setNames(species %in% model@traits$species, species)
}

## Name handling --------------------------------------------------------------

.clean_names <- function(species) {
  if (!is.character(species)) species <- as.character(species)
  species <- trimws(species)
  gsub("[[:space:]]+", " ", species)
}

.warn_unknown <- function(unknown, context) {
  if (!length(unknown)) return(invisible(NULL))
  shown <- if (length(unknown) > 10L) {
    paste0(paste(unknown[1:10], collapse = ", "), ", and ",
           length(unknown) - 10L, " more")
  } else {
    paste(unknown, collapse = ", ")
  }
  warning(sprintf(
    "%d species %s not in the model and %s been left out of %s: %s",
    length(unknown),
    if (length(unknown) == 1L) "is" else "are",
    if (length(unknown) == 1L) "has" else "have",
    context, shown
  ), call. = FALSE)
  invisible(NULL)
}

## Design matrix --------------------------------------------------------------

.design_matrix <- function(model, prey, predator) {
  rows_prey <- match(prey, model@traits$species)
  rows_predator <- match(predator, model@traits$species)
  numeric_block <- function(rows) {
    values <- vapply(
      model@numeric_predictors,
      function(column) as.numeric(model@traits[[column]][rows]),
      numeric(length(rows))
    )
    # vapply drops to a vector for a single pair, so the shape is imposed here.
    matrix(values, nrow = length(rows),
           ncol = length(model@numeric_predictors))
  }
  onehot_block <- function(rows) {
    blocks <- lapply(model@categorical_predictors, function(column) {
      levels_here <- model@categories[[column]]
      observed <- as.character(model@traits[[column]][rows])
      indicator <- matrix(0, nrow = length(rows), ncol = length(levels_here))
      position <- match(observed, levels_here)
      known <- !is.na(position)
      indicator[cbind(which(known), position[known])] <- 1
      indicator
    })
    do.call(cbind, blocks)
  }
  cbind(numeric_block(rows_prey), numeric_block(rows_predator),
        onehot_block(rows_prey), onehot_block(rows_predator))
}

## Forest traversal -----------------------------------------------------------

.forest_score <- function(model, design) {
  forest <- model@forest
  offset <- forest$offset
  n_trees <- length(offset) - 1L
  total <- numeric(nrow(design))
  for (tree in seq_len(n_trees)) {
    base <- offset[tree]
    node <- rep.int(1L, nrow(design))
    active <- seq_len(nrow(design))
    while (length(active)) {
      here <- base + node[active]
      feature <- forest$feature[here]
      leaf <- feature < 0L
      if (any(leaf)) {
        finished <- active[leaf]
        total[finished] <- total[finished] + forest$value[here[leaf]]
        keep <- !leaf
        active <- active[keep]
        here <- here[keep]
        feature <- feature[keep]
        if (!length(active)) break
      }
      goes_left <- design[cbind(active, feature + 1L)] <= forest$threshold[here]
      node[active] <- ifelse(goes_left, forest$left[here], forest$right[here]) + 1L
    }
  }
  total / n_trees
}

#' Probability that one species feeds on another
#'
#' Scores one or more ordered pairs with the fitted model. The pair is ordered:
#' the first species is the prey and the second the predator, and the two
#' directions of a pair are scored separately.
#'
#' Species the model does not cover cannot be scored, because the predictors are
#' not available for them. Such pairs return \code{NA} and raise a warning
#' naming the species.
#'
#' @param prey Character vector of prey species names.
#' @param predator Character vector of predator species names. Recycled against
#'   \code{prey} if one of the two has length one.
#' @param model A \code{FishMetaweb} object; defaults to the shipped model.
#' @return A numeric vector of probabilities between 0 and 1, one per pair, with
#'   \code{NA} where either species is unknown to the model.
#' @examples
#' interaction_probability("Engraulis encrasicolus", "Thunnus thynnus")
#' interaction_probability("Thunnus thynnus", "Engraulis encrasicolus")
#' @export
interaction_probability <- function(prey, predator, model = fish_metaweb()) {
  prey <- .clean_names(prey)
  predator <- .clean_names(predator)
  if (length(prey) != length(predator)) {
    if (length(prey) == 1L) prey <- rep(prey, length(predator))
    else if (length(predator) == 1L) predator <- rep(predator, length(prey))
    else stop("'prey' and 'predator' must have the same length, ",
              "unless one of them has length one", call. = FALSE)
  }
  if (!length(prey)) return(numeric(0))
  known <- model@traits$species
  unknown <- setdiff(unique(c(prey, predator)), known)
  .warn_unknown(unknown, "the scored pairs")
  scores <- rep(NA_real_, length(prey))
  usable <- which(prey %in% known & predator %in% known)
  # The design matrix holds 182 columns per pair, so long inputs are scored in
  # blocks to keep the memory footprint flat.
  for (block in split(usable, ceiling(seq_along(usable) / .block_size))) {
    design <- .design_matrix(model, prey[block], predator[block])
    scores[block] <- .forest_score(model, design)
  }
  scores
}
