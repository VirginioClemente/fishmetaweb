#' Fitted metaweb model
#'
#' An S4 class holding everything needed to predict trophic interactions among
#' ray-finned fishes: the trait table, the fitted random forest, and the rule
#' used to turn a predicted score into a link. The object is shipped fitted, so
#' no model is estimated when the package is used.
#'
#' @slot traits A data frame with one row per species. The first column,
#'   \code{species}, holds the accepted binomial; the remaining columns are the
#'   predictors, both the FishBase traits and the phylogenetic eigenvectors.
#' @slot forest A list describing the fitted forest, with the integer vectors
#'   \code{left}, \code{right} and \code{feature}, the numeric vectors
#'   \code{threshold} and \code{value}, and the integer vector \code{offset}
#'   giving the first node of each tree. Nodes are stored back to back.
#' @slot predictors Character vector naming the trait columns, in the order the
#'   forest expects them.
#' @slot numeric_predictors Character vector naming the numeric trait columns.
#' @slot categorical_predictors Character vector naming the categorical trait
#'   columns.
#' @slot categories A named list giving, for each categorical predictor, its
#'   levels in the order used by the one-hot encoding.
#' @slot threshold The operational cut applied to a predicted score to call a
#'   link.
#' @slot feature_names Character vector naming the columns of the design matrix
#'   the forest reads, prefixed by the role the species takes in the pair.
#' @slot info A named list of provenance notes.
#'
#' @seealso \code{\link{fish_metaweb}} to load the shipped object,
#'   \code{\link{interaction_probability}} to score a pair, and
#'   \code{\link{build_metaweb}} to assemble a web from a species list.
#' @export
setClass(
  "FishMetaweb",
  representation(
    traits = "data.frame",
    forest = "list",
    predictors = "character",
    numeric_predictors = "character",
    categorical_predictors = "character",
    categories = "list",
    threshold = "numeric",
    feature_names = "character",
    info = "list"
  ),
  validity = function(object) {
    problems <- character(0)
    if (!"species" %in% names(object@traits)) {
      problems <- c(problems, "the trait table needs a 'species' column")
    }
    if (anyDuplicated(object@traits$species)) {
      problems <- c(problems, "species names in the trait table must be unique")
    }
    required <- c("left", "right", "feature", "threshold", "value", "offset")
    if (!all(required %in% names(object@forest))) {
      problems <- c(problems, "the forest is missing one of its components")
    }
    if (length(object@threshold) != 1L || is.na(object@threshold)) {
      problems <- c(problems, "the threshold must be a single number")
    }
    if (length(problems)) problems else TRUE
  }
)

#' @describeIn FishMetaweb Print a short description of the fitted model.
#' @param object A \code{FishMetaweb} object.
#' @export
setMethod("show", "FishMetaweb", function(object) {
  cat("Fitted fish metaweb model\n")
  cat("  species        ", format(nrow(object@traits), big.mark = ","), "\n")
  cat("  predictors     ", length(object@feature_names),
      "(from", length(object@predictors), "traits per species)\n")
  cat("  trees          ", format(length(object@forest$offset) - 1L,
                                  big.mark = ","), "\n")
  cat("  link threshold ", format(object@threshold), "\n")
  if (!is.null(object@info$reference)) {
    cat("  reference      ", object@info$reference, "\n")
  }
  invisible(object)
})
