test_that("the shipped model loads and describes itself", {
  model <- fish_metaweb()
  expect_s4_class(model, "FishMetaweb")
  expect_gt(nrow(model@traits), 30000)
  expect_equal(length(model@forest$offset) - 1L, 1000L)
  expect_true(model@threshold > 0 && model@threshold < 1)
})

test_that("a known pair is scored between zero and one", {
  score <- interaction_probability("Engraulis encrasicolus", "Thunnus thynnus")
  expect_length(score, 1L)
  expect_true(score >= 0 && score <= 1)
})

test_that("the two directions of a pair are scored separately", {
  forward <- interaction_probability("Engraulis encrasicolus", "Thunnus thynnus")
  backward <- interaction_probability("Thunnus thynnus", "Engraulis encrasicolus")
  expect_false(isTRUE(all.equal(forward, backward)))
})

test_that("an unknown species warns and returns NA", {
  expect_warning(score <- interaction_probability("Homo sapiens",
                                                  "Thunnus thynnus"))
  expect_true(is.na(score))
})

test_that("has_species reports coverage", {
  answer <- has_species(c("Thunnus thynnus", "Homo sapiens"))
  expect_true(answer[["Thunnus thynnus"]])
  expect_false(answer[["Homo sapiens"]])
})

test_that("a web is assembled and unknown species are dropped with a warning", {
  assemblage <- c("Engraulis encrasicolus", "Sardina pilchardus",
                  "Thunnus thynnus", "Xiphias gladius")
  edges <- build_metaweb(assemblage, as_igraph = FALSE)
  expect_true(all(c("prey", "predator", "probability") %in% names(edges)))
  expect_true(all(edges$probability >= link_threshold()))
  expect_true(all(edges$prey %in% assemblage))

  expect_warning(
    partial <- build_metaweb(c(assemblage, "Homo sapiens"), as_igraph = FALSE)
  )
  expect_equal(nrow(partial), nrow(edges))
})

test_that("a stricter threshold gives a smaller web", {
  assemblage <- c("Engraulis encrasicolus", "Sardina pilchardus",
                  "Thunnus thynnus", "Xiphias gladius")
  wide <- build_metaweb(assemblage, as_igraph = FALSE)
  narrow <- build_metaweb(assemblage, threshold = 0.9, as_igraph = FALSE)
  expect_lte(nrow(narrow), nrow(wide))
})

test_that("an igraph object is returned when igraph is available", {
  skip_if_not_installed("igraph")
  assemblage <- c("Engraulis encrasicolus", "Sardina pilchardus",
                  "Thunnus thynnus", "Xiphias gladius")
  web <- build_metaweb(assemblage)
  expect_s3_class(web, "igraph")
  expect_equal(igraph::vcount(web), length(assemblage))
  expect_true(igraph::is_directed(web))
})
