find_repo_root <- function() {
  candidates <- unique(normalizePath(
    c(getwd(), file.path(getwd(), ".."), file.path(getwd(), "..", "..")),
    winslash = "/",
    mustWork = FALSE
  ))

  for (candidate in candidates) {
    if (file.exists(file.path(candidate, "inst", "webr", "openspecy_engine.R"))) {
      return(candidate)
    }
  }

  stop("Could not find repository root.", call. = FALSE)
}

repo_root <- find_repo_root()
source(file.path(repo_root, "inst", "webr", "openspecy_engine.R"))

testthat::test_that("static webR library allowlist is intentionally small", {
  testthat::expect_equal(openspecy_available_libraries(), c("medoid", "model"))
  testthat::expect_equal(openspecy_validate_browser_library("medoid"), "medoid")
  testthat::expect_equal(openspecy_validate_browser_library("model"), "model")
})

testthat::test_that("static webR wrapper rejects full library requests", {
  testthat::expect_error(
    openspecy_validate_browser_library("full"),
    "Unsupported browser reference library"
  )
  testthat::expect_error(
    openspecy_load_browser_library("derivative"),
    "Unsupported browser reference library"
  )
})

testthat::test_that("static webR library paths are fixed browser mounts", {
  testthat::expect_equal(
    openspecy_browser_library_path("medoid"),
    "/data/openspecy-medoid.rds"
  )
  testthat::expect_equal(
    openspecy_browser_library_path("model"),
    "/data/openspecy-model.rds"
  )
})
