find_repo_root <- function() {
  candidates <- unique(normalizePath(
    c(getwd(), file.path(getwd(), ".."), file.path(getwd(), "..", "..")),
    winslash = "/",
    mustWork = FALSE
  ))

  for (candidate in candidates) {
    if (file.exists(file.path(candidate, "R", "runtime.R"))) {
      return(candidate)
    }
  }

  stop("Could not find repository root.", call. = FALSE)
}

repo_root <- find_repo_root()
source(file.path(repo_root, "R", "runtime.R"))
source(file.path(repo_root, "R", "mod_fast_file.R"))

testthat::test_that("local path support is runtime guarded", {
  old <- options(
    OpenSpecy.runtime = "shinylive",
    OpenSpecy.local_file_access = TRUE,
    OpenSpecy.fast_upload = TRUE
  )
  on.exit(options(old), add = TRUE)

  testthat::expect_false(supports_local_paths())
})

testthat::test_that("file handles normalize Shiny uploads", {
  upload <- data.frame(
    name = "spectrum.csv",
    size = 123,
    type = "text/csv",
    datapath = tempfile(fileext = ".csv")
  )

  handle <- make_file_handle(mode = "shiny_upload", upload = upload)

  testthat::expect_s3_class(handle, "openspecy_file_handle")
  testthat::expect_equal(handle$mode, "shiny_upload")
  testthat::expect_equal(handle$name, "spectrum.csv")
  testthat::expect_equal(handle$type, "csv")
  testthat::expect_true(file_handle_supported(handle))
})

testthat::test_that("Shinylive VFS metadata becomes a path-only handle", {
  handle <- make_shinylive_vfs_file_handle(list(list(
    mode = "shinylive_vfs",
    path = "/uploads/spectrum.rds",
    name = "spectrum.rds",
    size = 456,
    type = "rds"
  )))

  testthat::expect_equal(handle$mode, "shinylive_vfs")
  testthat::expect_equal(handle$path, "/uploads/spectrum.rds")
  testthat::expect_equal(handle$type, "rds")
})
