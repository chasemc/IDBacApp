
old_db_path <- normalizePath(tempfile(),
                             winslash = "/",
                             mustWork = FALSE)

idbac_create(basename(old_db_path), 
             dirname(old_db_path))
old_db <- idbac_connect(basename(old_db_path), 
                        dirname(old_db_path))[[1]]

path <- .db_path_from_pool(old_db)

test_that(".db_path_from_pool() works", {
  expect_equal(path, 
               paste0(old_db_path, ".sqlite"))
})
