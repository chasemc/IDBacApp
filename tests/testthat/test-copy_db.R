
old_db_path <- normalizePath(tempfile(),
                   winslash = "/",
                   mustWork = FALSE)

idbac_create(basename(old_db_path), 
             dirname(old_db_path))
old_db <- idbac_connect(basename(old_db_path), 
                   dirname(old_db_path))[[1]]



new_db_path <- .copy_db(pool = old_db,
                        latest_db_version = "1.1.2")

test_that(".copy_db() works", {
  expect_equal(new_db_path, 
               paste0(old_db_path,
                      "_db-1_1_2.sqlite"))
  expect_true(file.exists(new_db_path))
  
})
