#' Merge all samples from IDBac sqlite databses
#'
#' @param existing_db_paths paths to exisitng IDBac experiments/sqlite files
#' @param new_db_name new experiment/sqlite database name
#' @param new_db_path new experiment/sqlite database path
#'
#' @export
#'
merge_idbac_experiments <- function(existing_db_paths,
                                    new_db_name,
                                    new_db_path) {
  if (!dir.exists(new_db_path)) {
    dir.create(new_db_path)
  }
  if (!dir.exists(new_db_path)) {
    stop("Was unable to create new directory")
  }
  if (!all(file.exists(existing_db_paths))) {
    stop("At least one given merge_idbac_experiments('existing_db_paths') wasn't found.")
  }
  existing_db_paths_names <- tools::file_path_sans_ext(basename(existing_db_paths))

  for (i in seq_along(existing_db_paths_names)) {
    # newPool is here because IDBacApp::copyToNewDatabase() closes the pool
    new_pool <- IDBacApp::idbac_connect(
      new_db_name,
      new_db_path
    )[[1]]
    existing_pool <- IDBacApp::idbac_connect(
      existing_db_paths_names[[i]],
      dirname(existing_db_paths[[i]])
    )[[1]]
    con <- pool::poolCheckout(existing_pool)
    samp_names <- IDBacApp::idbac_available_samples(con, type = "all")
    pool::poolReturn(con)
    copyToNewDatabase(
      existingDBPool = existing_pool,
      newDBPool = new_pool,
      sampleIDs = samp_names
    )
    cat(existing_db_paths_names[[i]], ";\n", i, "of", length(existing_db_paths_names), "\n")
  }
}
