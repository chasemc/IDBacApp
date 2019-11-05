test_that("brukerToMzml_popup works outside shiny", {
  expect_message(brukerToMzml_popup(), "IDBac is converting your Bruker files to open-source mzML,
during this step there is no progress bar. After this step IDBac
will begin to convert your files into an IDBac experiment.")
})
