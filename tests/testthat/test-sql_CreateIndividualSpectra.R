#
# a <- system.file("extdata/sqlite", package = "IDBacApp")
# z1 <- createPool("sample",
#                            a)[[1]]
# z1 <- pool::poolCheckout(z1)
#
#
#
# a2 <- tempfile()
# z2 <- createPool(basename(a2),
#                            dirname(a2))[[1]]
# z2 <- pool::poolCheckout(z2)
#
#
#
#
# DBI::dbListTables(z2)
#
#
# test_that("new DB is empty", {
#   expect_equal(character(0),
#                DBI::dbListTables(z2))
# })
#
#
# sql_create_spectra_table(z2)
# a <- DBI::dbListFields(z2, "spectra")
#
# test_that("new DB has IndividualSpectra table", {
#   expect_equal("spectra",
#                DBI::dbListTables(z2))
#
#   expect_identical("spectrum_mass_hashspectrum_intensity_hashxml_hashstrain_idpeak_matrixspectrum_intensitymax_massmin_massignorenumbertime_delaytime_deltacalibration_constantsv1_tof_calibrationdata_typedata_systemspectrometer_typeinletionization_modeacquisition_methodacquisition_dateacquisition_modetof_modeacquisition_operator_modelaser_attenuationdigitizer_typeflex_control_versionidinstrumentinstrument_idinstrument_typemass_errorlaser_shotspatchpathlaser_repetitionspotspectrum_typetarget_counttarget_id_stringtarget_serial_numbertarget_type_number",
#                    paste0(a, collapse = ""))
#
# })
#
