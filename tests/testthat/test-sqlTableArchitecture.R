a <- tempdir()
a <- idbac_create("temp",
                       a)
a <- idbac_connect("temp",
                             dirname(a))[[1]]

b <- DBI::dbListTables(a)

test_that("idbac_create() creates the correct tables", {
  expect_true("locale" %in% b)
  expect_true("mass_index" %in% b)
  expect_true("metadata" %in% b)
  expect_true("spectra" %in% b)
  expect_true("version" %in% b)
  expect_true("xml" %in% b)
})


z <- lapply(b, function(x) paste0(DBI::dbListFields(a, x), collapse = "|"))

names(z) <- b


test_that("idbac_create() creates the correct fields", {
  expect_equal(z$locale, 
               "locale")
  expect_equal(z$mass_index, 
               "spectrum_mass_hash|mass_vector")
  expect_equal(z$metadata, 
               "strain_id|genbank_accession|ncbi_taxid|kingdom|phylum|class|order|family|genus|species|maldi_matrix|dsm_cultivation_media|cultivation_temp_celsius|cultivation_time_days|cultivation_other|user_firstname_lastname|user_orcid|pi_firstname_lastname|pi_orcid|dna_16s")
  expect_equal(z$spectra, 
               "spectrum_mass_hash|spectrum_intensity_hash|xml_hash|strain_id|peak_matrix|spectrum_intensity|max_mass|min_mass|ignore|number|time_delay|time_delta|calibration_constants|v1_tof_calibration|data_type|data_system|spectrometer_type|inlet|ionization_mode|acquisition_method|acquisition_date|acquisition_mode|tof_mode|acquisition_operator_mode|laser_attenuation|digitizer_type|flex_control_version|id|instrument|instrument_id|instrument_type|mass_error|laser_shots|patch|path|laser_repetition|spot|spectrum_type|target_count|target_id_string|target_serial_number|target_type_number")
  expect_equal(z$version, 
               "idbac_version|r_version|db_version")
  expect_equal(z$xml, 
               "xml_hash|xml|manufacturer|model|ionization|analyzer|detector|instrument_metafile")
})





