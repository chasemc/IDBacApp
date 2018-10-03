

# Module UI function
sampMetaOutput <- function(id, label = "Sample Metadata Input") {
  # Create a namespace function using the provided id
  ns <- NS(id)


          rHandsontableOutput(ns("metaTable"))

}



sampMeta <- function(input, output, session, databaseConnection) {


  createNewLibraryTable <- reactive({

  })

  # Display the new Library as an editable table
  output$metaTable <- rhandsontable::renderRHandsontable({
    dbqueryReactive()
  })

dbqueryReactive <- reactive({
   dbQuery <- glue::glue_sql("SELECT *
                            FROM ({tab*})",
                              tab = "metaData",
                              .con = databaseConnection)

    conn <- pool::poolCheckout(databaseConnection)
    dbQuery <- DBI::dbSendQuery(conn, dbQuery)
    dbQuery <- DBI::dbFetch(dbQuery)


    exampleMetaData <- data.frame(      "Strain_ID"                    = "Example_Strain",
                                        "Genbank_Accession"            = "KY858228",
                                        "NCBI_TaxID"                   = "446370",
                                        "Kingdom"                      = "Bacteria",
                                        "Phylum"                       = "Firmicutes",
                                        "Class"                        = "Bacilli",
                                        "Order"                        = "Bacillales",
                                        "Family"                       = "Paenibacillaceae",
                                        "Genus"                        = "Paenibacillus",
                                        "Species"                      = "telluris",
                                        "MALDI_Matrix"                 = "CHCA",
                                        "DSM_Agar_Media"               = "1054_Fresh",
                                        "Cultivation_Temp_Celsius"     = "27",
                                        "Cultivation_Time_Days"        = "10",
                                        "Cultivation_Other"            = "",
                                        "User"                         = "Chase Clark",
                                        "User_ORCID"                   = "0000-0001-6439-9397",
                                        "PI_FirstName_LastName"        = "Brian Murphy",
                                        "PI_ORCID"                     = "0000-0002-1372-3887",
                                        "dna_16S"                      = "TCCTGCCTCAGGACGAACGCTGGCGGCGTGCCTAATACATGCAAGTCGAGCGGAGTTGATGGAGTGCTTGCACTCCTGATGCTTAGCGGCGGACGGGTGAGTAACACGTAGGTAACCTGCCCGTAAGACTGGGATAACATTCGGAAACGAATGCTAATACCGGATACACAACTTGGTCGCATGATCGGAGTTGGGAAAGACGGAGTAATCTGTCACTTACGGATGGACCTGCGGCGCATTAGCTAGTTGGTGAGGTAACGGCTCACCAAGGCGACGATGCGTAGCCGACCTGAGAGGGTGATCGGCCACACTGGGACTGAGACACGGCCCAGACTCCTACGGGAGGCAGCAGTAGGGAATCTTCCGCAATGGACGAAAGTCTGACGGAGCAACGCCGCGTGAGTGATGAAGGTTTTCGGATCGTAAAGCTCTGTTGCCAGGGAAGAACGCTAAGGAGAGTAACTGCTCCTTAGGTGACGGTACCTGAGAAGAAAGCCCCGGCTAACTACGTGCCAGCAGCCGCGGTAATACGTAGGGGGCAAGCGTTGTCCGGAATTATTGGGCGTAAAGCGCGCGCAGGCGGCCTTGTAAGTCTGTTGTTTCAGGCACAAGCTCAACTTGTGTTCGCAATGGAAACTGCAAAGCTTGAGTGCAGAAGAGGAAAGTGGAATTCCACGTGTAGCGGTGAAATGCGTAGAGATGTGGAGGAACACCAGTGGCGAAGGCGACTTTCTGGGCTGTAACTGACGCTGAGGCGCGAAAGCGTGGGGAGCAAACAGGATTAGATACCCTGGTAGTCCACGCCGTAAACGATGAATGCTAGGTGTTAGGGGTTTCGATACCCTTGGTGCCGAAGTTAACACATTAAGCATTCCGCCTGGGGAGTACGGTCGCAAGACTGAAACTCAAAGGAATTGACGGGGACCCGCACAAGCAGTGGAGTATGTGGTTTAATTCGAAGCAACGCGAAGAACCTTACCAGGTCTTGACATCCCTCTGAATCTGCTAGAGATAGCGGCGGCCTTCGGGACAGAGGAGACAGGTGGTGCATGGTTGTCGTCAGCTCGTGTCGTGAGATGTTGGGTTAAGTCCCGCAACGAGCGCAACCCTTGATCTTAGTTGCCAGCAGGTKAAGCTGGGCACTCTAGGATGACTGCCGGTGACAAACCGGAGGAAGGTGGGGATGACGTCAAATCATCATGCCCCTTATGACCTGGGCTACACACGTACTACAATGGCCGATACAACGGGAAGCGAAACCGCGAGGTGGAGCCAATCCTATCAAAGTCGGTCTCAGTTCGGATTGCAGGCTGCAACTCGCCTGCATGAAGTCGGAATTGCTAGTAATCGCGGATCAGCATGCCGCGGTGAATACGTTCCCGGGTCTTGTACACACCGCCCGTCACACCACGAGAGTTTACAACACCCGAAGCCGGTGGGGTAACCGCAAGGAGCCAGCCGTCGAAGGTGGGGTAGATGATTGGGGTGAAGTCGTAAC"
    )



    dbQuery <- rbind(exampleMetaData, dbQuery)




















    rhandsontable::rhandsontable(dbQuery,
                                 useTypes = FALSE,
                                 contextMenu = TRUE ) %>%
      hot_col("Strain_ID", readOnly = TRUE) %>%
      rhandsontable::hot_row(1,  readOnly = TRUE) %>%
      hot_context_menu(allowRowEdit = FALSE,
                       allowColEdit = TRUE) %>%
      hot_cols(colWidths = 100) %>%
      hot_rows(rowHeights = 25)






  })


  return(dbqueryReactive)



}


