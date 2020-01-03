


#' sampMetaOutput
#'
#' @param id NA
#' @param label NA
#'
#' @return NA
#' @export
#'

sampMetaOutput <- function(id, label = "Sample Metadata Input") {
  # Create a namespace function using the provided id
  ns <- NS(id)
  rHandsontableOutput(ns("metaTable"))
  
}



#' sampMeta
#'
#' @param input NA
#' @param output NA
#' @param session NA
#' @param databaseConnection NA
#'
#' @return NA
#' @export
#'

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
                              tab = "metadata",
                              .con = databaseConnection)
    
    conn <- pool::poolCheckout(databaseConnection)
    dbQuery <- DBI::dbSendQuery(conn, dbQuery)
    dbQuery <- DBI::dbFetch(dbQuery)
    
    
    exampleMetaData <- data.frame(      "strain_id"                    = "Example_Strain",
                                        "genbank_accession"            = "KY858228",
                                        "ncbi_taxid"                   = "446370",
                                        "kingdom"                      = "Bacteria",
                                        "phylum"                       = "Firmicutes",
                                        "class"                        = "Bacilli",
                                        "order"                        = "Bacillales",
                                        "family"                       = "Paenibacillaceae",
                                        "genus"                        = "Paenibacillus",
                                        "species"                      = "telluris",
                                        "maldi_matrix"                 = "CHCA",
                                        "dsm_cultivation_media"               = "1054_Fresh",
                                        "cultivation_temp_celsius"     = "27",
                                        "cultivation_time_days"        = "10",
                                        "cultivation_other"            = "",
                                        "user_firstname_lastname"      = "Chase Clark",
                                        "user_orcid"                   = "0000-0001-6439-9397",
                                        "pi_firstname_lastname"        = "Brian Murphy",
                                        "pi_orcid"                     = "0000-0002-1372-3887",
                                        "dna_16s"                      = "TCCTGCCTCAGGACGAACGCTGGCGGCGTGCCTAATACATGCAAGTCGAGCGGAGTTGATGGAGTGCTTGCACTCCTGATGCTTAGCGGCGGACGGGTGAGTAACACGTAGGTAACCTGCCCGTAAGACTGGGATAACATTCGGAAACGAATGCTAATACCGGATACACAACTTGGTCGCATGATCGGAGTTGGGAAAGACGGAGTAATCTGTCACTTACGGATGGACCTGCGGCGCATTAGCTAGTTGGTGAGGTAACGGCTCACCAAGGCGACGATGCGTAGCCGACCTGAGAGGGTGATCGGCCACACTGGGACTGAGACACGGCCCAGACTCCTACGGGAGGCAGCAGTAGGGAATCTTCCGCAATGGACGAAAGTCTGACGGAGCAACGCCGCGTGAGTGATGAAGGTTTTCGGATCGTAAAGCTCTGTTGCCAGGGAAGAACGCTAAGGAGAGTAACTGCTCCTTAGGTGACGGTACCTGAGAAGAAAGCCCCGGCTAACTACGTGCCAGCAGCCGCGGTAATACGTAGGGGGCAAGCGTTGTCCGGAATTATTGGGCGTAAAGCGCGCGCAGGCGGCCTTGTAAGTCTGTTGTTTCAGGCACAAGCTCAACTTGTGTTCGCAATGGAAACTGCAAAGCTTGAGTGCAGAAGAGGAAAGTGGAATTCCACGTGTAGCGGTGAAATGCGTAGAGATGTGGAGGAACACCAGTGGCGAAGGCGACTTTCTGGGCTGTAACTGACGCTGAGGCGCGAAAGCGTGGGGAGCAAACAGGATTAGATACCCTGGTAGTCCACGCCGTAAACGATGAATGCTAGGTGTTAGGGGTTTCGATACCCTTGGTGCCGAAGTTAACACATTAAGCATTCCGCCTGGGGAGTACGGTCGCAAGACTGAAACTCAAAGGAATTGACGGGGACCCGCACAAGCAGTGGAGTATGTGGTTTAATTCGAAGCAACGCGAAGAACCTTACCAGGTCTTGACATCCCTCTGAATCTGCTAGAGATAGCGGCGGCCTTCGGGACAGAGGAGACAGGTGGTGCATGGTTGTCGTCAGCTCGTGTCGTGAGATGTTGGGTTAAGTCCCGCAACGAGCGCAACCCTTGATCTTAGTTGCCAGCAGGTKAAGCTGGGCACTCTAGGATGACTGCCGGTGACAAACCGGAGGAAGGTGGGGATGACGTCAAATCATCATGCCCCTTATGACCTGGGCTACACACGTACTACAATGGCCGATACAACGGGAAGCGAAACCGCGAGGTGGAGCCAATCCTATCAAAGTCGGTCTCAGTTCGGATTGCAGGCTGCAACTCGCCTGCATGAAGTCGGAATTGCTAGTAATCGCGGATCAGCATGCCGCGGTGAATACGTTCCCGGGTCTTGTACACACCGCCCGTCACACCACGAGAGTTTACAACACCCGAAGCCGGTGGGGTAACCGCAAGGAGCCAGCCGTCGAAGGTGGGGTAGATGATTGGGGTGAAGTCGTAAC"
    )
    
    dbQuery <- rbind(exampleMetaData, dbQuery)
    
    rhandsontable::rhandsontable(dbQuery,
                                 useTypes = FALSE,
                                 contextMenu = TRUE ) %>%
      hot_col("strain_id", readOnly = TRUE) %>%
      rhandsontable::hot_row(1,  readOnly = TRUE) %>%
      hot_context_menu(allowRowEdit = FALSE,
                       allowColEdit = TRUE) %>%
      hot_cols(colWidths = 100) %>%
      hot_rows(rowHeights = 25) %>%
      hot_cols(fixedColumnsLeft = 1)
    
  })
  
  
  return(dbqueryReactive)
  
  
  
}


