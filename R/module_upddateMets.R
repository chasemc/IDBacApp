#' updateMeta_UI
#'
#' @param id id
#'
#' @return ui
#'
#'
updateMeta_UI <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    column(
      width = 6,
      p("Here is where you can add information about your sample. There are always standard
         columns like \"Genus\", but you can add your own columns as well."),
      p("After you are finished inputting your information, press \"save\" to write the information
         to the database."),
      actionButton(ns("saven"),
        label = "save"
      )
    ),
    column(
      width = 6,
      textInput(ns("addMetaColumnName"),
        label = "New Column Name"
      ),
      actionButton(ns("insertNewMetaColumn"),
        label = "Insert Column"
      )
    ),
    rhandsontable::rHandsontableOutput(ns("metaTable"))
  )
}

#' updateMeta_server
#'
#' @param input  .
#' @param output  .
#' @param session  .
#' @param pool  .
#' @param selectedDB  .
#'
#' @return .
#'
#'
updateMeta_server <- function(input,
                              output,
                              session,
                              pool,
                              selectedDB) {
  #----
  rhand <- reactiveValues(rtab = data.frame("strain_id" = "Placeholder"))
  #----
  output$metaTable <- rhandsontable::renderRHandsontable({
    rhandsontable::rhandsontable(rhand$rtab,
      useTypes = FALSE,
      contextMenu = TRUE
    ) %>%
      rhandsontable::hot_col("strain_id",
        readOnly = TRUE
      ) %>%
      rhandsontable::hot_row(1,
        readOnly = TRUE
      ) %>%
      rhandsontable::hot_context_menu(
        allowRowEdit = FALSE,
        allowColEdit = TRUE
      ) %>%
      rhandsontable::hot_cols(colWidths = 100) %>%
      rhandsontable::hot_rows(rowHeights = 25) %>%
      rhandsontable::hot_cols(fixedColumnsLeft = 1)
  })

  observeEvent(input$insertNewMetaColumn,
    ignoreInit = TRUE,
    {
      insertMetadataColumns(
        pool = pool(),
        columnNames = input$addMetaColumnName
      )
    }
  )


  observeEvent(input$saven,
    ignoreInit = TRUE,
    {
      showModal(modalDialog(
        size = "s",
        title = "Saving...",
        easyClose = FALSE,
        footer = ""
      ))
      #----
      # make sure not to use the wrong metadata table
      query <- glue::glue_sql("SELECT  `strain_id`
                                          FROM `metadata`",
        .con = pool()
      )
      query <- DBI::dbGetQuery(pool(), query)[, 1]
      # [-1, because first row is example row
      userTab <- as.character(rhandsontable::hot_to_r(input$metaTable)[-1, 1])
      tes <- identical(
        sort(userTab),
        sort(query)
      )
      validate(need(tes, "Metadata rhandsontable strain_ids didn't match database strain_ids"))
      #----
      DBI::dbWriteTable(
        conn = pool(),
        name = "metadata",
        value = rhandsontable::hot_to_r(input$metaTable)[-1, ], # remove example row
        overwrite = TRUE
      )
      removeModal()
    }
  )


  #----
  observeEvent(c(selectedDB$selectExperiment, input$insertNewMetaColumn), {
    if (identical(selectedDB$selectExperiment, "None")) {
      rhand$rtab <- data.frame("strain_id" = "Placeholder")
    } else {
      if (!is.null(pool())) {
        conn <- pool::poolCheckout(pool())
        if (!"metadata" %in% tolower(DBI::dbListTables(conn))) {
          warning("It appears the experiment file may be corrupt, please create again.")
          rhand$rtab <- data.frame(strain_id = "It appears the experiment file may be corrupt, please create the experiment again.")
        } else {
          dbQuery <- DBI::dbGetQuery(conn, "SELECT * FROM metadata")
          exampleMetaData <- data.frame(
            "strain_id" = "Example_Strain",
            "genbank_accession" = "KY858228",
            "ncbi_taxid" = "446370",
            "kingdom" = "Bacteria",
            "phylum" = "Firmicutes",
            "class" = "Bacilli",
            "order" = "Bacillales",
            "family" = "Paenibacillaceae",
            "genus" = "Paenibacillus",
            "species" = "telluris",
            "maldi_matrix" = "CHCA",
            "dsm_cultivation_media" = "1054_Fresh",
            "cultivation_temp_celsius" = "27",
            "cultivation_time_days" = "10",
            "cultivation_other" = "",
            "user_firstname_lastname" = "Chase Clark",
            "user_orcid" = "0000-0001-6439-9397",
            "pi_firstname_lastname" = "Brian Murphy",
            "pi_orcid" = "0000-0002-1372-3887",
            "dna_16s" = "TCCTGCCTCAGGACGAACGCTGGCGGCGTGCCTAATACATGCAAGTCGAGCGGAGTTGATGGAGTGCTTGCACTCCTGATGCTTAGCGGCGGACGGGTGAGTAACACGTAGGTAACCTGCCCGTAAGACTGGGATAACATTCGGAAACGAATGCTAATACCGGATACACAACTTGGTCGCATGATCGGAGTTGGGAAAGACGGAGTAATCTGTCACTTACGGATGGACCTGCGGCGCATTAGCTAGTTGGTGAGGTAACGGCTCACCAAGGCGACGATGCGTAGCCGACCTGAGAGGGTGATCGGCCACACTGGGACTGAGACACGGCCCAGACTCCTACGGGAGGCAGCAGTAGGGAATCTTCCGCAATGGACGAAAGTCTGACGGAGCAACGCCGCGTGAGTGATGAAGGTTTTCGGATCGTAAAGCTCTGTTGCCAGGGAAGAACGCTAAGGAGAGTAACTGCTCCTTAGGTGACGGTACCTGAGAAGAAAGCCCCGGCTAACTACGTGCCAGCAGCCGCGGTAATACGTAGGGGGCAAGCGTTGTCCGGAATTATTGGGCGTAAAGCGCGCGCAGGCGGCCTTGTAAGTCTGTTGTTTCAGGCACAAGCTCAACTTGTGTTCGCAATGGAAACTGCAAAGCTTGAGTGCAGAAGAGGAAAGTGGAATTCCACGTGTAGCGGTGAAATGCGTAGAGATGTGGAGGAACACCAGTGGCGAAGGCGACTTTCTGGGCTGTAACTGACGCTGAGGCGCGAAAGCGTGGGGAGCAAACAGGATTAGATACCCTGGTAGTCCACGCCGTAAACGATGAATGCTAGGTGTTAGGGGTTTCGATACCCTTGGTGCCGAAGTTAACACATTAAGCATTCCGCCTGGGGAGTACGGTCGCAAGACTGAAACTCAAAGGAATTGACGGGGACCCGCACAAGCAGTGGAGTATGTGGTTTAATTCGAAGCAACGCGAAGAACCTTACCAGGTCTTGACATCCCTCTGAATCTGCTAGAGATAGCGGCGGCCTTCGGGACAGAGGAGACAGGTGGTGCATGGTTGTCGTCAGCTCGTGTCGTGAGATGTTGGGTTAAGTCCCGCAACGAGCGCAACCCTTGATCTTAGTTGCCAGCAGGTKAAGCTGGGCACTCTAGGATGACTGCCGGTGACAAACCGGAGGAAGGTGGGGATGACGTCAAATCATCATGCCCCTTATGACCTGGGCTACACACGTACTACAATGGCCGATACAACGGGAAGCGAAACCGCGAGGTGGAGCCAATCCTATCAAAGTCGGTCTCAGTTCGGATTGCAGGCTGCAACTCGCCTGCATGAAGTCGGAATTGCTAGTAATCGCGGATCAGCATGCCGCGGTGAATACGTTCCCGGGTCTTGTACACACCGCCCGTCACACCACGAGAGTTTACAACACCCGAAGCCGGTGGGGTAACCGCAAGGAGCCAGCCGTCGAAGGTGGGGTAGATGATTGGGGTGAAGTCGTAAC"
          )
          rhand$rtab <- merge(exampleMetaData,
            dbQuery,
            all = TRUE,
            sort = TRUE
          )
          pool::poolReturn(conn)
        }
      }
    }
  })
}
