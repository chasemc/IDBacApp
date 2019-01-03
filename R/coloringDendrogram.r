#' @title Functions to color and adjust dendrogram 
#' @export
#' @rdname coloringDendrogram
#' 
#' @param useDots If TRUE then draw dend and dots
#' @param cutByHeight
#' @param useKMeans
#' @param useMetadata
#' @param excelFilePath
#' @param chosenIdColumn
#' @param chosenMetaColumn
#' @param dendrogram
#' @param cutHeight
#' @param cutK
#' @param chosenColorsMeta
#' @param colorsChosen

#' @return dendrogram


coloringDendrogram <- function(dendrogram,
                               useDots,
                               useHeight,
                               drawAbline,
                               useKMeans,
                               useMetadata,
                               excelFilePath,
                               chosenIdColumn,
                               chosenMetaColumn,
                               cutHeight,
                               cutK,
                               chosenColorsMeta,
                               drawAbline,
                               colorsChosen,
                               colorBlindPalette){
  
  # Dendrogram in IDBac is basically three parts (don't include expansion as that is done at render level, so not here)
  # Lines
  # Labels
  # Dots to side of dendgrogram (optional)
  
  toReturn <- new.env(parent = new.env())
  
  toReturn$dend <- dendrogram
  
  
  
  
  
  
  if(useKMeans == TRUE){
    toReturn$dend <- dendrogram %>% color_branches(k = cutK, col = as.vector(colorBlindPalette$col[1:cutK]))
  }
  
  
  if(useHeight == TRUE){
    toReturn$dend <- dendrogram %>% color_branches(h = cutHeight, col = as.vector(colorBlindPalette$col[1:length(unique(cutree(dendrogram, h = cutHeight)))]))
  }
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  if(useMetadata == TRUE){
    
    if(useDots == TRUE){
      
      # This is the user-provided excel sheet, columns represent factors, rows are samples
      # One column will be user-selected to match to names in the dendrogram
      # Another column will be user-selected to color/dot the dendrogram
      
      groupFile <- read_excel(excelFilePath,1 , na = c("","NA"))
      
      
      # User-chosen column that should represent labels of the dendrogram/isolates
      idCol <- chosenIdColumn
      # User-chosen column that represents a factor/attribute of the isolates
      sampCol <- chosenMetaColumn
      #          idCol <- "ID"  # removeme
      #          sampCol <- "DIV. PLATE #" #removeme
      
      # Retain only the columns that will be used
      groupFile %>%
        dplyr::select(c(idCol, sampCol)) %>%
        return(.) -> groupFile
      
      colnames(groupFile) <- c("sampleID", "metaData")
      
      
      # Dendrogram labels
      #       dendLabels <- labels(dendro())
      dendLabels <- dplyr::bind_cols("sampleID" = labels(dendrogram)) #removeme
      
      
      # Collapse all strings to exclude spaces. People have a hard time with spaces :(  # Might need to reconsider doing this
      dendLabels[ , "sampleID"] <- gsub(" ", "", as.character(unlist(dendLabels[ , "sampleID"])))
      groupFile[ , "sampleID"]  <- gsub(" ", "", as.character(unlist(groupFile[ , "sampleID"])))
      
      
      
      # Retain dendrogram
      groupFile <- dplyr::full_join(groupFile, dendLabels, by = "sampleID")
      
      
      groupFile %>%
        dplyr::mutate(metaData=replace(metaData, is.na(metaData), "Missing_in_Excel")) %>%
        return(.) -> groupFile
      
      
      # Make a large list of tibbles,
      bigList <- split(groupFile, groupFile$metaData)
      
      # Make each tibble contain all sampleIDs and for those not in that group, make the metaData = NA
      bigList <- lapply(bigList, function(x) dplyr::right_join(x, dendLabels[ , "sampleID"], by="sampleID"))
      
      
      
      
      awe <- bind_cols(colorNew = colorsChosen, idc = chosenColorsMeta)
      
      
      
      
      for(i in names(bigList)){
        
        awe %>%
          filter(`idc` == i) %>%
          select(`colorNew`) %>%
          as.character %>%
          return(.) -> toColor
        
        bigList[[i]] %>%
          mutate(metaData = replace(metaData, !is.na(metaData), toColor)) %>%
          mutate(metaData = replace(metaData, is.na(metaData), "#00000000")) %>%
          select(metaData) %>%
          return(.) -> bigList[[i]]
      }
      
      
      
      labs <- labels(bigList)
      bigList <- bind_cols(bigList)
      colnames(bigList) <- labs
      
      
      
      
      
      
      toReturn$dend <- dendrogram %>% set("labels_cex",1 )
      shortenedNames <- dendrogram
      labels(shortenedNames) <- strtrim(labels(shortenedNames),20)
      toReturn$bigMatrix <- as.data.frame(bigList)
      toReturn$shortenedNames <- shortenedNames
      
      
      
    }else{
      sampleMappings <- as.data.frame(read_excel(excelFilePath, 1))
      sampleFactors <- sampleMappings[chosenMetaColumn] %>% unique %>% unlist %>% as.vector %>% gsub(" ","", . )
      sampleIDs1 <- cbind.data.frame(sampleFactorID=sampleMappings[[chosenIdColumn]],chosenFactor=sampleMappings[[chosenMetaColumn]])
      
      # zz <- cbind.data.frame(colorsChosen,sampleFactors)
      a1 <- cbind(colorNew = colorsChosen, chosenFactor = chosenColorsMeta)
      b1 <- cbind(chosenFactor = sampleFactors)
      colorsToreplace <- merge(a1, b1, by="chosenFactor")
      
      # Merge excel sample IDs and factor with colors chosen by user in-app
      excelData <- merge(sampleIDs1,colorsToreplace,by="chosenFactor")
      
      # Collapse strings to exclude spaces
      excelData$sampleFactorID <- gsub(" ","",excelData$sampleFactorID)
      dendroLabels <- gsub(" ","",labels(dendrogram))
      dendColor <- as.vector(excelData$colorNew[match(dendroLabels,excelData$sampleFactorID)])
      dendColor[is.na(dendColor)]<-"#000000"
      toReturn$dend <-  dendrogram %>% color_labels(labels=labels(dendrogram), col=dendColor)
    }
    
  }
  
  
  
  
  toReturn
  
  
  
  
  
}





# dendextend::set.... branches_col - set the color of branches (using assign_values_to_branches_edgePar)
# dendextend::set.... branches_lwd - set the line width of branches (using assign_values_to_branches_edgePar)


colorDendLinesBy <- function(dendrogram,
                             colorBy,
                             colorBlindPalette){
  
  if(class(dendrogram) != "dendrogram"){
    warning("Dendrogram input wasn't of class \"dendrogram\"")
   } else if (class(colorBy) != "character") {
      warning(paste0("colorBy was type ", class(colorBy), ", expected character vector."))
    }
  else if (class(colorBlindPalette) != "character") {
    warning(paste0("colorBlindPalette was type ", class(colorBlindPalette), ", expected character vector."))
  }
    
    
    
    
    
  # set(object, what, value)
  # dendextend::set.... branches_col - set the color of branches (using assign_values_to_branches_edgePar)
  # dendextend::set.... branches_lwd - set the line width of branches (using assign_values_to_branches_edgePar)
  
  if(input$colorBy == "none") {
    
    
  } else if(colorBy == "height") {
    
    return( 
      color_branches(dendrogram, 
                     k = cutK,
                     col = colorBlindPalette[1:cutK])
    )
    
  } else if(input$colorBy == "groups") {
    
    return(
      dendrogram %>% color_branches(h = cutHeight, col = as.vector(colorBlindPalette$col[1:length(unique(cutree(dendrogram, h = cutHeight)))]))
    )
    
  } else if(input$colorBy == "metadata") {
    return(
      toReturn$dend <- dendrogram %>% color_branches(h = cutHeight, col = as.vector(colorBlindPalette$col[1:length(unique(cutree(dendrogram, h = cutHeight)))]))
    )
  }
  return(dendrogram)
}











