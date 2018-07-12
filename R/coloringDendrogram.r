# useDots = TRUE then draw dend and dots
# useDots = FALSE then color labels

coloringDendrogram <- function(useDots,
                               cutByHeight,
                               useKMeans,
                               userColor,
                               excelFilePath,
                               chosenIdColumn,
                               chosenMetaColumn,
                               dendrogram,
                               cutHeight,
                               cutK,
                               chosenColorsMeta,
                               colorsChosen){





  colorBlindPalette <- cbind.data.frame(fac = 1:1008,col = c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", rainbow(1000)))


  toReturn <- list(dend=NULL, bigMatrix=NULL, shortenedNames=NULL)

  if(userColor == TRUE){

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





          #get colors chosen
          #  colorsChosen <- sapply(1:length(sampleFactors),function(x)input[[paste0("factor-",x,"_",sampleFactors[[x]])]])


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

  if(useKMeans == TRUE){
    toReturn$dend <- dendrogram %>% color_branches(k = cutK, col = as.vector(colorBlindPalette$col[1:cutK]))
  }


  if(cutByHeight == TRUE){
    toReturn$dend <- dendrogram %>% color_branches(h = cutHeight, col = as.vector(colorBlindPalette$col[1:length(unique(cutree(dendrogram, h = cutHeight)))]))
  }


adend <<- toReturn

return(toReturn)


  #If no sample map is selected, run this:
}






