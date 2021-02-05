#' Read csv file with metadata
#'
#' @param file path and name of the .csv data file to read
#'
#' @return List with three data frames
#'    \describe{
#'     \item{dGeneralMeta}{General metadata with attribute and value}
#'     \item{dVariableMeta}{Table of metadata for each dataset variable}
#'     \item{dData}{The dataset}
#'    }
#' @export
#'
#' @examples
#' iris_withMeta_raw <- read.csv(system.file("iris_withMeta.csv", package = "metacsv" ))
#' iris_withMeta_parsed <- read_meta(system.file("iris_withMeta.csv", package = "metacsv" ))
read_meta <- function(file){
  #read the first row of the data file
  inFile <- utils::read.csv(file, stringsAsFactors=FALSE, header = FALSE)
  #test whether the file has metacsv format metadata,
  #which starts with "metacsv_format_file" in the first cell
  if(inFile[1,1] == "*metacsv_format_file*"){
    #find the metadata start/end markers in the first column
    genBreakRow <- match(c("***"), inFile[,1])
    inFile <- utils::read.csv(file, stringsAsFactors=FALSE, header = FALSE, skip = genBreakRow)
    varBreakRow <- match(c("###"), inFile[,1])
    #read the general metadata info and clean it up so first column
    # is the attribute name and the second column is the value of the attribute
    dGeneralMeta <- utils::read.csv(file, stringsAsFactors=FALSE,
                             header = FALSE, skip = 1, nrows = genBreakRow - 2)
    dGeneralMeta <- dGeneralMeta[,1:2]
    names(dGeneralMeta) <- c("Attribute", "Value")
    #Read in the variable metadata table and clean it up
    dVariableMeta <- utils::read.csv(file, stringsAsFactors=FALSE,
                              header = FALSE, skip = genBreakRow,
                              nrows = varBreakRow - 1)
    attributeNames <- dVariableMeta[1,]
    attributeNames <- attributeNames[!is.na(attributeNames)]
    dVariableMeta <- dVariableMeta[2:length(dVariableMeta[,1]), 1:length(attributeNames)]
    names(dVariableMeta) <- attributeNames
    #Read in the meta data file first find the length of the header in case the
    #dataset has fewer columns than the number of variable attribute tables
    dDataHeaderRow <- utils::read.csv(file, stringsAsFactors=FALSE,
                               header = FALSE, nrows = 1,
                               skip = genBreakRow + varBreakRow)
    header <- dDataHeaderRow[1,]
    header <- header[!is.na(header)]
    dData <- utils::read.csv(file, stringsAsFactors=FALSE,
                      header = TRUE, skip = genBreakRow + varBreakRow )
    dData <- dData[,1:length(header)]
    #need to add some code here to remove columns in dData if
    # variable name starts with "x" and all values in the column are NA
    # this situation happens if the number of variables in the data set are < length(attributeNames)
  } else{
    #name of potential seperate metadata file
    metaFileName <- stringr::word(file, sep = ".csv")
    metaFileName <- paste(metaFileName, "_meta.csv", sep = "")
    if(file.exists(metaFileName)){
      dMeta <- utils::read.csv(metaFileName, stringsAsFactors = FALSE, header = FALSE)
      genBreakRow <- match("***", dMeta[,1])
      dGeneralMeta <- utils::read.csv(metaFileName, stringsAsFactors=FALSE, skip = 1,
                               header = FALSE, nrows = genBreakRow - 2)
      dGeneralMeta <- dGeneralMeta[,1:2]
      names(dGeneralMeta) <- c("Attribute", "Value")
      dVariableMeta <- utils::read.csv(metaFileName, stringsAsFactors=FALSE,
                                header = TRUE, skip = genBreakRow)
      dVariableMeta <- dVariableMeta[1:nrow(dVariableMeta)-1,]
      dData <- utils::read.csv(file, stringsAsFactors=FALSE, header = TRUE)
    }
    else{
      dGeneralMeta <- NULL
      dVariableMeta <- NULL
      dData <- utils::read.csv(file, stringsAsFactors=FALSE, header = TRUE)
      warning("There are no embedded or external metadata associated with this file.")
    }
  }
  dList <- list(generalMetadata = dGeneralMeta,
                variableMetadata = dVariableMeta,
                data = dData)
  #return the datafile
  return(dList)
}
