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
#'
read_meta <- function(file){
  #read the first row of the data file
  inFile <- read.csv(file, stringsAsFactors=FALSE, header = FALSE)
  #test whether the file has NWFSC OA format metadata,
  #which starts with "NWFSC_OA_format_file" in the first cell
  if(inFile[1,1] == "NWFSC_OA_format_file"){
    #find the metadata start/end markers in the first column
    endGeneralMetaMakerRow <- match("***", inFile[,1])
    endVariableMetaMarkerRow <- match("###", inFile[,1])
    #read the general metadata info and clean it up so first column
    # is the attribute name and the second column is the value of the attribute
    dGeneralMeta <- read.csv(file, stringsAsFactors=FALSE,
                             header = FALSE, skip = 1, nrows = endGeneralMetaMakerRow - 2)
    dGeneralMeta <- dGeneralMeta[,1:2]
    names(dGeneralMeta) <- c("Attribute", "Value")
    #Read in the variable metadata table and clean it up
    dVariableMeta <- read.csv(file, stringsAsFactors=FALSE,
                              header = FALSE, skip = endGeneralMetaMakerRow,
                              nrows = endVariableMetaMarkerRow - endGeneralMetaMakerRow - 1)
    attributeNames <- dVariableMeta[1,]
    attributeNames <- attributeNames[!is.na(attributeNames)]
    dVariableMeta <- dVariableMeta[2:length(dVariableMeta[,1]), 1:length(attributeNames)]
    names(dVariableMeta) <- attributeNames
    #Read in the meta data file
    dData <- read.csv(file, stringsAsFactors=FALSE,
                      header = TRUE, skip = endVariableMetaMarkerRow)
    #need to add some code here to remove columns in dData if
    # variable name starts with "x" and all values in the column are NA
    # this situation happens if the number of variables in the data set are < length(attributeNames)
  } else{
    #name of potential seperate metadata file
    metaFileName <- word(file, sep = ".csv")
    metaFileName <- paste(metaFileName, "_meta.csv", sep = "")
    if(file.exists(metaFileName)){
      dMeta <- read.csv(metaFileName, stringsAsFactors = FALSE, header = FALSE)
      endGeneralMetaMakerRow <- match("***", dMeta[,1])
      dGeneralMeta <- read.csv(metaFileName, stringsAsFactors=FALSE,
                               header = FALSE, nrows = endGeneralMetaMakerRow - 1)
      dGeneralMeta <- dGeneralMeta[,1:2]
      names(dGeneralMeta) <- c("Attribute", "Value")
      dVariableMeta <- read.csv(metaFileName, stringsAsFactors=FALSE,
                                header = TRUE, skip = endGeneralMetaMakerRow)
      dData <- read.csv(fileName, stringsAsFactors=FALSE, header = TRUE)
    }
    else{
      dGeneralMeta <- NULL
      dVariableMeta <- NULL
      dData <- read.csv(file, stringsAsFactors=FALSE, header = TRUE)
    }
  }
  dList <- list(generalMetadata = dGeneralMeta,
                variableMetadata = dVariableMeta,
                data = dData)
  #return the datafile
  return(dList)
}
