#' Write csv file with metadata template
#'
#' @param dData Data frame to be written as csv file with metadata template
#' @param file Base name of output csv file
#' @param embedded Are the metada added to the csv file above the data or is the
#'   metadata written into a seperate file. If the data are embedded, the output
#'   file name is "file_withMeta.csv". If the data are not embedded, the data
#'   file is named "file.csv" and the metadate file is named "file_meta.csv".
#'   Default embedded = TRUE
#' @param gen_attributes Vector of general attributes for metadata template.
#'   Default is gen_format_2 from the internal list of format options. Can be
#'   any user provided character vector that does not contain any commas.
#' @param var_attributes Vector of variable attributes for metadata template.
#'   Default is gen_format_2 from the internal list of format options. Can be
#'   any user provided character vector that does not contain any commas.
#'
#' @return Writes csv file(s).
#' @export
#'
#' @examples
#' write_meta_template(iris, "~/iris")
#' write_meta_template(iris, "~/iris", embedded = FALSE)
write_meta_template <- function(dData, file, embedded = TRUE,
                       gen_attributes = gen_attrib$attribute_name,
                       var_attributes = var_attrib$attribute_name){
  # TODO suppress write append warnings
  dGenMeta <- data.frame(Attribute = gen_attributes)
  dGenMeta$Value <- ""
  #create the variable metadata data frame with variable names from the source data
  dVarMeta <- data.frame(matrix(ncol = length(var_attributes), nrow = ncol(dData)))
  colnames(dVarMeta) <- var_attributes
  dVarMeta[,] <- ""
  dVarMeta[,1] <- names(dData)
  #if present, remove the .csv extension from the file name
  file <- stringr::word(file, sep = ".csv")
  if(embedded){
    #add .csv extension
    file <- paste(file, "_withMeta.csv", sep = "")
    # write the output file appending the general metadata, variable metadata, actual data
    # and format makers in the appropriate order
    utils::write.table(data.frame("*metacsv_format_file*"), file, row.names = FALSE,
                col.names = FALSE,sep = ",")
    utils::write.table(dGenMeta, file, row.names = FALSE, col.names = FALSE, sep = ",", append = TRUE)
    utils::write.table(data.frame("***"), file, row.names = FALSE,
                col.names = FALSE,sep = ",", append = TRUE)
    suppressWarnings(utils::write.table(dVarMeta, file, row.names = FALSE, sep = ",", append = TRUE))
    utils::write.table(data.frame("###"), file, row.names = FALSE,
                col.names = FALSE,sep = ",", append = TRUE)
    suppressWarnings(utils::write.table(dData, file, row.names = FALSE, sep = ",", append = TRUE))
  } else{
    #write and external metafile template
    metaFileName <- paste(file, "_meta.csv", sep = "")
    utils::write.table(data.frame("*metacsv_format_file*"), metaFileName, row.names = FALSE,
                col.names = FALSE,sep = ",")
    utils::write.table(dGenMeta, metaFileName, row.names = FALSE, col.names = FALSE,
                sep = ",", append = TRUE)
    utils::write.table(data.frame("***"), metaFileName, row.names = FALSE,
                col.names = FALSE,sep = ",", append = TRUE)
    suppressWarnings(utils::write.table(dVarMeta, metaFileName, row.names = FALSE, sep = ",", append = TRUE))
    utils::write.table(data.frame("###"), metaFileName, row.names = FALSE,
                col.names = FALSE,sep = ",", append = TRUE)
    #write the data in a seperate .csv file
    file <- paste(file, ".csv", sep = "")
    utils::write.csv(dData, file, row.names = FALSE)
  }
}
