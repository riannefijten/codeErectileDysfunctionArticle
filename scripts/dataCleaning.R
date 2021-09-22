readSasFile <- function(fileLocation){
  library(sas7bdat)
  importedData = read.sas7bdat(fileLocation)
  return(importedData)
}
