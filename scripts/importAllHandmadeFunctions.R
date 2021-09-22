importAllHandmadeFunctions <- function(folderLocation){
  for (f in list.files("scripts/", pattern="*.R")){
    source(paste("scripts/",f, sep=""))
  }
}