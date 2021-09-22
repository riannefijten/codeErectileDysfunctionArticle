
createValidationSet <- function(completeDataset, trainingPercentage, columnForSplitting, columnPercentage){
  # this function will split the data into a training and (external) test set while making sure that the following characteristics are similar between the two sets:
  # - a percentage (as defined by "trainingPercentage) of the rows in the data  will be assigned to the training set and the rest will be assigned to the test set
  # - (optional) general characteristics defined by the column numbers in "columnsToTest" should not be significantly different.
  # - (optional) a percentage of values (defined by "columnPercentage") of a prespecified column (defined by  the number specified in "columnForSplitting) will be assigned to the training set and the remaining will be assigned to the test set
  if (missing(columnForSplitting)){
    trainingAndTestSet = simpleDataSplit(completeDataset, trainingPercentage)
  }
  else {
    trainingAndTestSet = splitByColumn(completeDataset, columnForSplitting, columnPercentage)
  }
  return(trainingAndTestSet)
}

splitByColumn = function(completeDataset, columnForSplitting, columnPercentage){
  dataInColumnForSplitting = completeDataset[,columnForSplitting]
  patientNumbers = getCountPerId(dataInColumnForSplitting)
  sortedPatientNumbers = patientNumbers[sort(patientNumbers[,2], decreasing=TRUE, index.return=TRUE)$ix,]
  
  trainingSet = completeDataset[completeDataset[, columnForSplitting] == sortedPatientNumbers[1],]
  testSet = completeDataset[completeDataset[, columnForSplitting] == sortedPatientNumbers[2],]
  
  index = convertToEvenNumber(round(0.25*dim(sortedPatientNumbers)[1]))
  for (i in seq(from = 3, to = index, by = 2)){
    trainingSet = rbind(trainingSet, completeDataset[completeDataset[, columnForSplitting] == sortedPatientNumbers[i],])
    testSet = rbind(testSet, completeDataset[completeDataset[, columnForSplitting] == sortedPatientNumbers[i+1],])
  }
  
  for (i in (index+1):dim(sortedPatientNumbers)[1]){
    if((dim(testSet)[1] / (dim(testSet)[1] + dim(trainingSet)[1])) < (1 - columnPercentage - 0.05)){
      testSet = rbind(testSet, completeDataset[completeDataset[, columnForSplitting] == sortedPatientNumbers[i],])
    }
    else if ((dim(testSet)[1] / (dim(testSet)[1] + dim(trainingSet)[1])) > (1 - columnPercentage)){
      trainingSet = rbind(trainingSet, completeDataset[completeDataset[, columnForSplitting] == sortedPatientNumbers[i],])
    }
  }
  return(list(trainingSet, testSet))
}

getCountPerId <- function(vectorForCount){
  iDAndNumbers = count(vectorForCount)$x
  iDAndNumbers = cbind(iDAndNumbers, count(vectorForCount)$freq)
  colnames(iDAndNumbers) = c("ID", "Number")
  return(iDAndNumbers)
}
convertToEvenNumber = function(number){
  if (is.even(number)){
    index = number
  } else {
    index = round(number + 1)
  }
  return(index)
}

is.even <- function(x) {
  x %% 2 == 0
}

  
simpleDataSplit = function(dataset, trainingPercentage){
  trainingSamples = sample(1:dim(dataset)[1], dim(dataset)[1] * trainingPercentage)
  trainingSet = dataset[trainingSamples,]
  testSet = dataset[-trainingSamples,]
  return(list(trainingSet, testSet))
}
  

checkNanPercentagePerColumn = function(dataset){
  percentageNan = numeric()
  for (i in 1:dim(dataset)[2]){
    numberOfNanInColumn = sum(is.na(dataset[,i]))
    percentageNan[i] = numberOfNanInColumn / dim(dataset)[1] * 100
  }
  return(percentageNan)
}


createBinaryDataset = function(dataset, binaryClassNumber1, binaryClassNumber2) {
  binaryDataset = dataset
  if (length(binaryClassNumber1) == 1){
    binaryDataset[which(dataset == binaryClassNumber1)] = 1
  } else {
    for (i in binaryClassNumber1){
      binaryDataset[which(dataset == i)] = 1
    }
  }
  
  if (length(binaryClassNumber2) == 1){
    binaryDataset[which(dataset == binaryClassNumber2)] = 2
  } else {
    for (i in binaryClassNumber2){
      binaryDataset[which(dataset == i)] = 2
    }
  }
  return(binaryDataset)
  }

createEmptyDataFrame = function(nrows, ncols, rowNames, columnNames){
  dataset = data.frame(matrix(ncol = ncols, nrow = nrows))
  if (!missing(columnNames)){
    colnames(dataset) = columnNames
  }
  if (!missing(rowNames)){
    row.names(dataset) = rowNames
  }
  return(dataset)
}



