testDifferenceBetweenSets = function(dataset1, dataset2, columnToTest){
  if(missing(columnToTest)){
    columnToTest = 1:dim(dataset1)[2]
  }

  completeDataset = rbind(dataset1, dataset2)
  testResults = cbind(variableName = colnames(dataset1),
                      pvalue = rep(2,length(columnToTest)), 
                      testName = rep(NA,length(columnToTest)), 
                      significance = rep(NA,length(columnToTest))) #since p-values are always somewhere between 0 and 1, the empty list has 2s to avoid confusion
  for (columnNumber in columnToTest){
    normalYesNo = testNormalDistribution(completeDataset[,columnNumber]) 
    if (normalYesNo){
      testResults[columnNumber, 2] = t.test(dataset1[,columnNumber], dataset2[,columnNumber])$p.value
      testResults[columnNumber, 3] = "Student"
    } else {
      testResults[columnNumber, 2] = wilcox.test(dataset1[,columnNumber], dataset2[,columnNumber])$p.value
      testResults[columnNumber, 3] = "Wilcoxon"
    }
    testResults[columnNumber, 4] = ifelse(as.numeric(testResults[columnNumber, 2]) < 0.05, "*", "") # star when significant (<0.05)
  }
  return(testResults)
}
  


testDifferenceBetweenSetsWithBoxplot = function(trainingSet, testSet, columnToTest){
  # only one column at a time, so we can plot a graph with it.
  
  completeDataset = rbind(trainingSet, testSet)
  normalYesNo = testNormalDistribution(completeDataset[,columnToTest]) 
  if (normalYesNo){
    testResult = t.test(trainingSet[,columnToTest], testSet[,columnToTest])
    boxplot = createScatterBoxPlot(completeDataset, c(replicate(dim(trainingSet)[1],1), replicate(dim(testSet)[1], 2)), completeDataset[columnToTest], 
                                   paste("Column: ", colnames(trainingSet[columnToTest]), "; t-test p-value = ", testResult$p.value, sep=""))
  } else {
    testResult = wilcox.test(trainingSet[,columnToTest], testSet[,columnToTest])
    boxplot = createScatterBoxPlot(completeDataset, c(replicate(dim(trainingSet)[1],1), replicate(dim(testSet)[1], 2)), completeDataset[,columnToTest], 
                                   paste("Column: ", colnames(trainingSet[columnToTest]), "; Wilcoxon p-value = ", testResult$p.value, sep=""))
  }
  return(boxplot)
}

testNormalDistribution = function (datasetForTesting){
  testResults = shapiro.test(datasetForTesting)  
  if (testResults$p.value > 0.05){
    return(TRUE)
  } else return(FALSE)
}

calculateFDRqvalues = function(pvalues){
  library(fdrtool)
  qvalues = cbind(qvalue = rep(2, length(pvalues)), 
                  significance = rep(NA, length(pvalues)))
  fdrstats = fdrtool(as.numeric(pvalues), statistic=c("pvalue"), plot=FALSE)
  qvalues[,1] = fdrstats$qval
  for (i in 1:dim(qvalues)[1]){
    qvalues[i,2] = ifelse(as.numeric(qvalues[i,1]) < 0.05, "*","")
  }
  return(qvalues)
}


