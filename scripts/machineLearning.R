performLogitRfe = function(dataset, outcome, sizesPerIteration){
  library(caret)
  library(glmnet)
  if (missing(sizesPerIteration)){
    sizesPerIteration = c(2:dim(dataset)[2])
  }
  
  control <- rfeControl(functions = caretFuncs, method = "boot", verbose=T, returnResamp = "all")
  rfeProfile <- rfe(dataset, factor(outcome), rfeControl = control, method="glmnet", metric = "Accuracy", sizes = sizesPerIteration)
  print(plot(rfeProfile, type = c("g", "o")))
  return(rfeProfile)  
}

calculateVariableImportance = function(rfeProfile, meanOrMedian = "median"){
  variableInformation = rfeProfile$variables
  variableImportance = createEmptyDataFrame(length(unique(variableInformation$var)), 4)
  colnames(variableImportance) = c("name", "mean", "median", "std")
  variableImportance[,1] = unique(variableInformation$var)
  for (i in variableImportance$name){
    variableImportance[which(variableImportance$name == i),2] = mean(variableInformation$Overall[variableInformation$var == i])
    variableImportance[which(variableImportance$name == i),3] = median(variableInformation$Overall[variableInformation$var == i])
    variableImportance[which(variableImportance$name == i),4] = sd(variableInformation$Overall[variableInformation$var == i])
  }
  if (meanOrMedian == "mean"){
    p = createBarPlot(variableImportance$mean, variableImportance$name, "Mean variable importance")
  } else if (meanOrMedian == "median"){
    p = createBarPlot(variableImportance$median, variableImportance$name, "Median variable importance")
  }
  
  print(p)
  return(variableImportance)
  
}

performLogisticRegression = function(dataset, outcomeToTest, showSummary){
  logitModel = glm(factor(outcomeToTest)~., family=binomial(link='logit'), data = dataset)
  if (!missing(showSummary)){
    print(summary(logitModel))
  }
  return(logitModel)
}

reduceVariablesLogit = function(trainingSet, outcomeData, variableImportance, meanOrMedian = "median"){
  library(pROC)
  
  if (meanOrMedian == "median"){
    variableImportance = variableImportance[order(variableImportance$median),]
  } else if (meanOrMedian == "mean"){
    variableImportance = variableImportance[order(variableImportance$mean),]
  }
  sortedDataset = trainingSet[,c(variableImportance$name)]
  allLogitModels = list()
  modelAuc = createEmptyDataFrame(dim(variableImportance)[1]-2, 5)
  colnames(modelAuc) = c("AUC", "sensitivity", "specificity", "overallAccuracy", "numberOfVariables")
  for (i in 1:(dim(variableImportance)[1]-2)){
    print(i)
    reducedDataset = sortedDataset[,(i+1):dim(sortedDataset)[2]]
    allLogitModels[[i]] = performLogisticRegression(reducedDataset, outcomeData)
    modelAuc[i,1] = auc(roc(outcomeData, predict(allLogitModels[[i]], type="response")))
    modelAuc[i,2] = calculateAccuracy(allLogitModels[[i]], outcomeData)[[1]]$sensitivity
    modelAuc[i,3] = calculateAccuracy(allLogitModels[[i]], outcomeData)[[1]]$specificity
    modelAuc[i,4] = calculateAccuracy(allLogitModels[[i]], outcomeData)[[1]]$overallAccuracy
    modelAuc[i,5] = dim(reducedDataset)[2]
  }
  return(list(allLogitModels, modelAuc))
}

calculateAccuracy = function(predictionModel, testSetOutcomes, testSet){
  # does not calculate AUC, but accuracy (TPR, TNR, FNR)
  confMatrix = createConfusionMatrix(predictionModel, testSetOutcomes, testSet)
  sensitivity = confMatrix[1,1] / (confMatrix[1,1] + confMatrix[2,1])
  specificity = confMatrix[2,2] / (confMatrix[1,2] + confMatrix[2,2])
  overallAccuracy = sum(confMatrix[1,1] + confMatrix[2,2]) / sum(confMatrix)
  return(list(
    data.frame("sensitivity" = sensitivity, "specificity" = specificity, "overallAccuracy" = overallAccuracy),
    confMatrix))
}

createConfusionMatrix = function(predictionModel, actualOutcomes, testSet){
  if (missing(testSet)){
    # predict onto trainingset
    modelPrediction = predict(predictionModel)
  } else {
    modelPrediction = predict(predictionModel, newdata = testSet)
  }
  if (predictionModel$method == "glmnet"){
    confMatrix = table(modelPrediction, actualOutcomes)
  } else if (predictionModel$method == "glm.fit"){
    confMatrix = table(ifelse(modelPrediction > 0.5, 2, 1), actualOutcomes)
  }
  if (dim(confMatrix)[1] != 2){
    confMatrix = rbind(confMatrix, c(0, 0))
  }
  return(confMatrix)
}

createROCCurve = function(predictionModel, outcomes, testSet){
  library(pROC)
  
  if (predictionModel$method == "glmnet"){
    if (missing(testSet)){
      outcomePrediction = predict(predictionModel, type = "prob")[1] # provides probabilities for both classes, but we only need one.
    } else {
      outcomePrediction = predict(predictionModel, type = "prob", newdata = testSet)[1] # provides probabilities for both classes, but we only need one.
    }
    rocObject = roc(outcomes, outcomePrediction[,1])
  } else if (predictionModel$method == "glm.fit"){
    if (missing(testSet)){
      outcomePrediction = predict(predictionModel) # provides probabilities for both classes, but we only need one.
    } else {
      outcomePrediction = predict(predictionModel, newdata = testSet) # provides probabilities for both classes, but we only need one.
    }
    rocObject = roc(outcomes, outcomePrediction)
  }
  
  
  plot(rocObject)
  text(0.2, 0.1, paste("AUC: ", round(auc(rocObject), 2)))
}


performUpscaling = function(dataset, outcomeData, method = "adasyn"){
  library(smotefamily)
  
  if (method == "adasyn" || method == "ADASYN"){
    adasyn = ADAS(dataset, outcomeData)
    return(adasyn$data)
  }
}

performRandomForestsRfe = function(dataset, outcome, sizesPerIteration){
  library(caret)
  library(randomForest)
  
  if (missing(sizesPerIteration)){
    sizesPerIteration = c(2:dim(dataset)[2])
  }
  
  control <- rfeControl(functions = rfFuncs, method = "boot", verbose=T, returnResamp = "all")
  rfeProfile <- rfe(dataset, factor(outcome), rfeControl = control, method="glmnet", metric = "Accuracy", sizes = sizesPerIteration)
  print(plot(rfeProfile, type = c("g", "o")))
  return(rfeProfile)
}

performRandomForests = function(dataset, outcome, testset, testsetOutcome, trees = 500){
  if (missing(testset)){
    rfModel = randomForest(dataset, factor(outcome), ntree = trees, importance = TRUE, proximity = TRUE)
  } else {
    rfModel = randomForest(dataset, outcome, ntree = trees, xtest = testset, ytest = testsetOutcome, 
                           importance = TRUE, proximity = TRUE)
  }
  
  print(plot(rfModel$err.rate[,1], type = "l")) #plot the oob error (y) for each tree (x)
  
}

plotRfVariableImportance = function(rfModel){ ### not yet correct!!!!
  print(barchart(rfModel$importance))
}