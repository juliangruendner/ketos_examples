splitData = function(seed, data){
  ## randomly splits data into training (0.8) and test (0.2) data
  set.seed(seed)
  training_index = createDataPartition(data[[ncol(data)]], p = 0.8, list = F)
  training = data[training_index,]
  test = data[-training_index,]
  
  return(list(training=training,test=test))
}

trainModelsCaret <- function(dataset = NULL, outcomeVar = NULL, metric = "Accuracy", control = NULL){
  # trains all the caret models according to control and metric
  
  retList <- list()
  
  form <- as.formula(paste(c(outcomeVar, "~."), collapse=" "))
  
  #set.seed(seed)
  print("start train GLMNET")
  set.seed(seed)
  retList$glmnet <- caret::train(form, data=dataset, method="glmnet", metric=metric, preProc=c("center", "scale"), trControl=control)
  
  # KNN (KNN - k-nearest-neighbour)
  print("start train KNN")
  set.seed(seed)
  retList$knn <- caret::train(form, data=dataset, method="knn", metric=metric, preProc=c("center", "scale"), trControl=control)
  
  # nnet (ANN - artificial neural network)
  print("start train ANN")
  set.seed(seed)
  retList$ann <- caret::train(form, data=dataset, method="nnet", metric=metric, trControl=control, trace=FALSE)
  
  #C5.0 (DT - decision tree)
  print("start train DT")
  set.seed(seed)
  retList$dt <- caret::train(form, data=dataset, method="C5.0", metric=metric, trControl=control)
  
  # RF (RF - random forest)
  print("start train RF")
  set.seed(seed)
  retList$rf <- caret::train(form, data=dataset, method="rf", metric=metric, trControl=control)
  
  return(retList)
  
}

createPredictModels <- function(dataset, modellist, packType = "caret"){
  
  ret <- list()
  dataForModel <- dataset
  
  for(model in modellist){
    
    if(packType == "mlr"){
      dataForModel <- makeSurvTask(data = dataset,target = model$task.desc$target)
      modelName <- model$learner$id
    } else {
      modelName <- model$method
      
    }
    
    ret[[modelName]] <- predict(model, dataForModel)
  }
  
  return(ret)
}


evalModelListsAccsDf <- function(modellist, data, outcomeVar, packType = "caret", evalMetric = "Accuracy"){
  
  Model <- c();
  Accuracy <- c();
  cnt <- 1
  modelNames <- names(modellist)
  
  for (model in modellist){
    
    if(packType == "caret") {
      confMat <- confusionMatrix(model, data[,outcomeVar])
      Model[cnt] <- modelNames[cnt] 
      Accuracy[cnt] <- confMat[[c("overall",evalMetric)]]
      
    } else {
      
      if(evalMetric == "Accuracy"){
        evalMetric <- acc
      }
      
      Model[cnt] <- modelNames[cnt] 
      Accuracy[cnt] <- performance(model, evalMetric)
    }
    
    
    cnt <- cnt + 1
  }
  
  if(packType == "caret"){
    return(data.frame(Model, Accuracy))
  } else {
    Cindex <- Accuracy
    return(data.frame(Model, Cindex))
  }
  
  
}