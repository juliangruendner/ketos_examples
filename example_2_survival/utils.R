splitData = function(seed, data){
  ## randomly splits data into training (0.8) and test (0.2) data
  set.seed(seed)
  training_index = createDataPartition(data[[ncol(data)]], p = 0.8, list = F)
  training = data[training_index,]
  test = data[-training_index,]
  
  return(list(training=training,test=test))
}

trainModelsMlr <- function(dataset = NULL, outcomeVars = NULL, measures = cindex){
  # available models see: https://mlr-org.github.io/mlr-tutorial/devel/html/integrated_learners/index.html#survival-analysis-15
  retList <- list()
  
  #create the survival task and resampling strategy
  learnTask = makeSurvTask(data = dataset, target = outcomeVars)
  resampleDesc = makeResampleDesc("RepCV", reps = 10)  #using 10-fold cv
  
  #build model for randomForestSrc
  tM.rfsrc.learner <- makeLearner("surv.randomForestSRC",id="rfsrc", predict.type = "response", par.vals = list(nsplit=10))
  
  # set tunable params
  survTunParams <- makeParamSet(
    makeIntegerParam("ntree",lower = 100, upper = 1000),
    makeIntegerParam("nsplit", lower = 10, upper = 10)    # what if param not listed here ..will it be optimized then?
  )
  
  print("tune rfsrc")
  survRanCtrl <- makeTuneControlRandom(maxit = 1L)
  rfsrc_tune <- tuneParams(learner = tM.rfsrc.learner, resampling = resampleDesc, task = learnTask, par.set = survTunParams, control = survRanCtrl, measures = measures)  # use cindex for survival data later  - see also listMeasures()
  
  print("train rfsrc")
  tM.rfsrc.ModelToTrain <- setHyperPars(tM.rfsrc.learner, par.vals = rfsrc_tune$x)
  tM.rfsrc = train(tM.rfsrc.ModelToTrain, learnTask)
  
  print("train coxph")
  tM.coxph.learner <- makeLearner("surv.coxph",id="coxph", predict.type = "response")
  tM.coxph.ModelToTrain <- setHyperPars(tM.coxph.learner)
  
  tM.coxph = train(tM.coxph.ModelToTrain, learnTask)
  
  print("tune glmnet")
  #set tunable params
  tM.glmnet.learner <- makeLearner("surv.glmnet",id="glmnet", predict.type = "response")
  
  survTunParams <- makeParamSet(
    makeNumericParam("alpha", lower = 0, upper = 1)    # what if param not listed here ..will it be optimized then?
  )
  
  glmnet_tune <- tuneParams(learner = tM.glmnet.learner, resampling = resampleDesc, task = learnTask, par.set = survTunParams, control = survRanCtrl, measures = measures)  # use cindex for survival data later  - see also listMeasures()
  
  tM.glmnet.ModelToTrain <- setHyperPars(tM.glmnet.learner, par.vals = glmnet_tune$x)
  print("train glmnet")
  tM.glmnet = train(tM.glmnet.ModelToTrain, learnTask)
  
  
  #add models to ret list
  retList$rfsrc = tM.rfsrc
  retList$coxph = tM.coxph
  retList$glmnet = tM.glmnet
  
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