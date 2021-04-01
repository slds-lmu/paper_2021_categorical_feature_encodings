# copied from https://github.com/liquidSVM/liquidSVM/blob/03df5c8841dac0466cb7a7f487e44e82858832f6/bindings/R/liquidSVM/R/mlr.R
# change the names to liquidsvm to avoid strange clashes with the mlr-integrated liquidSVM learners

commonParamSet <- function() {
  if(!requireNamespace('mlr', quietly=TRUE)) stop("this function needs mlr to be installed")
  if(!requireNamespace('ParamHelpers', quietly=TRUE)) stop("this function needs ParamHelpers to be installed")
  ParamHelpers::makeParamSet(
    ParamHelpers::makeLogicalLearnerParam(id = "scale", default = TRUE),
    ParamHelpers::makeDiscreteLearnerParam(id = "kernel", default = "gauss_rbf",
                                           values = c("gauss_rbf","poisson")),
    ParamHelpers::makeIntegerLearnerParam(id = "partition_choice", default = 0, lower = 0, upper = 6),
    ParamHelpers::makeNumericLearnerParam(id = "partition_param", default = -1,
                                          requires = quote(partition_choice >= 1L)),
    ParamHelpers::makeIntegerLearnerParam(id = "grid_choice", default = 0, lower = -2, upper = 2),
    ParamHelpers::makeIntegerLearnerParam(id = "folds", default = 5, lower = 1),
    ParamHelpers::makeNumericLearnerParam(id = "min_gamma", lower=0),
    ParamHelpers::makeNumericLearnerParam(id = "max_gamma", lower=0,requires = quote(min_gamma <= max_gamma)),
    ParamHelpers::makeIntegerLearnerParam(id = "gamma_steps", lower=0),
    ParamHelpers::makeNumericLearnerParam(id = "min_lambda", lower=0),
    ParamHelpers::makeNumericLearnerParam(id = "max_lambda", lower=0,requires = quote(min_lambda <= max_lambda)),
    ParamHelpers::makeIntegerLearnerParam(id = "lambda_steps", lower=0),
    ParamHelpers::makeDiscreteLearnerParam(id = "retrain_method", default = "select_on_each_fold",
                                           values = c("select_on_entire_train_Set","select_on_each_fold")),
    ParamHelpers::makeLogicalLearnerParam(id = "store_solutions_internally", default = TRUE),
    ParamHelpers::makeIntegerLearnerParam(id = "display", default = getOption("liquidSVM.default.display",0), lower = 0, upper=7),
    ParamHelpers::makeIntegerLearnerParam(id = "threads", default = getOption("liquidSVM.default.threads",0), lower = -1)
  )
}

makeRLearner.regr.liquidsvm <- function() {
  if(!requireNamespace('mlr', quietly=TRUE)) stop("this function needs mlr to be installed")
  if(!requireNamespace('ParamHelpers', quietly=TRUE)) stop("this function needs ParamHelpers to be installed")
  mlr::makeRLearnerRegr(
    cl = "regr.liquidsvm",
    package = "liquidSVM",
    par.set = commonParamSet(),
    #par.vals = list(fit = FALSE),
    properties = c("numerics", "factors"),
    name = "Support Vector Machines",
    short.name = "liquidsvm",
    note = "FIXME make integrated cross-validation more accessible."
  )
}

trainLearner.regr.liquidsvm <- function(.learner, .task, .subset, .weights = NULL,
                                       partition_choice=0, partition_param=-1,
                                       ...) {
  if(!requireNamespace('mlr', quietly=TRUE)) stop("this function needs mlr to be installed")
  f = mlr::getTaskFormula(.task)
  if(partition_param > 0) partition_choice <- c(partition_choice, partition_param)
  data <- mlr::getTaskData(.task, .subset)
  liquidSVM::lsSVM(f, data, partition_choice=partition_choice, clipping = -1, ...)
}


predictLearner.regr.liquidsvm <- function(.learner, .model, .newdata, ...) {
  if(!requireNamespace('mlr', quietly=TRUE)) stop("this function needs mlr to be installed")
  liquidSVM:::predict.liquidSVM(.model$learner.model, newdata = .newdata, ...)#[, 1L]
}


makeRLearner.classif.liquidsvm <- function() {
  if(!requireNamespace('mlr', quietly=TRUE)) stop("this function needs mlr to be installed")
  if(!requireNamespace('ParamHelpers', quietly=TRUE)) stop("this function needs ParamHelpers to be installed")
  mlr::makeRLearnerClassif(
    cl = "classif.liquidsvm",
    package = "liquidSVM",
    par.set = c(commonParamSet(), ParamHelpers::makeParamSet(
      ParamHelpers::makeDiscreteLearnerParam(id = "mc_type", default = "AvA_hinge",
                              values =  c("AvA_hinge", "OvA_ls", "OvA_hinge", "AvA_ls")),
      ParamHelpers::makeNumericVectorLearnerParam(id = "weights", len = NA_integer_, lower = 0)
    )),
    #par.vals = list(fit = FALSE),
    properties = c("twoclass", "multiclass", "numerics", "factors", "prob", "class.weights"),
    class.weights.param = "weights",
    name = "Support Vector Machines",
    short.name = "liquidsvm",
    note = "FIXME make integrated cross-validation more accessible."
  )
}

trainLearner.classif.liquidsvm <- function(.learner, .task, .subset, .weights = NULL, #scaled, clip, kernel,
                                            partition_choice=0, partition_param=-1, #grid_choice, folds,
                                            ...) {
  if(!requireNamespace('mlr', quietly=TRUE)) stop("this function needs mlr to be installed")
  if(partition_param > 0) partition_choice <- c(partition_choice, partition_param)
  f <-  mlr::getTaskFormula(.task)
  data <- mlr::getTaskData(.task, .subset)
  predict.prob <- (.learner$predict.type=="prob")
  liquidSVM::mcSVM(f, data, partition_choice=partition_choice, predict.prob=predict.prob, clipping = -1, ...)
}


predictLearner.classif.liquidsvm <- function(.learner, .model, .newdata, ...) {
  if(!requireNamespace('mlr', quietly=TRUE)) stop("this function needs mlr to be installed")
  m <- .model$learner.model
  ret <- liquidSVM:::predict.liquidSVM(m, newdata = .newdata, ...)
  if(.learner$predict.type=="prob"){
    ret <- as.matrix(ret)
    if(all(ret>=.5))
      warning("")
    ws_type <- liquidSVM::getConfig(m, "WS_TYPE")
    if(ws_type==0){ ## binary classification
      colnames(ret) <- .model$task.desc$class.levels
    }else if(ws_type==2){ ## OvA
      colnames(ret) <- .model$task.desc$class.levels
    }else if(ws_type==1){ ## AvA
      warning("You choose mc_type='AvA' which gives not class probabilities but comparison probabilities")
    }
  }
  ret
}

registerS3method("makeRLearner", "regr.liquidsvm", 
  makeRLearner.regr.liquidsvm)
registerS3method("trainLearner", "regr.liquidsvm", 
  trainLearner.regr.liquidsvm)
registerS3method("predictLearner", "regr.liquidsvm", 
  predictLearner.regr.liquidsvm)

registerS3method("makeRLearner", "classif.liquidsvm", 
  makeRLearner.classif.liquidsvm)
registerS3method("trainLearner", "classif.liquidsvm", 
  trainLearner.classif.liquidsvm)
registerS3method("predictLearner", "classif.liquidsvm", 
  predictLearner.classif.liquidsvm)
