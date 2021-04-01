# adapted from https://github.com/ja-thomas/autoxgboost/tree/master/R
# replace param early_stopping_data with early_stopping_split to automatically select
# the early_stopping data based on the current training set
# NOTE: the learners cannot handle weights any more

makeRLearner.classif.xgboost.earlystop.wrap = function() {
  makeRLearnerClassif(
    cl = "classif.xgboost.earlystop.wrap",
    package = "xgboost",
    par.set = makeParamSet(
      makeNumericLearnerParam(id = "eta", default = 0.3, lower = 0, upper = 1),
      makeNumericLearnerParam(id = "gamma", default = 0, lower = 0),
      makeIntegerLearnerParam(id = "max_depth", default = 6L, lower = 1L),
      makeNumericLearnerParam(id = "min_child_weight", default = 1, lower = 0),
      makeNumericLearnerParam(id = "subsample", default = 1, lower = 0, upper = 1),
      makeNumericLearnerParam(id = "colsample_bytree", default = 1, lower = 0, upper = 1),
      makeNumericLearnerParam(id = "colsample_bylevel", default = 1, lower = 0, upper = 1),
      makeIntegerLearnerParam(id = "num_parallel_tree", default = 1L, lower = 1L),
      makeNumericLearnerParam(id = "lambda", default = 0, lower = 0),
      makeNumericLearnerParam(id = "lambda_bias", default = 0, lower = 0),
      makeNumericLearnerParam(id = "alpha", default = 0, lower = 0),
      makeUntypedLearnerParam(id = "objective", default = NULL, tunable = FALSE),
      makeUntypedLearnerParam(id = "eval_metric", default = NULL, tunable = FALSE),
      makeNumericLearnerParam(id = "base_score", default = 0.5, tunable = FALSE),
      makeNumericLearnerParam(id = "early_stopping_split", default = 0.8, lower = 0, upper = 1),
      makeIntegerLearnerParam(id = "early_stopping_rounds", default = 1, lower = 1L, tunable = FALSE),
      makeIntegerLearnerParam(id = "max.nrounds", default = 10^6L, lower = 1L, upper = 10^7L),
      makeLogicalLearnerParam(id = "maximize", default = NULL, special.vals = list(NULL), tunable = FALSE),
      makeNumericLearnerParam(id = "scale_pos_weight", lower = 0),
      makeIntegerLearnerParam(id = "nthread", lower = 1L, tunable = FALSE),
      makeDiscreteLearnerParam(id = "booster", default = "gbtree", values = c("gbtree", "gblinear", "dart")),
      makeDiscreteLearnerParam(id = "sample_type", default = "uniform", values = c("uniform", "weighted"), requires = quote(booster == "dart")),
      makeDiscreteLearnerParam(id = "normalize_type", default = "tree", values = c("tree", "forest"), requires = quote(booster == "dart")),
      makeNumericLearnerParam(id = "rate_drop", default = 0, lower = 0, upper = 1, requires = quote(booster == "dart")),
      makeNumericLearnerParam(id = "skip_drop", default = 0, lower = 0, upper = 1, requires = quote(booster == "dart")),
      makeLogicalLearnerParam(id = "one_drop", default = FALSE, requires = quote(booster == "dart")),
      makeDiscreteLearnerParam(id = "tree_method", default = "exact", values = c("exact", "hist"), requires = quote(booster != "gblinear")),
      makeDiscreteLearnerParam(id = "grow_policy", default = "depthwise", values = c("depthwise", "lossguide"), requires = quote(tree_method == "hist")),
      makeIntegerLearnerParam(id = "max_leaves", default = 0L, lower = 0L, requires = quote(grow_policy == "lossguide")),
      makeIntegerLearnerParam(id = "max_bin", default = 256L, lower = 2L, requires = quote(tree_method == "hist"))
    ),
    properties = c("twoclass", "multiclass", "numerics", "prob", "missings"),
    name = "eXtreme Gradient Boosting",
    short.name = "xgboost.earlystop.wrap",
    note = ""
  )
}

trainLearner.classif.xgboost.earlystop.wrap = function(.learner, .task, .subset, 
  objective = NULL, eval_metric = NULL, early_stopping_rounds = 1, max.nrounds = 10^6, 
  early_stopping_split = 0.8, scale_pos_weight, nthread, ...) {

  td = getTaskDesc(.task)
  nc = length(td$class.levels)
  parlist = list(...)

  if (is.null(eval_metric))
    eval_metric = ifelse(nc == 2L, "error", "merror")
  parlist$eval_metric = eval_metric

  # subset Task based on .subset
  .task = subsetTask(.task, .subset)
  # take split * .task as early.stopping.data
  rinst = makeResampleInstance(makeResampleDesc("Holdout", split = early_stopping_split,
      stratify = TRUE), task = .task)
  early.stopping.data = subsetTask(.task, subset = rinst$test.inds[[1]])

  watchlist = list(eval = createDMatrixFromTask(early.stopping.data))

  data = createDMatrixFromTask(subsetTask(.task, subset = rinst$train.inds[[1]]))
  
  if (is.null(objective))
    objective = ifelse(nc == 2L, "binary:logistic", "multi:softprob")

  if (.learner$predict.type == "prob" && objective == "multi:softmax")
    stop("objective = 'multi:softmax' does not work with predict.type = 'prob'")

  if (objective %in% c("multi:softprob", "multi:softmax"))
    parlist$num_class = nc

  if (!missing(nthread)) {
    mod = xgboost::xgb.train(params = parlist, data = data, nrounds = max.nrounds, watchlist = watchlist,
      objective = objective, early_stopping_rounds = early_stopping_rounds, silent = 1L, verbose = 0L, nthread = nthread)
  } else {
    mod = xgboost::xgb.train(params = parlist, data = data, nrounds = max.nrounds, watchlist = watchlist,
      objective = objective, early_stopping_rounds = early_stopping_rounds, silent = 1L, verbose = 0L)
  }

  return(mod)
}

predictLearner.classif.xgboost.earlystop.wrap = function(.learner, .model, .newdata, ...) {
  td = .model$task.desc
  m = .model$learner.model
  cls = td$class.levels
  nc = length(cls)
  obj = .learner$par.vals$objective

  if (is.null(obj))
    .learner$par.vals$objective = ifelse(nc == 2L, "binary:logistic", "multi:softprob")

  p = predict(m, newdata = data.matrix(convertDataFrameCols(.newdata, ints.as.num = TRUE)), ...)

  if (nc == 2L) { #binaryclass
    if (.learner$par.vals$objective == "multi:softprob") {
      y = matrix(p, nrow = length(p) / nc, ncol = nc, byrow = TRUE)
      colnames(y) = cls
    } else {
      y = matrix(0, ncol = 2, nrow = nrow(.newdata))
      colnames(y) = cls
      y[, 1L] = 1 - p
      y[, 2L] = p
    }
    if (.learner$predict.type == "prob") {
      return(y)
    } else {
      p = colnames(y)[max.col(y)]
      names(p) = NULL
      p = factor(p, levels = colnames(y))
      return(p)
    }
  } else { #multiclass
    if (.learner$par.vals$objective  == "multi:softmax") {
      return(factor(p, levels = cls)) #special handling for multi:softmax which directly predicts class levels
    } else {
      p = matrix(p, nrow = length(p) / nc, ncol = nc, byrow = TRUE)
      colnames(p) = cls
      if (.learner$predict.type == "prob") {
        return(p)
      } else {
        ind = max.col(p)
        cns = colnames(p)
        return(factor(cns[ind], levels = cns))
      }
    }
  }
}


makeRLearner.regr.xgboost.earlystop.wrap = function() {
  makeRLearnerRegr(
    cl = "regr.xgboost.earlystop.wrap",
    package = "xgboost",
    par.set = makeParamSet(
      makeNumericLearnerParam(id = "eta", default = 0.3, lower = 0, upper = 1),
      makeNumericLearnerParam(id = "gamma", default = 0, lower = 0),
      makeIntegerLearnerParam(id = "max_depth", default = 6L, lower = 1L),
      makeNumericLearnerParam(id = "min_child_weight", default = 1, lower = 0),
      makeNumericLearnerParam(id = "subsample", default = 1, lower = 0, upper = 1),
      makeNumericLearnerParam(id = "colsample_bytree", default = 1, lower = 0, upper = 1),
      makeNumericLearnerParam(id = "colsample_bylevel", default = 1, lower = 0, upper = 1),
      makeIntegerLearnerParam(id = "num_parallel_tree", default = 1L, lower = 1L),
      makeNumericLearnerParam(id = "lambda", default = 0, lower = 0),
      makeNumericLearnerParam(id = "lambda_bias", default = 0, lower = 0),
      makeNumericLearnerParam(id = "alpha", default = 0, lower = 0),
      makeUntypedLearnerParam(id = "objective", default = NULL, tunable = FALSE),
      makeUntypedLearnerParam(id = "eval_metric", default = NULL, tunable = FALSE),
      makeNumericLearnerParam(id = "base_score", default = 0.5, tunable = FALSE),
      makeNumericLearnerParam(id = "early_stopping_split", default = 0.8, lower = 0, upper = 1),
      makeIntegerLearnerParam(id = "early_stopping_rounds", default = 1, lower = 1L, tunable = FALSE),
      makeIntegerLearnerParam(id = "max.nrounds", default = 10^6L, lower = 1L, upper = 10^7L),
      makeLogicalLearnerParam(id = "maximize", default = NULL, special.vals = list(NULL), tunable = FALSE),
      makeIntegerLearnerParam(id = "nthread", lower = 1L, tunable = FALSE),
      makeDiscreteLearnerParam(id = "booster", default = "gbtree", values = c("gbtree", "gblinear", "dart")),
      makeDiscreteLearnerParam(id = "sample_type", default = "uniform", values = c("uniform", "weighted"), requires = quote(booster == "dart")),
      makeDiscreteLearnerParam(id = "normalize_type", default = "tree", values = c("tree", "forest"), requires = quote(booster == "dart")),
      makeNumericLearnerParam(id = "rate_drop", default = 0, lower = 0, upper = 1, requires = quote(booster == "dart")),
      makeNumericLearnerParam(id = "skip_drop", default = 0, lower = 0, upper = 1, requires = quote(booster == "dart")),
      makeLogicalLearnerParam(id = "one_drop", default = FALSE, requires = quote(booster == "dart")),
      makeDiscreteLearnerParam(id = "tree_method", default = "exact", values = c("exact", "hist"), requires = quote(booster != "gblinear")),
      makeDiscreteLearnerParam(id = "grow_policy", default = "depthwise", values = c("depthwise", "lossguide"), requires = quote(tree_method == "hist")),
      makeIntegerLearnerParam(id = "max_leaves", default = 0L, lower = 0L, requires = quote(grow_policy == "lossguide")),
      makeIntegerLearnerParam(id = "max_bin", default = 256L, lower = 2L, requires = quote(tree_method == "hist"))
    ),
    properties = c("numerics", "missings"),
    name = "eXtreme Gradient Boosting",
    short.name = "xgboost.earlystop.wrap",
    note = ""
  )
}

trainLearner.regr.xgboost.earlystop.wrap = function(.learner, .task, .subset, 
  objective = NULL, eval_metric = NULL, early_stopping_rounds = 1, max.nrounds = 10^6, 
  early_stopping_split = 0.8, nthread, ...) {

  parlist = list(...)
  if (is.null(eval_metric))
    eval_metric = "rmse"
  parlist$eval_metric = eval_metric

  # subset Task based on .subset
  .task = subsetTask(.task, .subset)
  # take split * .task as early.stopping.data
  rinst = makeResampleInstance(makeResampleDesc("Holdout", split = early_stopping_split,
      stratify = FALSE), task = .task)
  early.stopping.data = subsetTask(.task, subset = rinst$test.inds[[1]])
  
  watchlist = list(eval = createDMatrixFromTask(early.stopping.data))

  data = createDMatrixFromTask(subsetTask(.task, subset = rinst$train.inds[[1]]))

  if (is.null(objective))
    objective = "reg:linear"

  if (!missing(nthread)) {
    mod = xgboost::xgb.train(params = parlist, data = data, nrounds = max.nrounds, watchlist = watchlist,
      objective = objective, early_stopping_rounds = early_stopping_rounds, silent = 1L, verbose = 0L, nthread = nthread)
  } else {
      mod = xgboost::xgb.train(params = parlist, data = data, nrounds = max.nrounds, watchlist = watchlist,
      objective = objective, early_stopping_rounds = early_stopping_rounds, silent = 1L, verbose = 0L)
  }

  return(mod)
}

predictLearner.regr.xgboost.earlystop.wrap = function(.learner, .model, .newdata, ...) {
  m = .model$learner.model
  p = predict(m, newdata = data.matrix(convertDataFrameCols(.newdata, ints.as.num = TRUE)), ...)
}

# helper functions
createDMatrixFromTask = function(task, weights = NULL) {

  data = getTaskData(task, target.extra = TRUE)
  data$data = convertDataFrameCols(data$data, ints.as.num = TRUE)
  if (getTaskType(task) == "classif")  {
    cl = getTaskClassLevels(task)
    data$target =  match(as.character(data$target), cl) - 1
  }

  if (!is.null(weights))
    xgboost::xgb.DMatrix(data = data.matrix(data$data), label = data$target, weight = weights)
  else if (!is.null(task$weights))
    xgboost::xgb.DMatrix(data = data.matrix(data$data), label = data$target, weight = task$weights)
  else
    xgboost::xgb.DMatrix(data = data.matrix(data$data), label = data$target)
}