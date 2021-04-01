# functions to build the pipeline, the learners, and the resampling

constructPipeline = function(lrn.id, task, encoder) {
  lrn = constructLearnerCPO(lrn.id, task)
  pipeline = 
    # impute all missing values and new levels
    cpoImputeAll(classes = list(numeric = imputeMean(), factor = imputeConstant("..NA..")), 
      impute.new.levels = TRUE, recode.factor.levels = TRUE) %>>%
    # add encoding
    encoder$cpo %>>% 
    # remove constant features
    cpoDropConstants() %>>%
    # one-hot encode the remaining factors with nlevels < high.card.thresh
    cpoDummyEncode(reference.cat = FALSE) %>>%
    # avoid strange bugs with some learners (liquidsvm)
    cpoStandardizeFeatNames() %>>%
    # add learner
    lrn$cpo 
  pipeline_ps = c(lrn$ps, encoder$ps)
  return(list(cpo = pipeline, ps = pipeline_ps))
}


constructLearnerCPO = function(lrn.id, task) {
  task_type = getTaskType(task)
  pred_type = ifelse(task_type == "regr", "response", "prob")
  lrn_id = paste(task_type, lrn.id, sep = ".")
  switch(lrn.id,
    featureless = {
      par_vals = list()
      lrn = makeLearner(lrn_id, predict.type = pred_type, par.vals = par_vals)
      ps = makeParamSet()
    },
    cvglmnet = {
      par_vals = list(alpha = 1, nfolds = 5, nlambda = 100, s = "lambda.min", 
        intercept = TRUE, standardize = TRUE)
      lrn = makeLearner(lrn_id, predict.type = pred_type, par.vals = par_vals)
      ps = makeParamSet()
    },
    kknn = {
      par_vals = list(k = 15, distance = 2, kernel = "rectangular", scale = TRUE)
      lrn = makeLearner(lrn_id, predict.type = pred_type, par.vals = par_vals)
      lrn = cpoFilterFeatures(method = "FSelectorRcpp_information.gain", abs = 25) %>>% lrn
      ps = makeParamSet()
    },
    ranger = {
      par_vals = list(num.trees = 500, respect.unordered.factors = "order", 
        oob.error = FALSE, save.memory = FALSE, num.threads = 1)
      lrn = makeLearner(lrn_id, predict.type = pred_type, par.vals = par_vals)
      ps = makeParamSet()
    },
    liquidSVM = {
      par_vals = list(kernel = "gauss_rbf", useCells = TRUE, partition_choice = 6,
        threads = 1, scale = TRUE, clipping = -1.0)
      require(liquidSVM)
      lrn = makeLearner(lrn_id, predict.type = pred_type, par.vals = par_vals)
      ps = makeParamSet()
    },
    xgboost.earlystop.wrap = {
      par_vals = list(nthread = 1, eta = 0.01, early_stopping_split = 0.8, 
        early_stopping_rounds = 10, max.nrounds = 6000)
      lrn = makeLearner(lrn_id, predict.type = pred_type, par.vals = par_vals)
      ps = makeParamSet()
    },
    keras = {
      par_vals = list(optimizer = "adam", beta_1 = 0.9, beta_2 = 0.999, 
        batch_size = 128L, decay = 0.01, epochs = 100, early_stopping_patience = 5, 
        lr = 0.003, l1_reg_layer = 0, l2_reg_layer = 0.01,
        act_layer = "relu", validation_split = 0.2, 
        layers = 2, units_layer1 = 256, units_layer2 = 256)
      lrn_id_keras = ifelse(task_type == "regr", "regr.kerasff", "classif.kerasff")
      lrn = cpoScale() %>>% makeLearner(lrn_id_keras, predict.type = pred_type, par.vals = par_vals)
      ps = makeParamSet()
    }
  )
  return(list(cpo = lrn, ps = ps))
}


resamplePipeline = function(task, pipeline, pipeline.ps) {
  if (!isEmpty(pipeline.ps)) {
    pipeline = makeTuneWrapper(learner = pipeline, 
    resampling = if (getTaskType(task) == "regr") {
      INNER_REGR_RDESC
    } else {
      INNER_CLASSIF_RDESC
    },
    measures = if (getTaskType(task) == "regr") {
      INNER_REGR_MES
    } else if (length(getTaskDesc(task)$class.levels) > 2) {
      INNER_MULTI_CLASSIF_MES
    } else {
      INNER_TWO_CLASSIF_MES
    }, 
    par.set = pipeline.ps, 
    control = CONTROL)
  }
  
  resample(learner = pipeline, task = task, 
    resampling = if (getTaskType(task) == "regr") {
      REGR_RDESC
    } else {
      CLASSIF_RDESC
    },
    measures = if (getTaskType(task) == "regr") {
      REGR_MES
    } else if (length(getTaskDesc(task)$class.levels) > 2) {
      MULTI_CLASSIF_MES
    } else {
      TWO_CLASSIF_MES
    },
    models = KEEP_MODELS, keep.pred = KEEP_PREDS, extract = extractModelInfos)
}

# helper functions

extractModelInfos = function(model) {
  # catch NoFeaturesModels
  if (class(getLearnerModel(model,more.unwrap = TRUE)) == "NoFeaturesModel") {
    return(list(num_feats = 0, hyper_pars = NULL))
  }
  
  tune_res = if (inherits(model, 'TuneModel')) {getTuneResult(model)[c("x", "y")]} else {NULL} 
  features = model$learner.model$next.model$features
  lrn_id = getLearnerId(model$learner.model$next.model$learner)
  model = model$learner.model$next.model$learner.model
  hyper_pars = if (grepl("cvglmnet", lrn_id)) {
    list(lambda.min = model$lambda.min)
  } else if (grepl("xgboost.earlystop.wrap", lrn_id)) {
    list(best_iteration = model$best_iteration)
  } else if (grepl("liquidSVM", lrn_id)) {
    list(select_errors = model$select_errors)
  } else {
    NULL
  }

  list(tune_res = tune_res, num_feats = length(features), # extract the final number of features
    hyper_pars = hyper_pars)
}