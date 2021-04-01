#' @export
makeRLearner.regr.embed_kerasff = function() {
  makeRLearnerRegr(
    cl = "regr.embed_kerasff",
    package = "keras",
    par.set = makeParamSet(
      makeDiscreteLearnerParam(id = "loss", default = "mean_squared_error",
        values = c("mean_squared_error", "mean_squared_logarithmic_error", "mean_absolute_error")),
      makeIntegerLearnerParam(id = "epochs", lower = 0L, default = 30L),
      makeIntegerLearnerParam(id = "early_stopping_patience", lower = 0L, default = 1L),
      makeDiscreteLearnerParam(id = "optimizer",  default = "adam",
        values = c("sgd", "rmsprop", "adagrad", "adadelta", "adam", "nadam")),
      makeNumericLearnerParam(id = "lr", lower = 0, upper = 1, default = 0.001),
      makeNumericLearnerParam(id = "decay", lower = 0, upper = 1, default = 0),
      makeNumericLearnerParam(id = "momentum", lower = 0, upper = 1, default = 0,
        requires = quote(optimizer == "sgd")),
      makeNumericLearnerParam(id = "rho", lower = 0, upper = 1, default = 0.001,
        requires = quote(optimizer == "rmsprop")),
      makeNumericLearnerParam(id = "beta_1", lower = 0, upper = 1, default = 0.9,
        requires = quote(optimizer %in% c("adam", "nadam"))),
      makeNumericLearnerParam(id = "beta_2", lower = 0, upper = 1, default = 0.999,
        requires = quote(optimizer %in% c("adam", "nadam"))),
      makeIntegerLearnerParam(id = "batch_size", lower = 1L, upper = Inf, default = 1L),
      makeIntegerLearnerParam(id = "n_layers", lower = 1L, upper = 4L, default = 1L),
      makeNumericLearnerParam(id = "dropout_rate", default = 0, lower = 0, upper = 1),
      makeIntegerLearnerParam(id = "embed_size", lower = 1, upper = Inf, default = NULL, special.vals = list(NULL)),
      makeNumericLearnerParam(id = "embed_dropout_rate", default = 0.05, lower = 0, upper = 1),
      # Neurons / Layers
      makeIntegerLearnerParam(id = "units_layer1", lower = 1L, default = 1L),
      makeIntegerLearnerParam(id = "units_layer2", lower = 1L, default = 1L,
        requires = quote(layers >= 2)),
      makeIntegerLearnerParam(id = "units_layer3", lower = 1L, default = 1L,
        requires = quote(layers >= 3)),
      makeIntegerLearnerParam(id = "units_layer4", lower = 1L, default = 1L,
        requires = quote(layers >= 4)),
      makeNumericLearnerParam(id = "validation_split", lower = 0, upper = 1, default = 0),
      makeUntypedLearnerParam(id = "callbacks", default = c()),
      makeLogicalLearnerParam(id = "use_batchnorm", default = FALSE),
      makeLogicalLearnerParam(id = "use_dropout", default = FALSE)
    ),
    properties = c("numerics", "factors"),
    par.vals = list(),
    name = "Keras Fully-Connected NN",
    short.name = "kerasff"
  )
}


trainLearner.regr.embed_kerasff  = function(.learner, .task, .subset, .weights = NULL,
  epochs = 10L, early_stopping_patience = 5L, batch_size = 256L,
  learning_rate_scheduler = TRUE, validation_split = 0.2,
  optimizer = "adam", lr = 0.001, beta_1 = 0.9, beta_2 = 0.999, decay = 0.01,
  momentum = 0, rho = 0.9, embed_size = NULL,
  embed_dropout_rate = 0.05, dropout_rate = 0.4,
  n_layers = 3L, loss = "mean_squared_error",
  units_layer1 = 512, units_layer2 = 256, units_layer3 = 128, units_layer4 = 64,
  tensorboard = FALSE, use_dropout = FALSE, use_batchnorm = FALSE,
  callbacks = c()) {

  require("keras")
  keras = reticulate::import("keras")
  input_shape = getTaskNFeats(.task)
  data = getTaskData(.task, .subset, target.extra = TRUE)

  optimizer = switch(optimizer,
    "sgd" = optimizer_sgd(lr, momentum, decay = decay),
    "rmsprop" = optimizer_rmsprop(lr, rho, decay = decay),
    "adagrad" = optimizer_adagrad(lr, decay = decay),
    "adam" = optimizer_adam(lr, beta_1, beta_2, decay = decay, clipnorm = 1),
    "nadam" = optimizer_nadam(lr, beta_1, beta_2, schedule_decay = decay)
  )

  if (early_stopping_patience > 0)
    callbacks = c(callbacks, callback_early_stopping(monitor = 'val_loss', patience = early_stopping_patience))

  if (tensorboard) callbacks = c(callbacks, callback_tensorboard())

  # --- Build Up Model -----------------------------------------------------------------------------
  units_layers = c(units_layer1, units_layer2, units_layer3, units_layer4)

  # The model consists of Embedding layers for the categorical variables,
  # followed by a Dropout of emb_drop, and a BatchNorm for the continuous variables.
  # The results are concatenated and followed by blocks of BatchNorm, Dropout,
  # Linear and ReLU (the first block skips BatchNorm and Dropout, the last block skips
  # the ReLU).
  # Keras requires "input" (= input layers) and "output" (additional layers)
  # for model construction.
  embedding = make_embedding_regr(data$data, embed_size = embed_size, embed_dropout = embed_dropout_rate)
  layers = embedding$layers
  for (i in seq_len(n_layers + 1L)) {
    if (i > 1) {
      if (use_batchnorm) layers = layers %>% layer_batch_normalization() %>%
      if (use_dropout) layers = layers %>% layer_dropout(dropout_rate)
    }
    if (i < n_layers + 1)
    layers = layers %>%
      layer_dense(units = units_layers[i]) %>%
      layer_activation_leaky_relu(alpha = 0.3)
    else
      layers = layers %>% layer_dense(units = 1L, activation = "linear")
  }
  model = keras_model(input = embedding$inputs, output = layers)

  # --- Compile and Fit ----------------------------------------------------------------------------
  # data has to be a list with 1 element per input. continuous vars are in a
  # list element "continuous".
  data = reshape_data_embedding_regr(data$data, data$target)

  model %>% compile(
    optimizer = optimizer,
    loss = loss,
    metrics = "mean_squared_error"
  )

  if (epochs > 0) {
    history = model %>% fit(
      x = data$data,
      y = as.numeric(data$label),
      batch_size = batch_size,
      epochs  = epochs,
      validation_split = validation_split,
      callbacks = callbacks
    )
  } else history = NULL

  return(list(model = model, history = history, data = data,
    history = history, fct_levels = data$fct_levels))
}

predictLearner.regr.embed_kerasff = function(.learner, .model, .newdata, ...) {
  newdata = reshape_data_embedding(.newdata, target = NULL)$data
  p = .model$learner.model$model %>% predict(as.matrix(.newdata))
  return(as.vector(p))
}

reshape_data_embedding_regr = function(data, target) {
  assert_numeric(target, null.ok = TRUE)
  assert_data_frame(data)
  type = BBmisc::vcapply(data, function(x) class(x)[[1]])
  embed_vars = type %in% c("ordered", "factor")

  fct_levels = lapply(as.list(data[, embed_vars]), function(x) levels(x))
  out_data = list()

  if (sum(embed_vars)  > 0)
    out_data = setNames(lapply(as.list(data[, embed_vars, drop = FALSE]), function(x) as.integer(x) - 1L), colnames(data)[embed_vars])
  if (sum(!embed_vars) > 0)
    out_data$continuous = as.matrix(data[, !embed_vars])

  if (is.null(target)) list(data = out_data, fct_levels = fct_levels)
  else list(
    data = out_data, fct_levels = fct_levels,
    label =  array(target, dim = c(nrow(data), 1))
    )
}

make_embedding_regr = function(data, embed_size = NULL, embed_dropout = 0) {
  assert_data_frame(data)
  assert_numeric(embed_size, null.ok = TRUE)
  assert_number(embed_dropout)
  type = BBmisc::vcapply(data, function(x) class(x)[[1]])
  embed_vars = type %in% c("ordered", "factor")
  n_cont = length(type[!embed_vars])

  # Embeddings for categorical variables
  embds = list()
  if (sum(embed_vars) > 0) {
    embds = Map(function(x, feat_name) {
      n_cat = length(levels(x))
      # Use heuristic from fast.ai https://github.com/fastai/fastai/blob/master/fastai/tabular/data.py
      if (length(embed_size) >= 2) embed_size = embed_size[feat_name]
      if (length(embed_size) == 0) embed_size = min(600L, round(1.6 * n_cat^0.56))
      input = layer_input(shape = 1, dtype = "int32", name = feat_name)
      layers = input %>%
      layer_embedding(input_dim = as.numeric(n_cat), output_dim = as.numeric(embed_size), input_length = 1, name = paste0("embed_", feat_name)) %>%
      layer_dropout(embed_dropout, input_shape = as.numeric(embed_size)) %>%
      layer_flatten()
      return(list(input = input, layers = layers))
    }, data[, embed_vars, drop = FALSE], names(type[embed_vars]))
  }
  # Layer for the continuous variables
  if (n_cont > 0) {
    input = layer_input(shape = n_cont, dtype = "float32", name = "continuous")
    layers = input %>% layer_batch_normalization(input_shape = n_cont, axis = -1)
    embds = c(embds, list(cont = list(input = input, layers = layers)))
  }

  # Concatenate in case
  if (length(embds) >= 2)
    layers = layer_concatenate(unname(lapply(embds, function(x) x$layers)))
  else
    layers = unname(embds[[1]]$layers)
   return(list(inputs = lapply(embds, function(x) x$input), layers = layers))
}

get_embeddings_regr = function(model) {
  assert_class(model, "WrappedModel")
  learner_model = mlr:::getLearnerModel(model, more.unwrap = TRUE)
  model = learner_model$model
  fct_levels = learner_model$fct_levels
  names(fct_levels) = paste0("embed_", names(fct_levels))

  layers = sapply(model$layers, function(x) x$name)
  embed_layers = layers[grepl("embed", layers)]

  wts = setNames(lapply(embed_layers,
    function(layer) {
      wt = get_layer(model, layer)$get_weights()[[1]]
      colnames(wt) = paste0(layer, seq_len(ncol(wt)))
      rownames(wt) = fct_levels[[layer]]
      return(wt)
    }), embed_layers)
  return(wts)
}

#' Return data with embeddings.
#' @param model mlr mdoel
#' @param data data.frame, getTaskData(task)
#' @param na_level Level for missing values chosen in the embedding.
#' @return data.frame data with levels instead of features
embed_with_model_regr = function(model, data, na_level = "_NA_") {
  assert_data_frame(data)
  assert_class(model, "WrappedModel")
  assert_string(na_level)

  wts = get_embeddings_regr(model)
  lst = lapply(names(wts), function(x) {
    fct = data[, gsub("embed_", "", x)]
    fct = addNA(fct)
    levels(fct)[is.na(levels(fct))] = na_level
    wts[[x]][fct, ]
  })
  data = data[, - which(colnames(data) %in% gsub("embed_", "", names(wts)))]
  data = cbind(data, do.call("cbind", lst))
  return(data)
}


#' @title Construct a CPO for dataset preprocessing, neural network embeddings creation and transformation
#'
#' @template cpo_doc_intro
#'
#' @description
#'
#' Preprocesses the data, handling the NA values. Learns neural network embeddings for the categorical features in the dataset.
#' Transforms all the categorical features to their embedded versions.
#'
#' @param embed_size
#'   The number of numeric values the embeddings will have. The higher this parameter,
#'   the more complex the embedding and the more time-consuming the process to create them.
#'
#' @param epochs
#'   Specifies the number of epochs for training the neural network.
#'
#' @template cpo_doc_outro
cpoEmbedregr = makeCPO(
  cpo.name = "embed_regr",
  par.set = pSS(
   lr = (3*10^-4): numeric [0, 1],
   epochs = 2: integer [0, Inf],
   embed_size = NULL: integer [1, Inf] [[special.vals = list(NULL)]]
  ),
  packages = c("keras"),
  fix.factors = TRUE,
  dataformat = "task",
  properties.target = c("regr"),
  properties.data = c("numerics", "factors", "ordered"),
  properties.adding = c("factors", "ordered"),
  properties.needed = "numerics",
  ################################################################
  cpo.train = {
   assertString(target)
   lrn = makeLearner("regr.embed_kerasff", lr = lr, embed_size = embed_size, epochs = epochs)
   train(lrn, data)
   },
  ################################################################
  cpo.retrafo = {
    embed_with_model_regr(control, data)
  }
)
