# algorithm function for each encoding

resampleWithDummyEncoder = function(job, data, instance, lrn.id, ...) {

  encoder = list(
    cpo = cpoCollapseFact2() %>>%  
      cpoDummyEncode(),
    ps = makeParamSet(makeLogicalParam("dummyencode.reference.cat"),
      makeDiscreteParam("collapse.fact.2.n.levels", values = HIGH_CARD_THRESH))
  )
  
  pipeline = constructPipeline(lrn.id, instance, encoder)
  resamplePipeline(instance, pipeline$cpo, pipeline$ps)
}

resampleWithIntegerEncoder = function(job, data, instance, lrn.id, ...) {

  caseAsNumeric2 = makeCPOCase(pSS(high.card.thresh: integer),
    properties.needed = "numerics",
    cpo.build = function (data, target, high.card.thresh) {
      high_card_feats = getHighCardFeats(instance, high.card.thresh)
      cpoAsNumeric2(affect.names = high_card_feats)})
  
  encoder = list(
    cpo = caseAsNumeric2(),
    ps = makeParamSet(makeDiscreteParam("case.high.card.thresh", 
      values = HIGH_CARD_THRESH))
  )

  pipeline = constructPipeline(lrn.id, instance, encoder)
  resamplePipeline(instance, pipeline$cpo, pipeline$ps)
}

resampleWithFrequencyEncoder = function(job, data, instance, lrn.id, ...) {

  caseFreqEncode = makeCPOCase(pSS(high.card.thresh: integer), 
    properties.needed = "numerics",
    cpo.build = function (data, target, high.card.thresh) {
      high_card_feats = getHighCardFeats(instance, high.card.thresh)
      cpoFreqEncode(rank = RANK, affect.names = high_card_feats)})
  
  encoder = list(
    cpo = caseFreqEncode(),
    ps = makeParamSet(makeDiscreteParam("case.high.card.thresh", 
      values = HIGH_CARD_THRESH))
  )
  
  pipeline = constructPipeline(lrn.id, instance, encoder)
  resamplePipeline(instance, pipeline$cpo, pipeline$ps)
}

resampleWithHashEncoder = function(job, data, instance, lrn.id, ...) {

  caseHashEncode = makeCPOCase(pSS(high.card.thresh: integer), 
    properties.needed = "numerics",
    cpo.build = function (data, target, high.card.thresh) {
      high_card_feats = getHighCardFeats(instance, high.card.thresh)
      cpoHashEncode(hash.size = high.card.thresh, affect.names = high_card_feats)})
  
  encoder = list(
    cpo = caseHashEncode(),
    ps = makeParamSet(makeDiscreteParam("case.high.card.thresh", 
      values = HIGH_CARD_THRESH))
  )
  
  pipeline = constructPipeline(lrn.id, instance, encoder)
  resamplePipeline(instance, pipeline$cpo, pipeline$ps)
}

resampleWithMultivHashEncoder = function(job, data, instance, lrn.id, ...) {

  caseMultivHashEncode = makeCPOCase(pSS(high.card.thresh: integer), 
    properties.needed = "numerics",
    cpo.build = function (data, target, high.card.thresh) {
      high_card_feats = getHighCardFeats(instance, high.card.thresh)
      cpoMultivHashEncode(hash.size = high.card.thresh, affect.names = high_card_feats)})
  
  encoder = list(
    cpo = caseMultivHashEncode(),
    ps = makeParamSet(makeDiscreteParam("case.high.card.thresh", 
      values = HIGH_CARD_THRESH))
  )
  
  pipeline = constructPipeline(lrn.id, instance, encoder)
  resamplePipeline(instance, pipeline$cpo, pipeline$ps)
}

resampleWithLeafEncoder = function(job, data, instance, lrn.id, ...) {

  caseLeafEncode = makeCPOCase(pSS(high.card.thresh: integer), 
    properties.needed = "numerics",
    cpo.build = function (data, target, high.card.thresh) {
      high_card_feats = getHighCardFeats(instance, high.card.thresh)
      cpoLeafEncode(prune = PRUNE, affect.names = high_card_feats)})
  
  encoder = list(
    cpo = caseLeafEncode(),
    ps = makeParamSet(makeDiscreteParam("case.high.card.thresh", 
      values = HIGH_CARD_THRESH))
  )
  
  pipeline = constructPipeline(lrn.id, instance, encoder)
  resamplePipeline(instance, pipeline$cpo, pipeline$ps)
}

resampleWithClusterEncoder = function(job, data, instance, lrn.id, ...) {

  caseClusterEncode = makeCPOCase(pSS(high.card.thresh: integer), 
    properties.needed = "numerics",
    cpo.build = function (data, target, high.card.thresh) {
      high_card_feats = getHighCardFeats(instance, high.card.thresh)
      cpoClusterEncode(max.levels = high.card.thresh, affect.names = high_card_feats)})
  
  encoder = list(
    cpo = caseClusterEncode(),
    ps = makeParamSet(makeDiscreteParam("case.high.card.thresh", 
      values = HIGH_CARD_THRESH))
  )

  pipeline = constructPipeline(lrn.id, instance, encoder)
  resamplePipeline(instance, pipeline$cpo, pipeline$ps)
}

resampleWithImpactEncoder = function(job, data, instance, lrn.id, ...) {

  caseImpactEncodeRegr = makeCPOCase(pSS(high.card.thresh: integer), 
    properties.needed = "numerics",
    cpo.build = function (data, target, high.card.thresh) {
      high_card_feats = getHighCardFeats(instance, high.card.thresh)
      cpoImpactEncodeRegr(smoothing = SMOOTHING, affect.names = high_card_feats)})
  
  caseImpactEncodeClassif = makeCPOCase(pSS(high.card.thresh: integer), 
    properties.needed = "numerics",
    cpo.build = function (data, target, high.card.thresh) {
      high_card_feats = getHighCardFeats(instance, high.card.thresh)
      cpoImpactEncodeClassif(smoothing = SMOOTHING, affect.names = high_card_feats)})

  encoder = list(
    cpo = if (getTaskType(instance) == "regr") {
      caseImpactEncodeRegr()
    } else {
      caseImpactEncodeClassif()
    },
    ps = makeParamSet(makeDiscreteParam("case.high.card.thresh", 
      values = HIGH_CARD_THRESH))
  )
  
  pipeline = constructPipeline(lrn.id, instance, encoder)
  resamplePipeline(instance, pipeline$cpo, pipeline$ps)
}

resampleWithLmerEncoder = function(job, data, instance, lrn.id, ...) {

  caseLmerEncodeRegr = makeCPOCase(pSS(high.card.thresh: integer, n.folds: integer), 
    properties.needed = "numerics",
    cpo.build = function (data, target, high.card.thresh, n.folds) {
      high_card_feats = getHighCardFeats(instance, high.card.thresh)
      cpoLmerEncodeRegr(n.folds = n.folds, affect.names = high_card_feats)})
  
  caseLmerEncodeMultiClassif = makeCPOCase(pSS(high.card.thresh: integer, n.folds: integer), 
    properties.needed = "numerics",
    cpo.build = function (data, target, high.card.thresh, n.folds) {
      high_card_feats = getHighCardFeats(instance, high.card.thresh)
      cpoLmerEncodeMultiClassif(n.folds = n.folds, affect.names = high_card_feats)})
  
  caseLmerEncodeTwoClassif = makeCPOCase(pSS(high.card.thresh: integer, n.folds: integer), 
    properties.needed = "numerics",
    cpo.build = function (data, target, high.card.thresh, n.folds) {
      high_card_feats = getHighCardFeats(instance, high.card.thresh)
      cpoLmerEncodeTwoClassif(n.folds = n.folds, affect.names = high_card_feats)})
  
  encoder = list(
    cpo = if (getTaskType(instance) == "regr") {
      caseLmerEncodeRegr()
    } else if (length(getTaskDesc(instance)$class.levels) > 2) {
      caseLmerEncodeMultiClassif()
    } else {
      caseLmerEncodeTwoClassif()
    },
    ps = makeParamSet(makeDiscreteParam("case.high.card.thresh", 
      values = HIGH_CARD_THRESH), makeDiscreteParam("case.n.folds", 
      values = N_FOLDS))
  )
  
  pipeline = constructPipeline(lrn.id, instance, encoder)
  resamplePipeline(instance, pipeline$cpo, pipeline$ps)
}

resampleWithRangerEncoder = function(job, data, instance, lrn.id, ...) {

  caseRangerEncodeRegr = makeCPOCase(pSS(high.card.thresh: integer, num.trees: integer), 
    properties.needed = "numerics",
    cpo.build = function (data, target, high.card.thresh, num.trees) {
      high_card_feats = getHighCardFeats(instance, high.card.thresh)
      cpoRangerEncodeRegr(num.trees = num.trees, min.node.size = MIN_NODE_SIZE,
        affect.names = high_card_feats)})
  
  caseRangerEncodeMultiClassif = makeCPOCase(pSS(high.card.thresh: integer, num.trees: integer), 
    properties.needed = "numerics",
    cpo.build = function (data, target, high.card.thresh, num.trees) {
      high_card_feats = getHighCardFeats(instance, high.card.thresh)
      cpoRangerEncodeMultiClassif(num.trees = num.trees, min.node.size = MIN_NODE_SIZE,
        affect.names = high_card_feats)})
  
  caseRangerEncodeTwoClassif = makeCPOCase(pSS(high.card.thresh: integer, num.trees: integer), 
    properties.needed = "numerics",
    cpo.build = function (data, target, high.card.thresh, num.trees) {
      high_card_feats = getHighCardFeats(instance, high.card.thresh)
      cpoRangerEncodeTwoClassif(num.trees = num.trees, min.node.size = MIN_NODE_SIZE,
        affect.names = high_card_feats)})
  
  encoder = list(
    cpo = if (getTaskType(instance) == "regr") {
      caseRangerEncodeRegr()
    } else if (length(getTaskDesc(instance)$class.levels) > 2) {
      caseRangerEncodeMultiClassif()
    } else {
      caseRangerEncodeTwoClassif()
    },
    ps = makeParamSet(makeDiscreteParam("case.high.card.thresh", 
      values = HIGH_CARD_THRESH), makeDiscreteParam("case.num.trees", 
      values = NUM_TREES))
  )
  
  pipeline = constructPipeline(lrn.id, instance, encoder)
  resamplePipeline(instance, pipeline$cpo, pipeline$ps)
}

resampleWithMultivRangerEncoder = function(job, data, instance, lrn.id, ...) {

  caseRangerMultivEncodeRegr = makeCPOCase(pSS(high.card.thresh: integer, num.trees: integer), 
    properties.needed = "numerics",
    cpo.build = function (data, target, high.card.thresh, num.trees) {
      high_card_feats = getHighCardFeats(instance, high.card.thresh)
      cpoRangerMultivEncodeRegr(num.trees = num.trees, min.node.size = MIN_NODE_SIZE,
        affect.names = high_card_feats)})
  
  caseRangerMultivEncodeMultiClassif = makeCPOCase(pSS(high.card.thresh: integer, num.trees: integer), 
    properties.needed = "numerics",
    cpo.build = function (data, target, high.card.thresh, num.trees) {
      high_card_feats = getHighCardFeats(instance, high.card.thresh)
      cpoRangerMultivEncodeMultiClassif(num.trees = num.trees, min.node.size = MIN_NODE_SIZE,
        affect.names = high_card_feats)})
  
  caseRangerMultivEncodeTwoClassif = makeCPOCase(pSS(high.card.thresh: integer, num.trees: integer), 
    properties.needed = "numerics",
    cpo.build = function (data, target, high.card.thresh, num.trees) {
      high_card_feats = getHighCardFeats(instance, high.card.thresh)
      cpoRangerMultivEncodeTwoClassif(num.trees = num.trees, min.node.size = MIN_NODE_SIZE,
        affect.names = high_card_feats)})
  
  encoder = list(
    cpo = if (getTaskType(instance) == "regr") {
      caseRangerMultivEncodeRegr()
    } else if (length(getTaskDesc(instance)$class.levels) > 2) {
      caseRangerMultivEncodeMultiClassif()
    } else {
      caseRangerMultivEncodeTwoClassif()
    },
    ps = makeParamSet(makeDiscreteParam("case.high.card.thresh", 
      values = HIGH_CARD_THRESH), makeDiscreteParam("case.num.trees", 
      values = NUM_TREES))
  )
  
  pipeline = constructPipeline(lrn.id, instance, encoder)
  resamplePipeline(instance, pipeline$cpo, pipeline$ps)
}


resampleWithEmbedEncoder = function(job, data, instance, lrn.id, ...) {
  
  # numeric features (together high_card_features) are used when learning embeddings
  
  caseEmbedregr = makeCPOCase(pSS(high.card.thresh: integer, 
    embed_size: integer, epochs: integer), 
    properties.needed = "numerics",
    cpo.build = function (data, target, high.card.thresh, embed_size, epochs) {
      high_card_feats = getHighCardFeats(instance, high.card.thresh)
      numeric_feats = getNumericFeats(instance)
      cpoEmbedregr(embed_size = embed_size, epochs = epochs,
        affect.names = union(high_card_feats, numeric_feats))})
  
  caseEmbedclassif = makeCPOCase(pSS(high.card.thresh: integer, 
    embed_size: integer, epochs: integer), 
    properties.needed = "numerics",
    cpo.build = function (data, target, high.card.thresh, embed_size, epochs) {
      high_card_feats = getHighCardFeats(instance, high.card.thresh)
      numeric_feats = getNumericFeats(instance)
      cpoEmbedclassif(embed_size = embed_size, epochs = epochs,
        affect.names = union(high_card_feats, numeric_feats))})
  
  encoder = list(
    cpo = if (getTaskType(instance) == "regr") {
      caseEmbedregr()
    } else {
      caseEmbedclassif()
    },
    ps = makeParamSet(makeDiscreteParam("case.high.card.thresh", 
      values = HIGH_CARD_THRESH), makeDiscreteParam("case.embed_size", 
      values = EMBED_SIZE), makeDiscreteParam("case.epochs", values = EPOCHS))
  )
  
  pipeline = constructPipeline(lrn.id, instance, encoder)
  resamplePipeline(instance, pipeline$cpo, pipeline$ps)
}

resampleWithNoEncoding = function(job, data, instance, lrn.id, ...) {
  
  encoder = list(
    cpo = NULLCPO,
    ps = makeParamSet()
  )
  
  pipeline = constructPipeline(lrn.id, instance, encoder)
  # remove finaldummies to ensure NO encoding!
  pipe = as.list(pipeline$cpo$cpo)
  pipe[[3]] = NULL
  pipeline$cpo$cpo = pipeCPO(pipe)
  resamplePipeline(instance, pipeline$cpo, pipeline$ps)
}

resampleWithNoHighCardFeats = function(job, data, instance, lrn.id, ...) {

  caseAsSelect = makeCPOCase(pSS(high.card.thresh: integer),
    cpo.build = function (data, target, high.card.thresh) {
      high_card_feats = getHighCardFeats(instance, high.card.thresh)
      # remove all high cardinality features
      cpoSelect(names = high_card_feats, invert = TRUE)})
  
  encoder = list(
    cpo = caseAsSelect(),
    ps = makeParamSet(makeDiscreteParam("case.high.card.thresh", 
      values = HIGH_CARD_THRESH))
  )
  
  pipeline = constructPipeline(lrn.id, instance, encoder)
  resamplePipeline(instance, pipeline$cpo, pipeline$ps)
}

# helper functions

getHighCardFeats = function(task, high.card.thresh) {
  df = getTaskData(task, target.extra = TRUE)$data
  colnames(df[, vlapply(df, function(x) nlevels(x) > high.card.thresh), drop = FALSE])
}

getNumericFeats = function(task) {
  df = getTaskData(task, target.extra = TRUE)$data
  colnames(df[, vlapply(df, function(x) is.numeric(x)), drop = FALSE])
}
