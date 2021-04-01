library(batchtools)
library(OpenML)
library(mlr)

library(data.table)
setDTthreads(threads = 1)

# source globals params
source("analysis/high_cardinality_benchmark/global_params.R")

# configure OpenML
saveOMLConfig(cachedir = "~/.openml/cache", arff.reader = "RWeka", overwrite = TRUE)

# download OML datasets and store them locally
populateOMLCache(data.ids = OML_DATA_IDS, overwrite = FALSE)

# make batchtools registry
reg_dir = "analysis/high_cardinality_benchmark/registry"
reg = makeExperimentRegistry(reg_dir, 
  source = c("analysis/high_cardinality_benchmark/config.R",
    "analysis/high_cardinality_benchmark/global_params.R",
    "analysis/high_cardinality_benchmark/problems.R", 
    "analysis/high_cardinality_benchmark/algorithms.R",
    "analysis/high_cardinality_benchmark/pipeline.R",
    "analysis/high_cardinality_benchmark/mlrCPOs.R",
    "analysis/high_cardinality_benchmark/learners/xgboost.earlystop.wrap.R",
    "analysis/high_cardinality_benchmark/learners/RLearner_classif_kerasff.R",
    "analysis/high_cardinality_benchmark/learners/RLearner_regr_kerasff.R",
    "analysis/high_cardinality_benchmark/learners/RLearner_classif_embed_kerasff_clean.R",
    "analysis/high_cardinality_benchmark/learners/RLearner_regr_embed_kerasff_clean.R"),
  seed = SEED)
setDefaultRegistry(loadRegistry(reg_dir, writeable = TRUE))

# add problems
for (did in OML_DATA_IDS) {
  addProblem(name = as.character(did), data = did, fun = provideTaskWithPreProc,
    seed = SEED)
}

# add algorithms
addAlgorithm(name = "dummy", fun = resampleWithDummyEncoder)
addAlgorithm(name = "integer", fun = resampleWithIntegerEncoder)
addAlgorithm(name = "frequency", fun = resampleWithFrequencyEncoder)
addAlgorithm(name = "hash", fun = resampleWithHashEncoder)
addAlgorithm(name = "hash_multiv", fun = resampleWithMultivHashEncoder)
addAlgorithm(name = "leaf", fun = resampleWithLeafEncoder)
addAlgorithm(name = "cluster", fun = resampleWithClusterEncoder)
addAlgorithm(name = "impact", fun = resampleWithImpactEncoder)
addAlgorithm(name = "lmer", fun = resampleWithLmerEncoder)
addAlgorithm(name = "ranger", fun = resampleWithRangerEncoder)
addAlgorithm(name = "ranger_multiv", fun = resampleWithMultivRangerEncoder)
addAlgorithm(name = "embed", fun = resampleWithEmbedEncoder)
addAlgorithm(name = "none", fun = resampleWithNoEncoding)
addAlgorithm(name = "remove", fun = resampleWithNoHighCardFeats)

# add experiments
algo_designs = list(
  dummy = CJ(lrn.id = setdiff(MLR_LRN_IDS, "featureless")),
  integer = CJ(lrn.id = setdiff(MLR_LRN_IDS, "featureless")),
  frequency = CJ(lrn.id = setdiff(MLR_LRN_IDS, "featureless")),
  hash = CJ(lrn.id = setdiff(MLR_LRN_IDS, "featureless")),
  hash_multiv = CJ(lrn.id = setdiff(MLR_LRN_IDS, "featureless")),
  leaf = CJ(lrn.id = setdiff(MLR_LRN_IDS, "featureless")),
  cluster = CJ(lrn.id = setdiff(MLR_LRN_IDS, "featureless")),
  impact = CJ(lrn.id = setdiff(MLR_LRN_IDS, "featureless")),
  lmer = CJ(lrn.id = setdiff(MLR_LRN_IDS, "featureless")),
  ranger = CJ(lrn.id = setdiff(MLR_LRN_IDS, "featureless")),
  ranger_multiv = CJ(lrn.id = setdiff(MLR_LRN_IDS, "featureless")),
  embed = CJ(lrn.ids = setdiff(MLR_LRN_IDS, "featureless")),
  none = CJ(lrn.id = c("featureless", "ranger")),
  remove = CJ(lrn.id = setdiff(MLR_LRN_IDS, "featureless"))
)

addExperiments(algo.designs = algo_designs, repls = REPLS, combine = "crossprod")
