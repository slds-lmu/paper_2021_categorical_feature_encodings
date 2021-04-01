# load OpenML dataset ids defined by doc/high_card_final_datasets.Rmd
OML_DATA_IDS = readRDS("analysis/high_cardinality_benchmark/oml_ids.rds")

MLR_LRN_IDS = c("featureless", "cvglmnet", "kknn", "ranger", 
  "liquidSVM", "xgboost.earlystop.wrap", "keras")

CLASSIF_RDESC = makeResampleDesc("CV", iters = 5L, stratify = TRUE)
REGR_RDESC = makeResampleDesc("CV", iters = 5L)

INNER_CLASSIF_RDESC = makeResampleDesc("CV", iters = 3L, stratify = TRUE)
INNER_REGR_RDESC = makeResampleDesc("CV", iters = 3L)

TWO_CLASSIF_MES = list(
  auc, brier, lsr, ssr, ber, kappa,
  mmce, acc, tp, tn, fp, fn, 
  timetrain, timepredict)
MULTI_CLASSIF_MES = list(
  multiclass.aunu, multiclass.aunp, multiclass.au1p, multiclass.au1u, 
  multiclass.brier, lsr, ssr, ber, kappa, mmce, acc,
  timetrain, timepredict
)
REGR_MES = list(rmse, mse, medse, mae, medae, rsq,
  kendalltau, spearmanrho, 
  timetrain, timepredict)

INNER_TWO_CLASSIF_MES = auc
INNER_MULTI_CLASSIF_MES = multiclass.aunu
INNER_REGR_MES = rmse

TUNE_THRESH = FALSE
CONTROL = makeTuneControlGrid(tune.threshold = TUNE_THRESH)

KEEP_MODELS = FALSE
KEEP_PREDS = FALSE

SEED = 250319
REPLS = 1

TUNE_PIPE = TRUE

## encoder hyperpars
HIGH_CARD_THRESH = c(10L, 25L, 125L)

# dummy
DUMMY_ENC = c(TRUE, FALSE)
# frequency
RANK = c(FALSE)
# leaf
PRUNE = c(TRUE)
# impact
SMOOTHING = c(1e-04)
# lmer
N_FOLDS = c(1L, 5L, 10L)
# ranger
NUM_TREES = c(10L, 25L, 50L)
MIN_NODE_SIZE = c(20L)
# embed
EMBED_SIZE = c(2L, 20L, 50L)
EPOCHS = c(10L, 30L)
