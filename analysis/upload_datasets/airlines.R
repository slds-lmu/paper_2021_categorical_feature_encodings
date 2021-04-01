# strange memory option for RWeka
options(java.parameters = "-Xmx8000m")

library(mlr)
library(mlrCPO)
library(OpenML)

# source("analysis/high_cardinality_benchmark/mlrCPOs.R")

# This should work for the final datasets!

# configure OpenML
# always use RWeka as farff does not work with too many factor levels!!
#saveOMLConfig(cachedir = "~/.openml/cache", arff.reader = "RWeka")

oml_id = 1169
highcard_vars = c("AirportFrom", "AirportTo", "Flight")

oml_dat = getOMLDataSet(data.id = oml_id)
# prepare data
dat = oml_dat$data
dat$Flight = as.factor(dat$Flight)

oml_dat$data = dat

# keep the extreme Flight feature in the dataset to detect encoding strategies that tend to overfit:
# lmer encoder overfits with n.folds = 1 but is fine with n.folds = 5 or 10
# oml_dat$desc$ignore.attribute = "Flight"

task = convertOMLDataSetToMlr(oml_dat)
set.seed(42)
task = downsample(task, perc = 0.05)
# task_no_highcard = subsetTask(task, features = setdiff(getTaskFeatureNames(task), highcard_vars))
# 
# # for binary classif, change learner to "classif.ranger" and cpoLmerEncodeRegr() to
# # cpoLmerEncodeMultiClassif( or cpoLmerEncodeMultiClassif() 
# 
# lrn = cpoImputeConstant("NA", affect.type = "factor") %>>% 
#   cpoImputeMean(affect.type = "numeric") %>>%
#   cpoDummyEncode(affect.names = setdiff(getTaskFeatureNames(task), highcard_vars)) %>>% 
#   cpoLmerEncodeTwoClassif(n.folds = 5) %>>% 
#   makeLearner("classif.ranger", num.threads = 10, predict.type = "prob")
# 
# # check whether performance drops if high cardinality features are removed!
# # (if it takes too long to compute, holdout should be enough)
# res = crossval(lrn, task, iters = 5, measures = list(auc, mmce)) 
# res_no_highcard = crossval(lrn, task_no_highcard, iters = 5, measures = list(auc, mmce))
# # compare performance between conditions
# (res_no_highcard$aggr - res$aggr) > 0

desc = oml_dat$desc
new_desc = makeOMLDataSetDescription(
  name = desc$name,
  description = paste(desc$description, 
                      "For this version, the task was downsampled to 5 percent. The feature Flight was correctly encoded as a factor variable."),
  original.data.url = desc$url,
  default.target.attribute = desc$default.target.attribute,
  licence = desc$licence,
  visibility = "Everyone"
)

new_oml_dat = convertMlrTaskToOMLDataSet(task, description = new_desc)

# upload dataset to the OML server
# !DO NOT ACCIDENTALLY RUN THIS!

# uploadOMLDataSet(new_oml_dat)
# Uploading data set to server.
# Uploading to 'http://www.openml.org/api/v1/data'.
# Data set successfully uploaded. Data set ID: 42493

