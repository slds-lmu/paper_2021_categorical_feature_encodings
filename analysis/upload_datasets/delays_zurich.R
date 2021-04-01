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

oml_id = 40753
highcard_vars = c("stop_id", "line_number")

oml_dat = getOMLDataSet(data.id = oml_id)
# prepare data
dat = oml_dat$data

anything_to_factor = c("vehicle_type", "line_number", "direction", "stop_id",
                       "weekday")
dat[, anything_to_factor] = lapply(dat[, anything_to_factor], factor)
dat$hours = as.numeric(substr(dat$time, 12, 13))
dat$minutes = as.numeric(substr(dat$time, 15, 16))
dat$seconds = as.numeric(substr(dat$time, 18, 19))
dat$time = dat$hours * 60 + dat$minutes + dat$seconds / 60

oml_dat$data = dat

task = convertOMLDataSetToMlr(oml_dat)
set.seed(47)
task = downsample(task, perc = 0.005)
# task_no_highcard = subsetTask(task, features = setdiff(getTaskFeatureNames(task), highcard_vars))
# 
# # for binary classif, change learner to "classif.ranger" and cpoLmerEncodeRegr() to
# # cpoLmerEncodeTwoClassif() or cpoLmerEncodeMultiClassif() 
# 
# lrn = cpoImputeConstant("NA", affect.type = "factor") %>>% 
#   cpoImputeMean(affect.type = "numeric") %>>%
#   cpoDummyEncode(affect.names = setdiff(getTaskFeatureNames(task), highcard_vars)) %>>% 
#   cpoLmerEncodeRegr(n.folds = 5) %>>% 
#   makeLearner("regr.ranger", num.threads = 10)
# 
# # check whether performance drops if high cardinality features are removed!
# # (if it takes too long to compute, holdout should be enough)
# res = crossval(lrn, task, iters = 5, measures = list(rsq, rmse)) 
# res_no_highcard = crossval(lrn, task_no_highcard, iters = 5, measures = list(rsq, rmse))
# # compare performance between conditions
# (res_no_highcard$aggr - res$aggr) > 0

desc = oml_dat$desc
new_desc = makeOMLDataSetDescription(
  name = desc$name,
  description = paste(desc$description, 
                      "For this version, the task was downsampled to 0.5 percent. Some features were recoded as factors and some new time features were computed."),
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
# Data set successfully uploaded. Data set ID: 42495
