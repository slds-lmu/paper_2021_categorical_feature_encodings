library(OpenML)
library(data.table)

# load OML config file to access server
loadOMLConfig(path = "~/.openml/config", assign = TRUE)

# KDD98
oml_dat = getOMLDataSet(23513)
desc = oml_dat$desc
dat = as.data.table(oml_dat$data)

# fix dataset
dat$TARGET_B = factor(dat$TARGET_B)

more_nominal_vars = c("HPHONE_D", "MHUC2", "INCOME", "WEALTH1", "WEALTH2")
dat[, c(more_nominal_vars) := lapply(.SD, factor), .SDcols = more_nominal_vars]

# remove constant variable
dat[, RFA_2R := NULL]

new_desc = makeOMLDataSetDescription(
  name = desc$name,
  description = paste(desc$description, 
    "For this version, the target was correctly encoded as a binary factor. The features 'HPHONE_D', 'MHUC2', 'INCOME', 'WEALTH1', 'WEALTH2' were recoded as nominal factor variables and the constant feature 'RFA_2R' was removed from the dataset."),
  original.data.url = desc$url,
  default.target.attribute = desc$default.target.attribute,
  licence = desc$licence,
  visibility = "Everyone"
)

new_oml_dat = makeOMLDataSet(
  desc = new_desc,
  data = dat,
  colnames.old = colnames(dat),
  colnames.new = colnames(dat),
  target.features = "TARGET_B")

# upload dataset to the OML server
# !DO NOT ACCIDENTALLY RUN THIS!

# uploadOMLDataSet(new_oml_dat)

# Uploading data set to server.
# Uploading to 'http://www.openml.org/api/v1/data'.
# Data set successfully uploaded. Data set ID: 41435


# prepare smaller version (undersample dataset)

options(java.parameters = "-Xmx8000m")

library(OpenML)
library(mlrCPO)

loadOMLConfig(path = "~/.openml/config", assign = TRUE)

oml_dat = getOMLDataSet(41435)
desc = oml_dat$desc

task = convertOMLDataSetToMlr(oml_dat)
# undersample the larger class
set.seed(42)
task = undersample(task, rate = 0.4, cl = "0")
# drop unused levels
task = task %>>% cpoFixFactors(drop.unused.levels = TRUE, fix.factors.prediction = FALSE)

# # test performance of new dataset
# source("analysis/high_cardinality_benchmark/mlrCPOs.R")
#
# highcard_vars = sapply(getTaskData(task, target.extra = TRUE)$data, function(x) nlevels(x) > 50)
# highcard_vars = getTaskFeatureNames(task)[highcard_vars]
# task_no_highcard = subsetTask(task, features = setdiff(getTaskFeatureNames(task), highcard_vars))
# 
# # for binary classif, change learner to "classif.ranger" and cpoLmerEncodeRegr() to
# # cpoLmerEncodeTwoClassif() or cpoLmerEncodeMultiClassif() 
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

new_desc = makeOMLDataSetDescription(
  name = desc$name,
  description = paste(desc$description, 
                      "For this version, the majority class was downsampled to 40% of the original size. Unused factor levels were dropped."),
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
# Data set successfully uploaded. Data set ID: 42343
