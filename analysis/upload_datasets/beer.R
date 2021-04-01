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

oml_id = 42078
highcard_vars = c("brewery_id", "review_profilename", "beer_beerid", "beer_style")

oml_dat = getOMLDataSet(data.id = oml_id)
# prepare data
dat = oml_dat$data
dat$brewery_id = as.factor(dat$brewery_id)
dat$review_profilename = as.factor(dat$review_profilename)
dat$beer_beerid = as.factor(dat$beer_beerid)
dat$review_time = dat$review_time / (60 * 60 * 24 * 365) + 1970
dat$beer_style = as.factor(dat$beer_style)

oml_dat$data = dat
oml_dat$desc$ignore.attribute = c("brewery_name", "review_aroma", 
                                  "review_apparance", "review_palate", 
                                  "review_taste", "beer_abv", "beer_name")
oml_dat$target.features = "review_overall"
oml_dat$desc$default.target.attribute = "review_overall"

task = convertOMLDataSetToMlr(oml_dat)
# downsample to 2% because performance and tendency for overfitting is high enough
set.seed(42)
task = downsample(task, perc = 0.02)
# task_no_highcard = subsetTask(task, features = setdiff(getTaskFeatureNames(task), highcard_vars))
# 
# # for binary classif, change learner to "classif.ranger" and cpoLmerEncodeRegr() to
# # cpoLmerEncodeMultiClassif( or cpoLmerEncodeMultiClassif() 
# 
# lrn = cpoImputeConstant("NA", affect.type = "factor") %>>% 
#   cpoImputeMean(affect.type = "numeric") %>>%
#   cpoDummyEncode(affect.names = setdiff(getTaskFeatureNames(task), highcard_vars)) %>>% 
#   cpoLmerEncodeRegr(n.folds = 10) %>>% 
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
                      "For this version, the task was downsampled to 2 percent. The feature review_overall was chosen as the target. Ignore some useless features and encode as features as factor variables."),
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
# Data set successfully uploaded. Data set ID: 42494

