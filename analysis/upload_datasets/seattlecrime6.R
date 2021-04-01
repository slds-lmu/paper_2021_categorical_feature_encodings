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

oml_id = 41960
highcard_vars = c("Primary_Offense_Description")

oml_dat = getOMLDataSet(data.id = oml_id)
# prepare data
dat = oml_dat$data
# date columns are non meaningful: perform extraction
dat$Occurred_Time = as.character(dat$Occurred_Time)
dat$Occurred_hour = 0
dat$Occurred_hour = ifelse(nchar(dat$Occurred_Time) == 4, 
                           substr(dat$Occurred_Time, 1, 2), dat$Occurred_hour)
dat$Occurred_hour = ifelse(nchar(dat$Occurred_Time) == 3, 
                           substr(dat$Occurred_Time, 2, 2), dat$Occurred_hour)
dat$Occurred_min = 0
dat$Occurred_min = ifelse(nchar(dat$Occurred_Time) == 4, 
                          substr(dat$Occurred_Time, 3, 4), dat$Occurred_min)
dat$Occurred_min = ifelse(nchar(dat$Occurred_Time) == 3, 
                          substr(dat$Occurred_Time, 2, 3), dat$Occurred_min)
dat$Occurred_min = ifelse(nchar(dat$Occurred_Time) == 2, 
                          substr(dat$Occurred_Time, 1, 2), dat$Occurred_min)
dat$Occurred_min = ifelse(nchar(dat$Occurred_Time) == 1, 
                          substr(dat$Occurred_Time, 1, 1), dat$Occurred_hour)

dat$Reported_Time = as.character(dat$Reported_Time)
dat$Reported_hour = 0
dat$Reported_hour = ifelse(nchar(dat$Reported_Time) == 4, 
                           substr(dat$Reported_Time, 1, 2), dat$Reported_hour)
dat$Reported_hour = ifelse(nchar(dat$Reported_Time) == 3, 
                           substr(dat$Reported_Time, 2, 2), dat$Reported_hour)
dat$Reported_min = 0
dat$Reported_min = ifelse(nchar(dat$Reported_Time) == 4, 
                          substr(dat$Reported_Time, 3, 4), dat$Reported_min)
dat$Reported_min = ifelse(nchar(dat$Reported_Time) == 3, 
                          substr(dat$Reported_Time, 2, 3), dat$Reported_min)
dat$Reported_min = ifelse(nchar(dat$Reported_Time) == 2, 
                          substr(dat$Reported_Time, 1, 2), dat$Reported_min)
dat$Reported_min = ifelse(nchar(dat$Reported_Time) == 1, 
                          substr(dat$Reported_Time, 1, 1), dat$Reported_hour)
dat$Occurred_hour = as.numeric(dat$Occurred_hour)
dat$Occurred_min = as.numeric(dat$Occurred_min)
dat$Reported_hour = as.numeric(dat$Reported_hour)
dat$Reported_min = as.numeric(dat$Reported_min)

dat$Occurred_Time = dat$Occurred_hour * 60 + dat$Occurred_min
dat$Reported_Time = dat$Reported_hour * 60 + dat$Reported_min
dat = dat[!is.na(dat$Reported_Time), ]

# remove Reported_hour and Reported_min to avoid perfect target prediction
dat$Reported_hour = NULL
dat$Reported_min = NULL

# remove Report_Number to make sure it is not included in task
# (bug in OpenML package, ignored features are only removed in the developers version)
dat$Report_Number = NULL

# remove Crime_Subcategory to increase importance of Primary_Offense_Description
dat$Crime_Subcategory = NULL

oml_dat$data = dat
oml_dat$target.features = "Reported_Time"
oml_dat$desc$default.target.attribute = "Reported_Time"

task = convertOMLDataSetToMlr(oml_dat)

# downsample to 10% because performance does not change
set.seed(42)
task = downsample(task, perc = 0.1)

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
                      "For this version, the task was downsampled to 10 percent. Compute a new target Reported_Time. Compute new date features, ignore some features and encode as features as factor variables."),
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
# Data set successfully uploaded. Data set ID: 42496
