options(java.parameters = "-Xmx8000m")

library(OpenML)
library(data.table)

# load OML config file to access server
loadOMLConfig(path = "~/.openml/config", assign = TRUE)

# Traffic_violations
oml_dat = getOMLDataSet(41246)
desc = oml_dat$desc
dat = as.data.table(oml_dat$data)

# remove variables
dat[, c("Date.Of.Stop", "Time.Of.Stop", "Agency", "SubAgency", "Location", "Latitude", "Longitude", "Accident", "Article", "Geolocation") := NULL]

# collapse SERO and ESERO to SERO
levels(dat$Violation.Type) = c("Citation", "SERO", "SERO", "Warning")

# fix factors
nominal_vars = colnames(dat[, dat[, sapply(.SD, is.character)], with = FALSE])

# remove punctuation
dat[, c(nominal_vars) := 
      lapply(.SD, stringr::str_replace_all, pattern = "[[:punct:]]", replacement = ""), 
    .SDcols = nominal_vars]

dat[, c(nominal_vars) := 
      lapply(.SD, stringr::str_replace_all, pattern = "`", replacement = ""), 
    .SDcols = c(nominal_vars)]

dat[, c(nominal_vars) := 
      lapply(.SD, stringr::str_replace_all, pattern = "-", replacement = ""), 
    .SDcols = c(nominal_vars)]

dat[, c(nominal_vars) := 
      lapply(.SD, stringr::str_squish), 
    .SDcols = nominal_vars]

dat[dat == ""] = NA

# recode all features as factors
dat[, c(nominal_vars) := lapply(.SD, factor), .SDcols = nominal_vars]

# check dataset for inconsistencies
str(dat)

# OML dataset
new_desc = makeOMLDataSetDescription(
  name = desc$name,
  creator = desc$creator,
  collection.date = desc$collection.date,
  language = desc$language,
  description = paste(desc$description, 
                      "For this version, some features were removed and all remaining character features were recoded as nominal factor variables. All punctuation characters were removed from factor levels.
                      The variable 'Violation.Type' is used as target by default. The smaller target categories 'SERO' and 'ESERO' were collapsed into one category labeled 'SERO'."),
  original.data.url = desc$url,
  default.target.attribute = "Violation.Type",
  licence = desc$licence,
  visibility = desc$visibility
)

new_oml_dat = makeOMLDataSet(
  desc = new_desc,
  data = dat,
  colnames.old = colnames(dat),
  colnames.new = colnames(dat),
  target.features = "Violation.Type")

# upload dataset to the OML server
# !DO NOT ACCIDENTALLY RUN THIS!

# uploadOMLDataSet(new_oml_dat)

# Uploading data set to server.
# Uploading to 'http://www.openml.org/api/v1/data'.
# Data set successfully uploaded. Data set ID: 41443



# prepare smaller version (downsample dataset)

options(java.parameters = "-Xmx8000m")

library(OpenML)
library(mlrCPO)

loadOMLConfig(path = "~/.openml/config", assign = TRUE)

oml_dat = getOMLDataSet(41443)
desc = oml_dat$desc

task = convertOMLDataSetToMlr(oml_dat)
# downsample dataset
set.seed(42)
task = downsample(task, perc = 0.05, stratify = TRUE)

# drop unused levels
task = task %>>% cpoFixFactors(drop.unused.levels = TRUE, fix.factors.prediction = FALSE)

# dropping almost constant features
task = dropFeatures(task, features = c("Fatal", "HAZMAT", "Work.Zone", "Alcohol"))

# # test performance of new dataset
# source("analysis/high_cardinality_benchmark/mlrCPOs.R")
# 
# highcard_vars = c("Description", "Make", "Charge", "Model", "Driver.City")
# task_no_highcard = subsetTask(task, features = setdiff(getTaskFeatureNames(task), highcard_vars))
# 
# # for binary classif, change learner to "classif.ranger" and cpoLmerEncodeRegr() to
# # cpoLmerEncodeTwoClassif() or cpoLmerEncodeMultiClassif() 
# 
# lrn = cpoImputeConstant("NA", affect.type = "factor") %>>% 
#   cpoImputeMean(affect.type = "numeric") %>>%
#   cpoDummyEncode(affect.names = setdiff(getTaskFeatureNames(task), highcard_vars)) %>>% 
#   cpoLmerEncodeMultiClassif(n.folds = 5) %>>% 
#   makeLearner("classif.ranger", num.threads = 10, predict.type = "prob")
# # construct new learner, as cpoLmerEncodeMultiClassif fails when no features are encoded...
# lrn2 = cpoImputeConstant("NA", affect.type = "factor") %>>% 
#   cpoImputeMean(affect.type = "numeric") %>>%
#   cpoDummyEncode(affect.names = setdiff(getTaskFeatureNames(task), highcard_vars)) %>>% 
#   makeLearner("classif.ranger", num.threads = 10, predict.type = "prob")
# 
# # check whether performance drops if high cardinality features are removed!
# # (if it takes too long to compute, holdout should be enough)
# res = crossval(lrn, task, iters = 5, measures = list(multiclass.aunu, mmce))
# res_no_highcard = crossval(lrn2, task_no_highcard, iters = 5, measures = list(multiclass.aunu, mmce))

new_desc = makeOMLDataSetDescription(
  name = desc$name,
  description = paste(desc$description, 
                      "For this version, the dataset was downsampled to 5% of the original size. Unused factor levels and a few almost constant features were dropped."),
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
# Data set successfully uploaded. Data set ID: 42345
