library("OpenML")
library(data.table)

# load OML config file to access server
loadOMLConfig(path = "~/.openml/config", assign = TRUE)

# load dataset
dat = fread(file = "data/police_incidents/Police_Department_Incident_Reports__Historical_2003_to_May_2018.csv")

# preprocessing

dat[, ViolentCrime := Category %in% c("ASSAULT", "ROBBERY", "SEX OFFENSES, FORCIBLE", "KIDNAPPING") |
  Descript %in% c("GRAND THEFT PURSESNATCH", "ATTEMPTED GRAND THEFT PURSESNATCH")]

dat[, ViolentCrime := factor(ViolentCrime, levels = c("TRUE", "FALSE"), labels = c("Yes", "No"))]

dat[, Hour := hour(as.ITime(Time, format = "%H:%M"))]
  
dat[, Date := as.IDate(Date, format = "%m/%d/%Y")]

dat[, DayOfWeek := wday(Date)]
dat[, Month := month(Date)]
dat[, Year := year(Date)]

# dat[, Address := stri_replace_all(dat$Address, replacement = "_", regex = "/")]
# dat[, Address := abbreviate(Address, minlength = 15)]

# dat[, Address := match(Address, unique(na.omit(Address)))]

nominal_vars = c("DayOfWeek", "Month", "Year", "PdDistrict", "Address")
dat[, c(nominal_vars) := lapply(.SD, factor), .SDcols = nominal_vars]

# dat = dat[sample(nrow(dat), size = 50000, replace = FALSE), c("IncidntNum", nominal_vars, "X", "Y", "ViolentCrime"), with = FALSE]
dat = dat[, c("IncidntNum", "Hour", nominal_vars, "X", "Y", "ViolentCrime"), with = FALSE]

dat = dat[PdDistrict != "",]

# check dataset for inconsistencies
summary(dat)

# OML description
desc = makeOMLDataSetDescription(
  name = "sf-police-incidents",
  description = "Incident reports from the San Franciso Police Department between January 2003 and May 2018, provided by the City and County of San Francisco. The dataset was downloaded on 05.11.2018. from [https://data.sfgov.org/Public-Safety/Police-Department-Incident-Reports-Historical-2003/tmnf-yvry]. For a description of all variables, checkout the homepage of the data provider. The original data was published under ODC Public Domain Dedication and Licence (PDDL) [https://opendatacommons.org/licenses/pddl/1.0/]. As target, the binary variable 'ViolentCrime' was created. A 'ViolentCrime' was defined as 'Category' %in% c('ASSAULT', 'ROBBERY', 'SEX OFFENSES, FORCIBLE', 'KIDNAPPING') | 'Descript' %in% c('GRAND THEFT PURSESNATCH', 'ATTEMPTED GRAND THEFT PURSESNATCH'). Additional date and time features 'Hour', 'DayOfWeek', 'Month', and 'Year' were created. The original variables 'Category', 'Descript', 'Date', 'Time', 'Resolution', 'Location', and 'PdId' were removed from the dataset. One record which contained the only missing value in the variable 'PdDistrict' was removed from the dataset. Using this dataset for machine learning was inspired by Nina Zumel's blogpost [http://www.win-vector.com/blog/2012/07/modeling-trick-impact-coding-of-categorical-variables-with-many-levels/]. Note that incidents consist of multiple rows in the dataset when the crime belongs to more than one 'Category', which is indicated by the ID variable 'IncidntNum' (ignored by default).",
  original.data.url = "https://data.sfgov.org/Public-Safety/Police-Department-Incident-Reports-Historical-2003/tmnf-yvry",
  default.target.attribute = "ViolentCrime",
  licence = "ODC Public Domain Dedication and Licence (PDDL)",
  ignore.attribute = "IncidntNum", 
  visibility = "Everyone"
)

# create OML dataset
oml_dat = makeOMLDataSet(
  desc = desc,
  data = dat,
  colnames.old = colnames(dat),
  colnames.new = colnames(dat),
  target.features = "ViolentCrime")

# upload dataset to the OML server
# !DO NOT ACCIDENTALLY RUN THIS!

# uploadOMLDataSet(oml_dat)

# Uploading data set to server.
# Uploading to 'http://www.openml.org/api/v1/data'.
# Data set successfully uploaded. Data set ID: 41436

# old version with integers in address
# Uploading data set to server.
# Uploading to 'http://www.openml.org/api/v1/data'.
# Data set successfully uploaded. Data set ID: 41271


# prepare smaller version (undersample dataset)

options(java.parameters = "-Xmx8000m")

library(OpenML)
library(mlrCPO)

oml_dat = getOMLDataSet(41436)
desc = oml_dat$desc

task = convertOMLDataSetToMlr(oml_dat)

# downsample majority class
set.seed(42)
task = undersample(task, rate = sum(getTaskData(task, target.extra = TRUE)$target == "Yes") / 
                     sum(getTaskData(task, target.extra = TRUE)$target == "No"), 
                   cl = "No")
# drop unused levels
task = task %>>% cpoFixFactors(drop.unused.levels = TRUE, fix.factors.prediction = FALSE)

# drop features to increase importance of categorical features
task = dropFeatures(task, features = c("X", "Y"))

# # test performance of new dataset
# source("analysis/high_cardinality_benchmark/mlrCPOs.R")
# 
# highcard_vars = c("Address")
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
                      "For this version, the majority class was downsampled to achieve a balanced classification task. Unused factor levels were dropped. The numeric features 'X' and 'Y' were removed to increase the importance of the high cardinal factorial features"),
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
# Data set successfully uploaded. Data set ID: 42344

