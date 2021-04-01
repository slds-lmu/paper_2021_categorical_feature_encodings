library("OpenML")
library(data.table)

# load OML config file to access server
loadOMLConfig(path = "~/.openml/config", assign = TRUE)

# load dataset
library(AppliedPredictiveModeling)
data(schedulingData)
dat = as.data.table(schedulingData)

# check dataset for inconsistencies
summary(dat)

# OML description
desc = makeOMLDataSetDescription(
  name = "hpc-job-scheduling",
  description = "HPC Job Scheduling Data as included in the R-package 'AppliedPredictiveModeling' [Max Kuhn and Kjell Johnson (2018). AppliedPredictiveModeling: Functions and Data Sets for 'Applied Predictive Modeling'. R package version 1.1-7. https://CRAN.R-project.org/package=AppliedPredictiveModeling]. To obtain the dataset directly in R, call data(schedulingData, package = 'AppliedPredictiveModeling'). For a description of the dataset checkout the documentation of the R-package. The variable 'Class' was chosen as target variable.",
  licence = NA,
  default.target.attribute = "Class",
  visibility = "Everyone"
)

# create OML dataset
oml_dat = makeOMLDataSet(
  desc = desc,
  data = dat,
  colnames.old = colnames(dat),
  colnames.new = colnames(dat),
  target.features = "Class")

# upload dataset to the OML server
# !DO NOT ACCIDENTALLY RUN THIS!

# uploadOMLDataSet(oml_dat)

# Uploading data set to server.
# Uploading to 'http://www.openml.org/api/v1/data'.
# Data set successfully uploaded. Data set ID: 41212
