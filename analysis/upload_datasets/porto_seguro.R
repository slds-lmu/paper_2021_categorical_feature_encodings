library("OpenML")
library(data.table)

# load OML config file to access server
loadOMLConfig(path = "~/.openml/config", assign = TRUE)

# load dataset
dat = fread(file = "data/porto_seguro/train.csv")

# basic preprocessing

dat[dat == -1] = NA
  
nominal_vars = grep("bin|cat", colnames(dat), value = TRUE)
nominal_vars = c(nominal_vars, "target")
dat[, c(nominal_vars) := lapply(.SD, factor), .SDcols = nominal_vars]

# remove useless calc variables, see https://www.kaggle.com/c/porto-seguro-safe-driver-prediction/kernels
calc_vars = grep("calc", colnames(dat), value = TRUE)
dat[, (calc_vars) := NULL]

# check dataset for inconsistencies
summary(dat)

# OML description
desc = makeOMLDataSetDescription(
  name = "porto-seguro",
  description = "Training dataset of the 'Porto Seguros Safe Driver Prediction' Kaggle challenge [https://www.kaggle.com/c/porto-seguro-safe-driver-prediction]. The goal was to predict whether a driver will file an insurance claim next year. The official rules of the challenge explicitely state that the data may be used for 'academic research and education, and other non-commercial purposes' [https://www.kaggle.com/c/porto-seguro-safe-driver-prediction/rules]. For a description of all variables checkout the Kaggle dataset repository [https://www.kaggle.com/c/porto-seguro-safe-driver-prediction/data]. It states that numeric features with integer values that do not contain 'bin' or 'cat' in their variable names are in fact ordinal features which could be treated as ordinal factors in R. For further information on effective preprocessing and feature engineering checkout the 'Kernels' section of the Kaggle challenge website [https://www.kaggle.com/c/porto-seguro-safe-driver-prediction/kernels]. For this version we removed all 'calc' variables, as the Kaggle forum indicates that they do not carry much information.",
  original.data.url = "https://www.kaggle.com/c/porto-seguro-safe-driver-prediction/data",
  default.target.attribute = "target",
  row.id.attribute = "id", 
  visibility = "Everyone"
)

# create OML dataset
oml_dat = makeOMLDataSet(
  desc = desc,
  data = dat,
  colnames.old = colnames(dat),
  colnames.new = colnames(dat),
  target.features = "target")

# upload dataset to the OML server
# !DO NOT ACCIDENTALLY RUN THIS!

# uploadOMLDataSet(oml_dat)

# Uploading data set to server.
# Uploading to 'http://www.openml.org/api/v1/data'.
# Data set successfully uploaded. Data set ID: 42206
