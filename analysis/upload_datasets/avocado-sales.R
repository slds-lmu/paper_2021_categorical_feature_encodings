library("OpenML")
library(data.table)

# load OML config file to access server
loadOMLConfig(path = "~/.openml/config", assign = TRUE)

# load dataset
dat = fread(file = "data/avocado_sales/avocado.csv")

# basic preprocessing

dat[, "V1" := NULL]

nominal_vars = c("type", "year", "region")
dat[, c(nominal_vars) := lapply(.SD, factor), .SDcols = nominal_vars]

# check dataset for inconsistencies
summary(dat)

# OML description
desc = makeOMLDataSetDescription(
  name = "avocado-sales",
  description = "Historical data on avocado prices and sales volume in multiple US markets. Downloaded from Kaggle [https://www.kaggle.com/neuromusic/avocado-prices/home] on 29.10.2018. The original data stems from the HASS AVOCADO BOARD [http://www.hassavocadoboard.com/retail/volume-and-price-data]. The Kaggle dataset was licensed under the Open Database License (ODbL) [https://opendatacommons.org/licenses/odbl/1.0/]. The variable 'AveragePrice' was selected as target variable. For a description of all variables checkout the Kaggle dataset repo or the original dataset description by the HASS AVOCADO BOARD. 'Year' is coded as a categorical features as the dataset covers only the years 2015-2018. The dataset also includes a 'Date' variable (ignored by default) which can be used to construct additional month or day features. The ID variable from the Kaggle version was removed from the dataset.",
  licence = "Open Database License (ODbL)",
  original.data.url = "http://www.hassavocadoboard.com/retail/volume-and-price-data",
  default.target.attribute = "AveragePrice",
  ignore.attribute = "Date",
  visibility = "Everyone"
)

# create OML dataset
oml_dat = makeOMLDataSet(
  desc = desc,
  data = dat,
  colnames.old = colnames(dat),
  colnames.new = colnames(dat),
  target.features = "AveragePrice")

# upload dataset to the OML server
# !DO NOT ACCIDENTALLY RUN THIS!

# uploadOMLDataSet(oml_dat)

# Uploading data set to server.
# Uploading to 'http://www.openml.org/api/v1/data'.
# Data set successfully uploaded. Data set ID: 41210
