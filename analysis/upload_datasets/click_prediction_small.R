library(OpenML)
library(data.table)

# load OML config file to access server
loadOMLConfig(path = "~/.openml/config", assign = TRUE)

# Click_prediction_small
oml_dat = getOMLDataSet(1220)
desc = oml_dat$desc
dat = as.data.table(oml_dat$data)

# code id features as factors
more_nominal_vars = c("ad_id", "advertiser_id", "keyword_id", "title_id", 
  "description_id", "user_id")
dat[, c(more_nominal_vars) := lapply(.SD, factor), .SDcols = more_nominal_vars]

# check dataset for inconsistencies
summary(dat)

# OML description
new_desc = makeOMLDataSetDescription(
  name = desc$name,
  description = "This is the same data as version 5 (OpenML ID = 1220) with '_id' features coded as nominal factor variables.",
  original.data.url = desc$url,
  default.target.attribute = desc$default.target.attribute,
  ignore.attribute = c("url_hash", "query_id"),
  licence = desc$licence,
  visibility = "Everyone"
)

# OML dataset
new_oml_dat = makeOMLDataSet(
  desc = new_desc,
  data = dat,
  colnames.old = colnames(dat),
  colnames.new = colnames(dat),
  target.features = "click")

# upload dataset to the OML server
# !DO NOT ACCIDENTALLY RUN THIS!

# uploadOMLDataSet(new_oml_dat)

# Uploading data set to server.
# Uploading to 'http://www.openml.org/api/v1/data'.
# Data set successfully uploaded. Data set ID: 41434
