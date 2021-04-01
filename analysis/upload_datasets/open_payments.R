library(OpenML)
library(data.table)

# load OML config file to access server
loadOMLConfig(path = "~/.openml/config", assign = TRUE)

# open_payments
oml_dat = getOMLDataSet(41243)
desc = oml_dat$desc
dat = as.data.table(oml_dat$data)

# recode all features as factors
dat[, c(colnames(dat)) := lapply(.SD, factor), .SDcols = colnames(dat)]

new_desc = makeOMLDataSetDescription(
  name = desc$name,
  creator = desc$creator,
  collection.date = desc$collection.date,
  language = desc$language,
  description = paste(desc$description, 
                      "For this version, all features were recoded as nominal factor variables."),
  original.data.url = desc$url,
  default.target.attribute = "status",
  licence = desc$licence,
  visibility = "public"
)

new_oml_dat = makeOMLDataSet(
  desc = new_desc,
  data = dat,
  colnames.old = colnames(dat),
  colnames.new = colnames(dat),
  target.features = "status")

# upload dataset to the OML server
# !DO NOT ACCIDENTALLY RUN THIS!

# uploadOMLDataSet(new_oml_dat)

# Uploading data set to server.
# Uploading to 'http://www.openml.org/api/v1/data'.
# Data set successfully uploaded. Data set ID: 41442
