library(OpenML)

# churn
oml_dat = getOMLDataSet(40701)
desc = oml_dat$desc
dat = oml_dat$data

# fix dataset
dat$state = factor(dat$state)
dat$phone_number = NULL

new_desc = makeOMLDataSetDescription(
  name = desc$name,
  description = paste(desc$description, 
    "For this version, the feature 'phone_number' was removed and 'state' was recoded as a nominal factor variable."),
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
  target.features = "class")

# upload dataset to the OML server
# !DO NOT ACCIDENTALLY RUN THIS!

# uploadOMLDataSet(new_oml_dat)

# Uploading data set to server.
# Uploading to 'http://www.openml.org/api/v1/data'.
# Data set successfully uploaded. Data set ID: 41283
