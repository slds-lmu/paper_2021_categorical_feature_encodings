options(java.parameters = "-Xmx8000m")

library(OpenML)
library(data.table)

# load OML config file to access server
loadOMLConfig(path = "~/.openml/config", assign = TRUE)

# medical_charges
oml_dat = getOMLDataSet(41239)
desc = oml_dat$desc
dat = as.data.table(oml_dat$data)

# remove variables
dat[, c("Provider.Id", "Provider.Street.Address", "Provider.Zip.Code", "Average.Total.Payments", "Average.Covered.Charges") := NULL]

# recode all features as factors
nominal_vars = colnames(dat[, dat[, sapply(.SD, is.character)], with = FALSE])
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
                      "For this version, some features were removed and all remaining character features were recoded as nominal factor variables.
                      The variable 'Average.Medicare.Payments' is used as target by default."),
  original.data.url = desc$url,
  default.target.attribute = "Average.Medicare.Payments",
  licence = desc$licence,
  visibility = desc$visibility
  )

new_oml_dat = makeOMLDataSet(
  desc = new_desc,
  data = dat,
  colnames.old = colnames(dat),
  colnames.new = colnames(dat),
  target.features = "Average.Medicare.Payments")

# upload dataset to the OML server
# !DO NOT ACCIDENTALLY RUN THIS!

# uploadOMLDataSet(new_oml_dat)

# Uploading data set to server.
# Uploading to 'http://www.openml.org/api/v1/data'.
# Data set successfully uploaded. Data set ID: 41444

