options(java.parameters = "-Xmx8000m")

library(OpenML)
library(data.table)

# load OML config file to access server
loadOMLConfig(path = "~/.openml/config", assign = TRUE)

# Midwest_survey
oml_dat = getOMLDataSet(41234)
desc = oml_dat$desc
dat = as.data.table(oml_dat$data)

# remove variables
dat[, c("In.what.ZIP.code.is.your.home.located...enter.5.digit.ZIP.code..for.example..00544.or.94305.") := NULL]

# recode all features as factors
nominal_vars = colnames(dat[, dat[, sapply(.SD, is.character)], with = FALSE])
dat[, c(nominal_vars) := lapply(.SD, factor), .SDcols = nominal_vars]

# create new target category for missing values (as was probably done in Cerda et al., 2018)
dat$Location..Census.Region. = addNA(dat$Location..Census.Region, ifany = TRUE)
# rename NA level as it might not work with some learners (e.g. ranger)
levels(dat$Location..Census.Region.)[is.na(levels(dat$Location..Census.Region.))] = "Not Available"

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
                      The variable 'Location..Census.Region.' is used as target by default. For missing values in the target variable, a new target level was created labeled 'Not Available'."),
  original.data.url = desc$url,
  default.target.attribute = "Location..Census.Region.",
  licence = desc$licence,
  visibility = desc$visibility
  )

new_oml_dat = makeOMLDataSet(
  desc = new_desc,
  data = dat,
  colnames.old = colnames(dat),
  colnames.new = colnames(dat),
  target.features = "Location..Census.Region.")

# upload dataset to the OML server
# !DO NOT ACCIDENTALLY RUN THIS!

# uploadOMLDataSet(new_oml_dat)

# Uploading data set to server.
# Uploading to 'http://www.openml.org/api/v1/data'.
# Data set successfully uploaded. Data set ID: 41446
