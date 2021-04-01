options(java.parameters = "-Xmx8000m")

library(OpenML)
library(data.table)

# load OML config file to access server
loadOMLConfig(path = "~/.openml/config", assign = TRUE)

# employee_salaries
oml_dat = getOMLDataSet(41204)
desc = oml_dat$desc
dat = as.data.table(oml_dat$data)

# remove variables
dat[, c("Full_Name", "X2016_Gross_Pay_Received", "X2016_Overtime_Pay", "Department", "Underfilled_Job_Title", "Date_First_Hired") := NULL]

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
                      The variable 'Current_Annual_Salary' is used as target by default."),
  original.data.url = desc$url,
  default.target.attribute = "Current_Annual_Salary",
  licence = desc$licence,
  visibility = desc$visibility
  )

new_oml_dat = makeOMLDataSet(
  desc = new_desc,
  data = dat,
  colnames.old = colnames(dat),
  colnames.new = colnames(dat),
  target.features = "Current_Annual_Salary")

# upload dataset to the OML server
# !DO NOT ACCIDENTALLY RUN THIS!

# uploadOMLDataSet(new_oml_dat)

# Uploading data set to server.
# Uploading to 'http://www.openml.org/api/v1/data'.
# Data set successfully uploaded. Data set ID: 41445

