library("OpenML")
library(data.table)

# load OML config file to access server
loadOMLConfig(path = "~/.openml/config", assign = TRUE)

# load dataset
dat = fread(file = "data/road-safety-drivers-sex/MakeModel2016.csv")

# basic preprocessing
dat[dat == -1] = NA

dat = dat[, .(Was_Vehicle_Left_Hand_Drive, `Engine_Capacity_(CC)`, Propulsion_Code, 
  Age_of_Vehicle, make, model, Sex_of_Driver)]

colnames(dat)[colnames(dat) == "Engine_Capacity_(CC)"] = "Engine_Capacity"

nominal_vars = c("Was_Vehicle_Left_Hand_Drive", "Propulsion_Code", "make", 
  "model", "Sex_of_Driver")

dat[, c(nominal_vars) := 
  lapply(.SD, stringr::str_replace_all, pattern = "'", replacement = ""), 
  .SDcols = c(nominal_vars)]

# remove rows with NAs in Sex_of_Driver
dat[Sex_of_Driver == 3, Sex_of_Driver := NA]
dat = dat[!is.na(Sex_of_Driver)]
dat[, Sex_of_Driver := factor(Sex_of_Driver, labels = c("male", "female"))]

# recode variables as factors
dat[, c(nominal_vars) := lapply(.SD, factor), 
  .SDcols = nominal_vars]

# check dataset for inconsistencies
str(dat)

# OML description
desc = makeOMLDataSetDescription(
  name = "road-safety-drivers-sex",
  description = "Road Safety - Vehicles by Make and Model 2016. Predict the sex of drivers involved in personal injury road accidents in Great Britain in 2016, based on characteristics of their vehicles. The data was downloaded on 27.01.2019 from [http://data.dft.gov.uk/road-accidents-safety-data/MakeModel2016.zip]. The original data was published under the Open Government Licence (OGL) [http://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/]. The variable 'Sex_of_Driver' was selected as target variable. For a description of all variables, checkout the original dataset description at [https://data.gov.uk/dataset/cb7ae6f0-4be6-4935-9277-47e5ce24a11f/road-safety-data].",
  licence = "Open Government Licence (OGL)",
  original.data.url = "http://data.dft.gov.uk/road-accidents-safety-data/MakeModel2016.zip",
  default.target.attribute = "Sex_of_Driver",
  visibility = "Everyone"
)

# create OML dataset
oml_dat = makeOMLDataSet(
  desc = desc,
  data = dat,
  colnames.old = colnames(dat),
  colnames.new = colnames(dat),
  target.features = "Sex_of_Driver")

# upload dataset to the OML server
# !DO NOT ACCIDENTALLY RUN THIS!

# uploadOMLDataSet(oml_dat)

# Uploading data set to server.
# Uploading to 'http://www.openml.org/api/v1/data'.
# Data set successfully uploaded. Data set ID: 41447
