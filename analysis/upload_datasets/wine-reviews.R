library(OpenML)
library(data.table)

# load OML config file to access server
loadOMLConfig(path = "~/.openml/config", assign = TRUE)

# load dataset
dat = fread(file = "data/wine_reviews/winemag-data-130k-v2.csv")

# basic preprocessing

dat[, V1 := NULL]

dat[dat == ""] = NA

nominal_vars = c("country", "province", "region_1", 
  "taster_name", "variety", "winery")

text_vars = c("designation", "region_2", "description", "taster_twitter_handle", "title")

dat[, c(text_vars) := lapply(.SD, stringi::stri_trans_general, id = "latin-ascii"), 
   .SDcols = text_vars]

dat[, c(text_vars) := 
  lapply(.SD, stringr::str_replace_all, pattern = "[^[:alnum:]^[:blank:]^[:punct:]]", replacement = ""), 
  .SDcols = text_vars]

# "'" are not allowed in nominal factors by OpenML
dat[, c(text_vars, nominal_vars) := 
  lapply(.SD, stringr::str_replace_all, pattern = "'", replacement = " "), 
  .SDcols = c(text_vars, nominal_vars)]

dat[, c(text_vars) := 
  lapply(.SD, stringr::str_squish), 
  .SDcols = text_vars]

# dat[, c(nominal_vars) := lapply(.SD, function(x) factor(match(x, unique(na.omit(x))))), 
#   .SDcols = nominal_vars]

dat[, c(nominal_vars) := lapply(.SD, factor), 
  .SDcols = nominal_vars]


# check dataset for inconsistencies
summary(dat)

# OML description
desc = makeOMLDataSetDescription(
  name = "wine-reviews",
  description = "130k wine reviews with variety, location, winery, price, and description. Downloaded from Kaggle [https://www.kaggle.com/zynicide/wine-reviews/home] on 29.10.2018. The original data was scraped from the WineEnthusiast homepage [https://www.winemag.com/?s=&drink_type=wine]. The second version of the dataset was used, which was scraped on 22.11.2017. The Kaggle dataset was licensed under Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International (CC BY-NC-SA 4.0) [https://creativecommons.org/licenses/by-nc-sa/4.0/]. The variable 'points' (the number of points WineEnthusiast rated the wine on a scale of 1-100) was selected as target variable. For a description of all variables, checkout the Kaggle dataset repo. The variable 'region_2' is ignored by default as it contains a large portion of missing values. The variable 'designation' is not used by default, as the number of factor labels is extremely high compared to the number of observations. The dataset further includes the text based variables 'description', 'taster_twitter_handle', and 'title' (ignored by default) which could be used to construct additional features. Special characters in text features have been removed to allow the upload to the platform. The ID variable from the Kaggle version was removed from the dataset.",
  licence = "CC BY-NC-SA 4.0",
  original.data.url = "https://www.kaggle.com/zynicide/wine-reviews#winemag-data-130k-v2.csv",
  default.target.attribute = "points",
  ignore.attribute = c("region_2", "designation", 
    "description", "taster_twitter_handle", "title"),
  visibility = "Everyone"
)

# create OML dataset
oml_dat = makeOMLDataSet(
  desc = desc,
  data = dat,
  colnames.old = colnames(dat),
  colnames.new = colnames(dat),
  target.features = "points"
)

# upload dataset to the OML server
# !DO NOT ACCIDENTALLY RUN THIS!

# uploadOMLDataSet(oml_dat)

# Uploading data set to server.
# Uploading to 'http://www.openml.org/api/v1/data'.
# Data set successfully uploaded. Data set ID: 41437

# old version with integers in factor features
# Uploading data set to server.
# Uploading to 'http://www.openml.org/api/v1/data'.
# Data set successfully uploaded. Data set ID: 41275
