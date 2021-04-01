library("OpenML")
library(data.table)

# load OML config file to access server
loadOMLConfig(path = "~/.openml/config", assign = TRUE)

# load dataset
library(AmesHousing)
dat = as.data.table(make_ames())

# check dataset for inconsistencies
summary(dat)

# OML description
desc = makeOMLDataSetDescription(
  name = "ames-housing",
  description = "A processed version of the 'Ames Iowa Housing' dataset as provided by the make_ames() function in the R-package 'AmesHousing' [Max Kuhn (2017). AmesHousing: The Ames Iowa Housing Data. R package version 0.0.3. https://CRAN.R-project.org/package=AmesHousing]. The original data was published in [De Cock, D. (2011). 'Ames, Iowa: Alternative to the Boston Housing Data as an End of Semester Regression Project', Journal of Statistics Education, Volume 19, Number 3]. For a description of the dataset checkout either the documentation of the R-package or the original publication. The variable 'Sale_Price' was chosen as target variable. Note that all factors are unordered in this version of the dataset as provided by the make_ames() function, in contrast to the version provided by the make_ordinal_ames() function.",
  licence = NA,
  paper.url = "https://www.tandfonline.com/doi/abs/10.1080/10691898.2011.11889627",
  default.target.attribute = "Sale_Price",
  visibility = "Everyone"
)

# create OML dataset
oml_dat = makeOMLDataSet(
  desc = desc,
  data = dat,
  colnames.old = colnames(dat),
  colnames.new = colnames(dat),
  target.features = "Sale_Price")

# upload dataset to the OML server
# !DO NOT ACCIDENTALLY RUN THIS!

# uploadOMLDataSet(oml_dat)

# Uploading data set to server.
# Uploading to 'http://www.openml.org/api/v1/data'.
# Data set successfully uploaded. Data set ID: 41211
