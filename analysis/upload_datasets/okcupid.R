library("OpenML")
library(data.table)

# load OML config file to access server
loadOMLConfig(path = "~/.openml/config", assign = TRUE)

# load dataset
dat = fread(file = "data/jse_okcupid/profiles.csv")

# basic preprocessing

dat[dat == ""] = NA
dat[dat == -1] = NA

dat = dat[!is.na(job) & !(job %in% c("unemployed", "retired", "rather not say"))]
dat = dat[, -grep("essay", names(dat), value = TRUE), with = FALSE]

nominal_vars = c("body_type", "diet", "drinks", "drugs", "education", "ethnicity",
  "income", "job", "location", "offspring", "orientation", "pets", "religion", 
  "sex", "sign", "smokes", "speaks", "status")
dat[, c(nominal_vars) := lapply(.SD, factor), .SDcols = nominal_vars]

stem_jobs = c("computer / hardware / software", "science / tech / engineering")
levels(dat$job) = list(
  stem = stem_jobs, 
  non_stem = setdiff(levels(dat$job), c(stem_jobs, "student")), 
  student = c("student")
)

# check dataset for inconsistencies
summary(dat)

# OML description
desc = makeOMLDataSetDescription(
  name = "okcupid-stem",
  description = "User profile data for San Francisco OkCupid users published in [Kim, A. Y., & Escobedo-Land, A. (2015). OKCupid data for introductory statistics and data science courses. Journal of Statistics Education, 23(2).]. The curated dataset was downloaded from [https://github.com/rudeboybert/JSE_OkCupid]. The original dataset was created with the use of a python script that pulled the data from public profiles on www.okcupid.com on 06/30/2012. It includes people (n = 59946) within a 25 mile radius of San Francisco, who were online in the last year (06/30/2011), with at least one profile picture. Permission to use this data was obtained by the author of the original paper from OkCupid president and co-founder Christian Rudder under the condition that the dataset remains public. As target, the variable 'job' was collapsed into three categories: 'stem', 'non_stem', and 'student'. STEM jobs were defined as 'job' %in% c('computer / hardware / software', 'science / tech / engineering'). Observations with 'job' %in% c('unemployed', 'retired', 'rather not say') or missing values in 'job' were removed. The original dataset also included ten open text variables 'essay0' to 'essay9', which were removed from the dataset uploaded here. The dataset further includes the date/time variable 'last_online' (ignored by default) which could be used to construct additional features. Using OkCupid data for predicting STEM jobs was inspired by Max Kuhns book 'Feature Engineering and Selection: A Practical Approach for Predictive Models' [https://bookdown.org/max/FES/].",
  original.data.url = "https://github.com/rudeboybert/JSE_OkCupid/blob/master/profiles.csv.zip", 
  citation = "Kim, A. Y., & Escobedo-Land, A. (2015). OKCupid data for introductory statistics and data science courses. Journal of Statistics Education, 23(2).",
  paper.url = "https://github.com/rudeboybert/JSE_OkCupid/blob/master/JSE.pdf",
  default.target.attribute = "job",
  ignore.attribute = "last_online",
  visibility = "Everyone"
)

# create OML dataset
oml_dat = makeOMLDataSet(
  desc = desc,
  data = as.data.frame(dat),
  colnames.old = colnames(dat),
  colnames.new = colnames(dat), 
  target.features = "job")

# upload dataset to the OML server
# !DO NOT ACCIDENTALLY RUN THIS!

# uploadOMLDataSet(oml_dat)

# Uploading data set to server.
# Uploading to 'http://www.openml.org/api/v1/data'.
# Data set successfully uploaded. Data set ID: 41440

# old version with integers in speaks
# Uploading data set to server.
# Uploading to 'http://www.openml.org/api/v1/data'.
# Data set successfully uploaded. Data set ID: 41278

