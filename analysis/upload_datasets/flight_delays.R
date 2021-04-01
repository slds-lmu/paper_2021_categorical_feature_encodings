library("OpenML")
library(data.table)

# load OML config file to access server
loadOMLConfig(path = "~/.openml/config", assign = TRUE)

# load dataset
dat = fread(file = "data/flight_delays/660669749_T_ONTIME_REPORTING_2017_12.csv")

# basic preprocessing

dat[, c("V15", "CANCELLED", "DIVERTED") := NULL]
dat = dat[!is.na(ARR_DELAY),]

nominal_vars = c("DAY_OF_MONTH", "DAY_OF_WEEK", "OP_UNIQUE_CARRIER", "ORIGIN", 
  "ORIGIN_STATE_NM", "DEST", "DEST_STATE_NM")
dat[, c(nominal_vars) := lapply(.SD, factor), .SDcols = nominal_vars]

# check dataset for inconsistencies
summary(dat)

# OML description
desc = makeOMLDataSetDescription(
  name = "flight-delay-usa-dec-2017",
  description = "Predicting US flight delay in December 2017 based on airline on-time performance data provided by the Bureau of Transportation Statistics (BTS) [https://www.transtats.bts.gov/Tables.asp?DB_ID=120]. Data was downloaded on 03.11.2018. For a description of all variables included in this dataset (many more are available from the BTS website) checkout [https://www.transtats.bts.gov/Fields.asp]. All flights which got canceled or diverted were removed. The dataset includes the variables 'FL_DATE' and 'CRS_DEP_TIME' (ignored by default) which can be used to construct additional time features.",
  original.data.url = "https://www.transtats.bts.gov/Tables.asp?DB_ID=120",
  default.target.attribute = "ARR_DELAY",
  ignore.attribute = c("FL_DATE", "CRS_DEP_TIME"),
  visibility = "Everyone"
)

# create OML dataset
oml_dat = makeOMLDataSet(
  desc = desc,
  data = dat,
  colnames.old = colnames(dat),
  colnames.new = colnames(dat),
  target.features = "ARR_DELAY")

# upload dataset to the OML server
# !DO NOT ACCIDENTALLY RUN THIS!

# uploadOMLDataSet(oml_dat)

# Uploading data set to server.
# Uploading to 'http://www.openml.org/api/v1/data'.
# Data set successfully uploaded. Data set ID: 41251


