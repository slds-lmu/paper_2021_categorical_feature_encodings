library("OpenML")
library(data.table)

# load OML config file to access server
loadOMLConfig(path = "~/.openml/config", assign = TRUE)

# load dataset
dat = fread(file = "data/nyc_taxi/green_tripdata_2016-12.csv")
dat = dat[, 1:19]
colnames(dat) = as.character(
  read.csv(file = "data/nyc_taxi/green_tripdata_2016-12.csv", 
    header = F, nrows = 1, as.is = T)
)

# basic preprocessing

dat[, "ehail_fee" := NULL]

# select only payment type 1 (credit card) 
# tips are not recorded most remaining types
dat = dat[payment_type == 1,]

nominal_vars = c("VendorID", "store_and_fwd_flag", "RatecodeID", "PULocationID", 
  "DOLocationID", "extra", "mta_tax", "improvement_surcharge", 
  "payment_type", "trip_type")
dat[, c(nominal_vars) := lapply(.SD, factor), .SDcols = nominal_vars]

# remove variables to increase the importance of categorical features
rm_vars = c("trip_distance", "payment_type", "fare_amount")
dat[, (rm_vars) := NULL]

# check dataset for inconsistencies
summary(dat)

# OML description
desc = makeOMLDataSetDescription(
  name = "nyc-taxi-green-dec-2016",
  description = "Trip Record Data provided by the New York City Taxi and Limousine Commission (TLC) [http://www.nyc.gov/html/tlc/html/about/trip_record_data.shtml]. The dataset includes TLC trips of the green line in December 2016. Data was downloaded on 03.11.2018. For a description of all variables in the dataset checkout the TLC homepage [http://www.nyc.gov/html/tlc/downloads/pdf/data_dictionary_trip_records_green.pdf]. The variable 'tip_amount' was chosen as target variable. The variable 'total_amount' is ignored by default, otherwise the target could be predicted deterministically. The date variables 'lpep_pickup_datetime' and 'lpep_dropoff_datetime' (ignored by default) could be used to compute additional time features. In this version, we chose only trips with 'payment_type' == 1 (credit card), as tips are not included for most other payment types. We also removed the variables 'trip_distance' and 'fare_amount' to increase the importance of the categorical features 'PULocationID' and 'DOLocationID'.",
  original.data.url = "http://www.nyc.gov/html/tlc/html/about/trip_record_data.shtml", 
  default.target.attribute = "tip_amount", 
  ignore.attribute = c("lpep_pickup_datetime", "lpep_dropoff_datetime", "total_amount"),
  visibility = "Everyone"
)

# create OML dataset
oml_dat = makeOMLDataSet(
  desc = desc,
  data = dat,
  colnames.old = colnames(dat),
  colnames.new = colnames(dat), 
  target.features = "tip_amount")

# upload dataset to the OML server
# !DO NOT ACCIDENTALLY RUN THIS!

# uploadOMLDataSet(oml_dat)

# Uploading data set to server.
# Uploading to 'http://www.openml.org/api/v1/data'.
# Data set successfully uploaded. Data set ID: 42208
