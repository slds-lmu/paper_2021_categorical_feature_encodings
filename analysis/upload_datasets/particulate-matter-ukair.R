library(OpenML)
library(data.table)
library(fasttime)

# load OML config file to access server
loadOMLConfig(path = "~/.openml/config", assign = TRUE)

# library(rdefra)
# library(parallelMap)
# 
# stations_raw = as.data.table(ukair_catalogue())
# stations = as.data.table(ukair_get_coordinates(stations_raw))
# stations$SiteID = ukair_get_site_id(stations$UK.AIR.ID)
# 
# parallelStart(mode = "multicore", cpus = 4)
# 
# fun = function(x){
#   tryCatch(as.data.table(ukair_get_hourly_data(x, years = 2017)),
#     error = function(e)return(data.table()))
# }
# dat = parallelLapply(names(table(stations$SiteID, useNA = "no")), fun)
# 
# parallelStop()
# 
# dat = rbindlist(dat, fill = TRUE)
# dat = merge(dat, stations)

# !DO NOT ACCIDENTALLY OVERWRITE THIS!
# fwrite(dat, file = "data/ukair_2017/ukair_2017.csv")

# load dataset
dat = fread(file = "data/ukair_2017/ukair_2017.csv")

# preprocessing

dat[, datetime := fastPOSIXct(datetime, tz = "GMT")]
dat[, c("Month", "DayofWeek", "Hour") := .(month(datetime), wday(datetime), hour(datetime))]
dat[, datetime := as.character(datetime)]

dat = dat[, .(datetime, Hour, Month, DayofWeek, 
  Site.Name, Environment.Type, Zone, Latitude, Longitude, Altitude..m.,  
  PM.sub.10..sub..particulate.matter..Hourly.measured., PM.sub.2.5..sub..particulate.matter..Hourly.measured.)]
dat = dat[complete.cases(dat)]

nominal_vars = c("Month", "DayofWeek", "Site.Name", "Environment.Type", "Zone")
dat[, c(nominal_vars) := lapply(.SD, factor), .SDcols = nominal_vars]

# remove Longitude and Latitude to increase the importance of Zone and Site.Name
geo_vars = c("Longitude", "Latitude")
dat[, (geo_vars) := NULL]

# check dataset for inconsistencies
summary(dat)

# OML description
desc = makeOMLDataSetDescription(
  name = "particulate-matter-ukair-2017",
  description = "Hourly particulate matter air polution data of Great Britain for the year 2017, provided by Ricardo Energy and Environment on behalf of the UK Department for Environment, Food and Rural Affairs (DEFRA) and the Devolved Administrations on [https://uk-air.defra.gov.uk/]. The data was scraped from the UK AIR homepage via the R-package 'rdefra' [Vitolo, C., Russell, A., & Tucker, A. (2016, August). Rdefra: interact with the UK AIR pollution database from DEFRA. The Journal of Open Source Software, 1(4). doi:10.21105/joss.00051] on 09.11.2018. The data was published by DEFRA under the Open Government Licence (OGL) [http://www.nationalarchives.gov.uk/doc/open-government-licence/version/2/]. For a description of all variables, checkout the UK AIR homepage. The variable 'PM.sub.10..sub..particulate.matter..Hourly.measured.' was chosen as the target. The dataset also contains another measure of particulate matter 'PM.sub.2.5..sub..particulate.matter..Hourly.measured.' (ignored by default) which could be used as the target instead. The string variable 'datetime' (ignored by default) could be used to construct additional date/time features. In this version of the dataset, the features 'Longitude' and 'Latitude' were removed to increase the importance of the categorical features 'Zone' and 'Site.Name'.",
  licence = "Open Government Licence (OGL)",
  original.data.url = "https://uk-air.defra.gov.uk/",
  default.target.attribute = "PM.sub.10..sub..particulate.matter..Hourly.measured.",
  ignore.attribute = c("datetime", "PM.sub.2.5..sub..particulate.matter..Hourly.measured."),
  visibility = "Everyone"
)

# create OML dataset
oml_dat = makeOMLDataSet(
  desc = desc,
  data = dat,
  colnames.old = colnames(dat),
  colnames.new = colnames(dat),
  target.features = "PM.sub.10..sub..particulate.matter..Hourly.measured.")

# upload dataset to the OML server
# !DO NOT ACCIDENTALLY RUN THIS!

# uploadOMLDataSet(oml_dat)

# Uploading data set to server.
# Uploading to 'http://www.openml.org/api/v1/data'.
# Data set successfully uploaded. Data set ID: 42207
