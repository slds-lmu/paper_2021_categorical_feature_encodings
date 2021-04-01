library(batchtools)

# load registry in read-only mode
reg = loadRegistry(file.dir = "analysis/high_cardinality_benchmark/registry", writeable = FALSE)

# save all test set evaluations as data.table: list column result, each entry is a data.frame
res = reduceResultsDataTable(reg = reg, fun = function(x) x$measures.test)
  
# new unwrapped data.table with each row refering one test set evaluation
dat = data.table()
for(i in seq_row(res)) {
  dat = rbind(dat, res[i, data.table(job.id, rbindlist(result, fill = TRUE))], fill = TRUE)
}

# compute test mean, sd, min, max for each job.id
# aggregate dataset on job.id level
mes_names = colnames(dat[, multiclass.aunp:brier])
dat = dat[, c(lapply(.SD, mean), lapply(.SD, sd), lapply(.SD, min), lapply(.SD, max)), 
  by = job.id, .SDcols = mes_names]
setnames(dat, old = 2:ncol(dat), paste(mes_names, rep(c("test.mean", "test.sd", "test.min", "test.max"), 
  each = length(mes_names)), sep = "."))

# load dataset descriptives
descr_dat = readRDS("analysis/high_cardinality_benchmark/descr_dat.rds")
# merge job parameters, descriptives, measures
descr_dat$OmlId = as.character(descr_dat$OmlId)
dat = unwrap(getJobTable(reg = reg))[descr_dat, on = c("problem==OmlId")][
  dat, on = "job.id"]

# remove problematic high.card.thresh conditions (in doubt remove highest one)
# make sure that:
# no condition is effectively computed multiple times
# algorithm == "remove" always removes something
# algorithm %in% thresh_encs alway encode anything
rmJobs= function(by, dt) {
  df = dt[, tstrsplit(HighCardLevels, ", ")]
  df = df[, lapply(.SD, as.numeric)]
  df = df[, lapply(.SD, function(x) x > dt$high.card.thresh)]
    
  ind_1 = rowSums(df) == 0
  ind_2 = duplicated(df)
    
  if (by == "ReInFrLeImLmRa") {
    return(dt[!(ind_1 | ind_2)])    
  } else if (by == "Du") {
    return(dt[!(ind_1 & ind_2)])
  } else if (by == "HaCl") {
    return(dt[!ind_1])
  } else if (by == "No") {
    return(dt)
  } else {
    warning("Something is terribly wrong!")
  }
}
  
dat = rbind(
  dat[algorithm %in% c("lmer", "ranger")][
    , rmJobs("ReInFrLeImLmRa",.SD), by = c("algorithm", "problem", "lrn.id", "n.folds", "num.trees")],
  dat[algorithm %in% c("remove", "integer", "frequency", "leaf", "impact")][
    , rmJobs("ReInFrLeImLmRa",.SD), by = c("algorithm", "problem", "lrn.id")],
  dat[algorithm == "dummy"][
    , rmJobs("Du",.SD), by = c("algorithm", "problem", "lrn.id", "dummy.enc")],
  dat[algorithm %in% c("hash", "cluster")][
    , rmJobs("HaCl",.SD), by = c("algorithm", "problem", "lrn.id")],
  dat[algorithm == "none"][
    , rmJobs("No",.SD), by = c("algorithm", "problem", "lrn.id")]
)

# remove all results for three not finished datasets
dat = dat[!(problem %in% c("41435", "41443", "41436"))]

# save data.table
saveRDS(file = "analysis/high_cardinality_benchmark/results.rds", 
  object = dat)
