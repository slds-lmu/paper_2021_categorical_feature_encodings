# problem function to prepare the datasets

provideTaskWithPreProc = function(job, data) {
  # read dataset from cache
  oml_dat = getOMLDataSet(data.id = data, cache.only = TRUE)
  # convert dataset to task
  task = convertOMLDataSetToMlr(oml_dat)
  task = task %>>% 
    # avoid strange bugs with cpoDummyEncode
    cpoStandardizeFeatNames()
  
  return(task)
}