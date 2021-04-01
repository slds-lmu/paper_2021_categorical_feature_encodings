cluster.functions = batchtools::makeClusterFunctionsSlurm("slurm-lrz")
default.resources = list(walltime = 10000L, # in seconds
  memory = 1700L, nodes = 1L, ntasks = 1L, ncpus = 1L, 
  clusters = "serial", partition = "serial_mpp2")
max.concurrent.jobs = 1000L

