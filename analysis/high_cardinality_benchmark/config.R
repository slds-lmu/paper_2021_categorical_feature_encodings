options(java.parameters = "-Xmx8000m")

library(OpenML)
library(mlrCPO)
library(BBmisc)
library(checkmate)

configureMlr(on.learner.error = "warn", on.learner.warning = "warn",
  on.par.without.desc = "warn", on.par.out.of.bounds = "warn",
  on.measure.not.applicable = "stop", show.learner.output = TRUE,
  on.error.dump = FALSE, show.info = TRUE)