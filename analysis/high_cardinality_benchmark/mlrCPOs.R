#' @title Convert All Features to Numerics
#'
#' @template cpo_doc_intro
#'
#' @description
#' Converts all \code{feature} columns to randomized (integer) \code{numeric} 
#' columns. Missing values are ignored by the CPO. New factor levels are converted
#' to NAs during prediction.
#'
#'#' @section CPOTrained State:
#' The state's \code{$control} slot is a named list, containing for each factorial
#' feature a named integer vector indicating the numeric value for each observed
#' factor level.
#'
#' @template cpo_doc_outro
#' @export
cpoAsNumeric2 = makeCPO("as.numeric.2", properties.adding = c("factors", "ordered"), 
  properties.needed = "numerics",
  dataformat = "factor",
  fix.factors = TRUE,
  cpo.train = function(data, target){
    lapply(data, function(col) {
      setNames(1:nlevels(col), nm = sample(levels(col)))
    })
  }, 
  cpo.retrafo = function(data, control) {
    data[] = lapply(colnames(data), function(cname) {
      as.numeric(control[[cname]][as.character(data[[cname]])])
      })
    data
  })

#' @title Standardize Feature Names
#'
#' @template cpo_doc_intro
#'
#' @description
#' Create standardized feature names (Feat1, Feat2, ...) to avoid rare bugs
#' if some learners cannot handle special symbols in column names (e.g. liquidSVM). 
#' Useful after CPO encoders that name new features based on factor levels.
#'
#' @section CPOTrained State:
#' NULL
#' 
#' @template cpo_doc_outro
#' @export
cpoStandardizeFeatNames = makeCPO("standardize.feat.names",
  dataformat = "df.features",
  cpo.train = NULL,
  cpo.retrafo = function(data) {
    if (ncol(data) > 0) {
      names(data) = paste0("..Feat", 1:ncol(data), "..")
    }
    data
  })

#' @title New Factor Level for Missing Values
#'
#' @template cpo_doc_intro
#'
#' @description
#' This CPO creates a new factor level labeled ..NA.. for factorial columns
#' containing missing values.
#'
#' @section CPOTrained State:
#' NULL
#' 
#' @template cpo_doc_outro
#' @export
cpoNaToFactLvl = makeCPO("na.to.fact.lvl",
  dataformat = "factor",
  properties.data = c("missings", "numerics", "factors", "ordered"),
  properties.adding = "missings.sometimes",
  properties.needed = "factors",
  fix.factors = FALSE,
  cpo.train = NULL,
  cpo.retrafo = function(data) {
    NA_index = vlapply(data, anyNA)
    data[NA_index] = lapply(data[NA_index], function(col) {
      fac = addNA(col, ifany = TRUE)
      # rename NA level as it might not work with some learners (e.g. ranger)
      levels(fac)[is.na(levels(fac))] = "..NA.."
      fac
    })
    data
  })

#' @title Combine Rare Factor Levels 2
#'
#' @template cpo_doc_intro
#'
#' @description
#' This CPO combines rare factor levels into a single factor level, by specifying 
#' the desired number of levels for the new factor.
#'
#' @section CPOTrained State:
#' The state's \code{$control} slot is a named list, containing for each factorial
#' feature a character vector of all levels sorted by decreasing frequency.
#' 
#' @template cpo_doc_outro
#' @export
cpoCollapseFact2 = makeCPO("collapse.fact.2",
  pSS(n.levels = 10: integer [2, ]),
  dataformat = "factor",
  properties.data = c("missings", "numerics", "factors", "ordered"),
  properties.adding = "missings.sometimes",
  properties.needed = "factors",
  fix.factors = FALSE,
  cpo.train = function(data, target, n.levels) {
    sapply(colnames(data), function(cname) {
      names(sort(table(data[[cname]], useNA = "no"), decreasing = TRUE))
    }, simplify = FALSE)
  },
  cpo.retrafo = function(data, control, n.levels) {
    data[] = lapply(colnames(data), function(cname) {
      if (n.levels >= nlevels(data[[cname]])) {
        return(data[[cname]])
      }
      # find smallest levels
      collapse_lvls = control[[cname]][n.levels : length(control[[cname]])]
      # add new levels
      new_lvls = levels(data[[cname]])
      new_lvls = new_lvls[!(new_lvls %in% control[[cname]])]
      collapse_lvls = union(collapse_lvls, new_lvls)
      
      # collapse levels
      levels(data[[cname]])[levels(data[[cname]]) %in% collapse_lvls] =
        "..collapsed..level.."
      data[[cname]]
    })
    data
  })


#' @title Frequency and Frequency Rank Encoding
#'
#' @template cpo_doc_intro
#'
#' @description
#' Frequency encoding converts factor levels of each factorial column to the 
#' absolute frequency of the factor level during training.
#' Frequency rank encoding ranks all factor levels according to their frequency
#' during training in decreasing order (rank 1 for the most frequent level).
#' New factor levels are converted to a frequency of 1 (Frequency Encoding) or
#' to the lowest observed rank + 1 (Frequency Rank Encoding) during prediction.
#' NAs are ignored by the CPO.
#'
#' @section CPOTrained State:
#' The state's \code{$control} slot is a list of tables (\code{useNA = "no"}) for
#' each factorial data column, indicating the absolute frequencies of all levels.
#'
#' @param rank [\code{logical}]\cr
#'   If \dQuote{rank} is \code{TRUE}, Frequency Rank Encoding is used. 
#'   Otherwise, Frequency Encoding is employed (the default).
#' @template cpo_doc_outro
#' @export
cpoFreqEncode = makeCPO("freq.encode",
  pSS(rank = FALSE: logical),
  dataformat = "factor",
  properties.data = c("missings", "numerics", "factors", "ordered"),
  properties.adding = c("factors", "ordered"),
  properties.needed = c("numerics", "missings.sometimes"),
  fix.factors = FALSE,
  cpo.train = function(data, target, rank) {
    lapply(data, table, useNA = "no")
  },
  cpo.retrafo = function(data, control, rank) {
    if(rank) {
      data[] = lapply(colnames(data), function(cname) {
        # add new level for all values not present during training
        fac = flagNewLvls(data[[cname]], names(control[[cname]]))
        ind_new = fac == "..new..level.."
        
        num_vals = match(as.character(fac),
          names(control[[cname]][order(control[[cname]], decreasing = TRUE)]))
        # substitue new levels with new highest rank
        tail_rank = length(control[[cname]])
        num_vals[ind_new & !is.na(ind_new)] = tail_rank + 1L
        num_vals
      })
    } else {
      data[] = lapply(colnames(data), function(cname) {
        # add new level for all values not present during training
        fac = flagNewLvls(data[[cname]], names(control[[cname]]))
        ind_new = fac == "..new..level.."
        
        num_vals = as.numeric(control[[cname]][as.character(fac)])
        # substitute new levels with freq == 1
        num_vals[ind_new & !is.na(ind_new)] = 1L
        num_vals
      })
    }
    data
  })

#' @title Impact Encoding with Random Intercept Models
#'
#' @template cpo_doc_intro
#'
#' @description
#' cpoLmerEncodeRegr() converts factor levels of each factorial column to the 
#' estimated coefficients of a simple random intercept model.
#' Models are fitted with the lmer function of the lme4 package and are 
#' of the type \code{target ~ 1 + (1 | factor)}. Thus, the numeric target 
#' variable is used as dependent variable and the factor is used for grouping.
#' For training, multiple models can be estimated in a cross-validation scheme
#' to ensure that the same factor level does not always result in identical
#' values in the converted numerical feature.
#' For prediction, a global model (which was fitted on all observations 
#' during training) is used for each factor. 
#' New factor levels are converted to the value of the intercept coefficient
#' of the global model for prediction.
#' NAs are ignored by the CPO.
#' 
#' @section CPOTrained State:
#' The state's \code{$control} slot is a named list of named vectors for each
#' factorial data column. Each vector contains the estimates for each factor level
#' from the global model fitted to that factor with all observations during training. 
#'
#' @param n.folds [\code{integer(1)}]\cr
#'   Number of folds used in the cross-validation scheme during training.
#'   With \code{n.folds = 1} the single global model for each factor 
#'   (which is used during predictions) is also used during training. 
#'   Default is \code{5L}.
#' @param fast.optim [\code{logical}]\cr
#'   If \dQuote{fast.optim} is \code{TRUE} (default), a faster (up to 50 percent)
#'   optimizer from the nloptr package is used when fitting the lmer models. 
#'   This uses additional stopping criteria which can give suboptimal results.   
#' @template cpo_doc_outro
#' @export
cpoLmerEncodeRegr = makeCPOExtendedTrafo("lmer.encode.regr",
  pSS(n.folds = 5: integer [1, ], fast.optim = TRUE: logical),
  dataformat = "factor",
  properties.data = c("missings", "numerics", "factors", "ordered"),
  properties.adding = c("factors", "ordered"),
  properties.needed = c("numerics", "missings.sometimes"),
  properties.target = "regr",
  packages = "lme4",
  fix.factors = FALSE,
  cpo.trafo = function(data, target, n.folds, fast.optim) {

    # heavily influenced by the embed package
    fitLmer = function(feature, target) {
      # performance tips from https://cran.r-project.org/web/packages/lme4/vignettes/lmerperf.html
      nlopt <- function(par, fn, lower, upper, control) {
      .nloptr <<- res <- nloptr::nloptr(par, fn, lb = lower, ub = upper, 
        opts = list(algorithm = "NLOPT_LN_BOBYQA", print_level = 1,
        maxeval = 1000, xtol_abs = 1e-6, ftol_abs = 1e-6))
      list(par = res$solution,
        fval = res$objective,
        conv = if (res$status > 0) 0 else res$status,
        message = res$message)
      }
      args = list(formula = y ~ 1 + (1 | lvl),
        data = data.frame(lvl = feature, y = target),
        na.action = na.omit,
        control = if (fast.optim) {
          lme4::lmerControl(optimizer = "nloptwrap", calc.derivs = FALSE)
        } else {
          lme4::lmerControl()
        })
      mod = do.call(lme4::lmer, args)
      coefs = coef(mod)$lvl
      lvls = rownames(coefs)
      coefs = coefs[,1]
      names(coefs) = lvls
      intercept = unname(lme4::fixef(mod))
      # replace missing coefs with intercept value
      coefs[is.na(coefs)] = intercept
      # save intercept value for new levels during prediction
      coefs = c(coefs, ..new..level.. = intercept)
      coefs
    }
    
    # for prediction, use complete encoding model
    control = lapply(data, function(col) {
      fitLmer(col, target[[1]])
      })
    
    # if n.folds == 1 use only the complete model in training
    if (n.folds == 1) {
      data[] = lapply(colnames(data), function(cname) {
        as.numeric(control[[cname]][as.character(data[[cname]])])
      })
      return(data)
    }
    
    # else use n.folds encoding models in crossvalidation scheme in training
    rinst = makeResampleInstance(makeResampleDesc("CV", iters = n.folds), 
      size = nrow(data))
    # list with n.folds models for each data column
    mod_list = lapply(colnames(data), function(cname) {
      lapply(rinst$train.inds, function(inds) {
        fitLmer(data[[cname]][inds], target[[1]][inds])
        })
      })
    names(mod_list) = names(data)
    # list with replaced values in n.folds for each data column
    num_vals_list = lapply(colnames(data), function(cname) {
      lapply(seq_len(n.folds), function(fold) {
        # add new level for all values not present during training
        fac = flagNewLvls(data[[cname]][rinst$test.inds[[fold]]],
          names(mod_list[[cname]][[fold]]))
        mod = mod_list[[cname]][[fold]]
        
        as.numeric(mod[as.character(fac)])
        })
      })
    names(num_vals_list) = names(mod_list)
    # recombine replaced values from n.folds for each data column
    data[] = lapply(seq_along(data), function(cnum) {
      unlist(num_vals_list[[cnum]])[order(unlist(rinst$test.inds))]
      })
    data
  },
  cpo.retrafo = function(data, control, n.folds, fast.optim) {
    # use complete encoding model for new prediction data
    data[] = lapply(colnames(data), function(cname) {
      # add new level for all values not present during training
      fac = flagNewLvls(data[[cname]], names(control[[cname]]))
     
      as.numeric(control[[cname]][as.character(fac)])
      })
    data
  })

#' @title Impact Encoding with Random Intercept Models
#'
#' @template cpo_doc_intro
#'
#' @description
#' cpoLmerTwoClassif() converts factor levels of each factorial column to the
#' estimated coefficients of a simple random intercept model.
#' Models are fitted with the glmer function of the lme4 package and are 
#' of the type \code{target ~ 1 + (1 | factor)}. Thus, the binary target 
#' variable is used as dependent variable and the factor is used for grouping.
#' For training, multiple models can be estimated in a cross-validation scheme
#' to ensure that the same factor level does not always result in identical
#' values in the converted numerical feature.
#' For prediction, a global model (which was fitted on all observations 
#' during training) is used for each factor. 
#' New factor levels are converted to the value of the intercept coefficient
#' of the global model for prediction.
#' NAs are ignored by the CPO.
#' 
#' @section CPOTrained State:
#' The state's \code{$control} slot is a named list of named vectors for each
#' factorial data column. Each vector contains the estimates for each factor level
#' from the global model fitted to that factor with all observations during training. 
#'
#' @param n.folds [\code{integer(1)}]\cr
#'   Number of folds used in the cross-validation scheme during training.
#'   With \code{n.folds = 1} the single global model for each factor 
#'   (which is used during predictions) is also used during training. 
#'   Default is \code{5L}.
#' @param fast.optim [\code{logical}]\cr
#'   If \dQuote{fast.optim} is \code{TRUE} (default), a faster (up to 50 percent)
#'   optimizer from the nloptr package is used when fitting the glmer models. 
#'   This uses additional stopping criteria which can give suboptimal results.   
#' @template cpo_doc_outro
#' @export
cpoLmerEncodeTwoClassif = makeCPOExtendedTrafo("lmer.encode.two.classif",
  pSS(n.folds = 5: integer [1, ], fast.optim = TRUE: logical),
  dataformat = "factor",
  properties.data = c("missings", "numerics", "factors", "ordered"),
  properties.adding = c("factors", "ordered"),
  properties.needed = c("numerics", "missings.sometimes"),
  properties.target = c("twoclass", "classif"),
  packages = "lme4",
  fix.factors = FALSE,
  cpo.trafo = function(data, target, n.folds, fast.optim) {

    # heavily influenced by the embed package
    fitGlmer = function(feature, target) {
      # performance tips from https://cran.r-project.org/web/packages/lme4/vignettes/lmerperf.html
      nlopt <- function(par, fn, lower, upper, control) {
      .nloptr <<- res <- nloptr::nloptr(par, fn, lb = lower, ub = upper, 
        opts = list(algorithm = "NLOPT_LN_BOBYQA", print_level = 1,
        maxeval = 1000, xtol_abs = 1e-6, ftol_abs = 1e-6))
      list(par = res$solution,
        fval = res$objective,
        conv = if (res$status > 0) 0 else res$status,
        message = res$message)
      }
      args = list(formula = y ~ 1 + (1 | lvl),
        data = data.frame(lvl = feature, y = target),
        family = stats::binomial,
        na.action = na.omit,
        control = if (fast.optim) {
          lme4::glmerControl(optimizer = "nloptwrap", calc.derivs = FALSE)
        } else {
          lme4::glmerControl()
        })
      mod = do.call(lme4::glmer, args)
      coefs = coef(mod)$lvl
      lvls = rownames(coefs)
      coefs = coefs[,1]
      names(coefs) = lvls
      intercept = unname(lme4::fixef(mod))
      # replace missing coefs with intercept value
      coefs[is.na(coefs)] = intercept
      # save intercept value for new levels during prediction
      coefs = c(coefs, ..new..level.. = intercept)
      coefs
    }
    
    # for prediction, use complete encoding model
    control = lapply(data, function(col) {
      fitGlmer(col, target[[1]])
      })
    
    # if n.folds == 1 use only the complete model in training
    if (n.folds == 1) {
      data[] = lapply(colnames(data), function(cname) {
        as.numeric(control[[cname]][as.character(data[[cname]])])
      })
      return(data)
    }
    
    # else use n.folds encoding models in crossvalidation scheme in training
    rinst = makeResampleInstance(makeResampleDesc("CV", iters = n.folds), 
      size = nrow(data))
    # list with n.folds models for each data column
    mod_list = lapply(colnames(data), function(cname) {
      lapply(rinst$train.inds, function(inds) {
        fitGlmer(data[[cname]][inds], target[[1]][inds])
        })
      })
    names(mod_list) = names(data)
    # list with replaced values in n.folds for each data column
    num_vals_list = lapply(colnames(data), function(cname) {
      lapply(seq_len(n.folds), function(fold) {
        # add new level for all values not present during training
        fac = flagNewLvls(data[[cname]][rinst$test.inds[[fold]]],
          names(mod_list[[cname]][[fold]]))
        mod = mod_list[[cname]][[fold]]
        
        as.numeric(mod[as.character(fac)])
        })
      })
    names(num_vals_list) = names(mod_list)
    # recombine replaced values from n.folds for each data column
    data[] = lapply(seq_along(data), function(cnum) {
      unlist(num_vals_list[[cnum]])[order(unlist(rinst$test.inds))]
      })
    data
  },
  cpo.retrafo = function(data, control, n.folds, fast.optim) {
    # use complete encoding model for new prediction data
    data[] = lapply(colnames(data), function(cname) {
      # add new level for all values not present during training
      fac = flagNewLvls(data[[cname]], names(control[[cname]]))
     
      as.numeric(control[[cname]][as.character(fac)])
      })
    data
  })

#' @title Impact Encoding with Random Intercept Models
#'
#' @template cpo_doc_intro
#'
#' @description
#' cpoLmerMultiClassif() converts factor levels of each factorial column to the
#' estimated coefficients of simple random intercept models.
#' For each level of the multiclass target variable, binary "one vs. rest" models
#' are fitted with the glmer function of the lme4 package. Models are 
#' of the type \code{target ~ 1 + (1 | factor)}. Thus, the binary "one vs. rest" 
#' target variable for the respective level of the target variable is used as 
#' dependent variable and the factor is used for grouping.
#' For training, multiple models can be estimated in a cross-validation scheme
#' to ensure that the same factor level does not always result in identical
#' values in the converted numerical features.
#' For prediction, a global model (which was fitted on all observations 
#' during training) is used for each combination of factor and target level. 
#' New factor levels are converted to the value of the intercept coefficient
#' of the global models for prediction.
#' NAs are ignored by the CPO.
#' 
#' @section CPOTrained State:
#' The state's \code{$control} slot is a named nested list with named vectors for 
#' each level of the target variable nested within each factorial data column. 
#' Each vector contains the estimates for each factor level from the global model 
#' fitted with all observations during training to the combination of factorial 
#' feature and level of the target variable. 
#'
#' @param n.folds [\code{integer(1)}]\cr
#'   Number of folds used in the cross-validation scheme during training.
#'   With \code{n.folds = 1} the single global model for each combination of
#'   factorial feature and level of the target variable
#'   (which is used during predictions) is also used during training. 
#'   Default is \code{5L}.
#' @param fast.optim [\code{logical}]\cr
#'   If \dQuote{fast.optim} is \code{TRUE} (default), a faster (up to 50 percent)
#'   optimizer from the nloptr package is used when fitting the glmer models. 
#'   This uses additional stopping criteria which can give suboptimal results.   
#' @template cpo_doc_outro
#' @export
cpoLmerEncodeMultiClassif = makeCPOExtendedTrafo("lmer.encode.multi.classif",
  pSS(n.folds = 5: integer [1, ], fast.optim = TRUE: logical),
  dataformat = "factor",
  properties.data = c("missings", "numerics", "factors", "ordered"),
  properties.adding = c("factors", "ordered"),
  properties.needed = c("numerics", "missings.sometimes"),
  properties.target = c("multiclass", "classif"),
  packages = "lme4",
  fix.factors = FALSE,
  cpo.trafo = function(data, target, n.folds, fast.optim) {

    # heavily influenced by the embed package
    fitGlmer = function(feature, target) {
      # performance tips from https://cran.r-project.org/web/packages/lme4/vignettes/lmerperf.html
      nlopt <- function(par, fn, lower, upper, control) {
      .nloptr <<- res <- nloptr::nloptr(par, fn, lb = lower, ub = upper, 
        opts = list(algorithm = "NLOPT_LN_BOBYQA", print_level = 1,
        maxeval = 1000, xtol_abs = 1e-6, ftol_abs = 1e-6))
      list(par = res$solution,
        fval = res$objective,
        conv = if (res$status > 0) 0 else res$status,
        message = res$message)
      }
      args = list(formula = y ~ 1 + (1 | lvl),
        data = data.frame(lvl = feature, y = target),
        family = stats::binomial,
        na.action = na.omit,
        control = if (fast.optim) {
          lme4::glmerControl(optimizer = "nloptwrap", calc.derivs = FALSE)
        } else {
          lme4::glmerControl()
        })
      mod = do.call(lme4::glmer, args)
      coefs = coef(mod)$lvl
      lvls = rownames(coefs)
      coefs = coefs[,1]
      names(coefs) = lvls
      intercept = unname(lme4::fixef(mod))
      # replace missing coefs with intercept value
      coefs[is.na(coefs)] = intercept
      # save intercept value for new levels during prediction
      coefs = c(coefs, ..new..level.. = intercept)
      coefs
    }
    
    # create list with binary "one vs. rest" target variables
    bin_targets = sapply(levels(target[[1]]), function(x) factor(x == target[[1]]),
      simplify = FALSE)
    
    # for prediction, use complete encoding model
    control = sapply(colnames(data), function(cname) {
      sapply(levels(target[[1]]), function(lvl) {
        fitGlmer(data[[cname]], bin_targets[[lvl]])
        }, simplify = FALSE)
      }, simplify = FALSE)
    
    # if n.folds == 1 use only the complete model in training
    if (n.folds == 1) {
      num_vals_list = sapply(colnames(data), function(cname) {
        sapply(levels(target[[1]]), function(lvl) {
          as.numeric(control[[cname]][[lvl]][as.character(data[[cname]])])
        }, simplify = FALSE)
      }, simplify = FALSE)
      # return df with new feature names: feature_name.target_level
      # NOTE: special symbols in target levels (e.g. "-") are transformed to "." 
      # by as.data.frame to allign with nameing rules for data.frame columns
      return(as.data.frame(num_vals_list, row.names = rownames(data)))
    }

    # else, use n.folds encoding models in crossvalidation scheme in training
    rinst = makeResampleInstance(makeResampleDesc("CV", iters = n.folds), 
      size = nrow(data))
    # list with n.folds models for each target level for each data column
    mod_list = sapply(colnames(data), function(cname) {
      sapply(levels(target[[1]]), function(lvl) {
        lapply(rinst$train.inds, function(inds) {
          fitGlmer(data[[cname]][inds], bin_targets[[lvl]][inds])
          })
        }, simplify = FALSE)
      }, simplify = FALSE)
    # list with replaced values in n.folds for each target level for each data column
    num_vals_list = sapply(colnames(data), function(cname) {
      sapply(levels(target[[1]]), function(lvl) {
        lapply(seq_len(n.folds), function(fold) {
        # add new level for all values not present during training
          fac = flagNewLvls(data[[cname]][rinst$test.inds[[fold]]],
            names(mod_list[[cname]][[lvl]][[fold]]))
          mod = mod_list[[cname]][[lvl]][[fold]]
        
          as.numeric(mod[as.character(fac)])
          })
        }, simplify = FALSE)
      }, simplify = FALSE)
    # recombine replaced values from n.folds for each data column
    num_vals_list = sapply(colnames(data), function(cname) {
      sapply(levels(target[[1]]), function(lvl) {
        unlist(num_vals_list[[cname]][[lvl]])[order(unlist(rinst$test.inds))]
        }, simplify = FALSE)
      }, simplify = FALSE)
    # return df with new feature names: feature_name.target_level
    # NOTE: special symbols in target levels (e.g. "-") are transformed to "." 
    # by as.data.frame to allign with nameing rules for data.frame columns
    as.data.frame(num_vals_list, row.names = rownames(data))
  },
  cpo.retrafo = function(data, control, n.folds, fast.optim) {
    target_lvls = names(control[[1]])
    # use complete encoding model for new prediction data
    num_vals_list = sapply(colnames(data), function(cname) {
      # add new level for all values not present during training
      sapply(target_lvls, function(lvl) {
        fac = flagNewLvls(data[[cname]], names(control[[cname]][[lvl]]))
     
        as.numeric(control[[cname]][[lvl]][as.character(fac)])
        }, simplify = FALSE)
      }, simplify = FALSE)
    # return df with new feature names: feature_name.target_level
    # NOTE: special symbols in target levels (e.g. "-") are transformed to "." 
    # by as.data.frame to allign with nameing rules for data.frame columns
    as.data.frame(num_vals_list, row.names = rownames(data))
  })

#' @title Impact Encoding with Random Forests
#'
#' @template cpo_doc_intro
#'
#' @description
#' cpoRangerEncodeRegr() converts factor levels of each factorial column to the
#' predictions of a random forest, in which the numeric target variable 
#' is used as dependent variable and the factor is used as the only predictor.
#' For each factorial column, a random forest is fitted with the ranger function 
#' of the ranger package, using \code{respect.unordered.factors = "order"} to 
#' handle the factorial feature and \code{replace = FALSE} to prevent bootstrap 
#' samples which include observations with rare factor levels multiple times.
#' For training, the prediction for each observation is only computed based on
#' the "out-of-bag" trees to ensure that the same factor level does not always 
#' result in identical values in the converted numerical feature.
#' For prediction, the complete forest of the respective factor is used. 
#' For new factor levels, the mean prediction accross all observed levels is used
#' in each tree. Missing values are ignored by the CPO.
#'  
#' @section CPOTrained State:
#' The state's \code{$control} slot is a named list containing a prediction matrix 
#' for each factorial data column. The matrices contain the response prediction 
#' of each tree in the forest (unnamed columns) for all unique levels of the 
#' respective factor observed during training (named rows).
#'
#' @param num.trees [\code{integer(1)}]\cr
#'   Number of trees, used by ranger.
#' @param min.node.size [\code{integer(1)}]\cr
#'   Minimal node size, used by ranger.   
#' @template cpo_doc_outro
#' @export
cpoRangerEncodeRegr = makeCPOExtendedTrafo("ranger.encode.regr",
  pSS(num.trees = 10: integer [5, ], min.node.size = 10: integer [1, ]),
  dataformat = "factor",
  properties.data = c("missings", "numerics", "factors", "ordered"),
  properties.adding = c("factors", "ordered"),
  properties.needed = c("numerics", "missings.sometimes"),
  properties.target = "regr",
  packages = "ranger",
  fix.factors = FALSE,
  cpo.trafo = function(data, target, num.trees, min.node.size) {
    rf = sapply(colnames(data), function(cname) {
      na_ind = is.na(data[[cname]])
      ranger::ranger(dependent.variable.name = "y",
        # remove NAs for ranger fitting
        data = data.frame(x = data[[cname]][!na_ind], y = target[[1]][!na_ind]),
        num.trees = num.trees, mtry = 1, min.node.size = min.node.size, 
        replace = FALSE, respect.unordered.factors = "order", keep.inbag = TRUE, 
        num.threads = 1, save.memory = TRUE, verbose = FALSE)
    }, simplify = FALSE)
    control = sapply(colnames(data), function(cname) {
      pred_mat = predict(rf[[cname]], 
        # to save time predict only unique levels and remove NAs
        data = data.frame(x = unique(na.omit(data[[cname]]))), 
        predict.all = TRUE, type = "response", num.threads = 1, 
        save.memory = TRUE)$predictions
      rownames(pred_mat) = as.character(unique(na.omit(data[[cname]])))
      # for new levels compute the mean across all levels per tree
      new_lvl_pred = apply(pred_mat, 2, mean)
      dim(new_lvl_pred) = c(1, length(new_lvl_pred))
      rownames(new_lvl_pred) = "..new..level.."
      # add new level prediction
      pred_mat = rbind(pred_mat, new_lvl_pred)
      pred_mat
      }, simplify = FALSE)
    data[] = lapply(colnames(data), function(cname) {
      pred_mat = control[[cname]]
      # enlarge matrix to include predictions for complete data
      # does not work with NAs...
      na_ind = is.na(data[[cname]])
      pred_mat = pred_mat[as.character(data[[cname]][!na_ind]), , drop = FALSE]
      # for training data, compute mean oob predictions
      pred_mat[do.call(cbind, rf[[cname]]$inbag.counts) == 1] = NA
      preds = rowMeans(pred_mat, na.rm = TRUE)
      # use complete forest for observations which are oob in all trees
      if (anyNA(preds)) {
        pred_mat = control[[cname]]
        pred_mat = pred_mat[as.character(data[[cname]][!na_ind]), , drop = FALSE]
        preds[is.nan(preds)] = rowMeans(pred_mat[is.nan(preds), , drop = FALSE], 
          na.rm = TRUE)
      }
      # readd Nas
      preds_na = rep(NA, length(data[[cname]]))
      preds_na[!na_ind] = preds
      preds_na
      })
    data
  },
  cpo.retrafo = function(data, control, num.trees, min.node.size) {
    data[] = lapply(colnames(data), function(cname) {
      # rename new levels
      data[[cname]] = flagNewLvls(data[[cname]], rownames(control[[cname]]))
      na_ind = is.na(data[[cname]])
      pred_mat = control[[cname]]
      pred_mat = pred_mat[as.character(data[[cname]][!na_ind]), , drop = FALSE]
      # use complete forest
      preds = rowMeans(pred_mat, na.rm = TRUE)
      # readd Nas
      preds_na = rep(NA, length(data[[cname]]))
      preds_na[!na_ind] = preds
      preds_na
    })
    data
  })

#' @title Impact Encoding with Random Forests
#'
#' @template cpo_doc_intro
#'
#' @description
#' cpoRangerEncodeTwoClassif() converts factor levels of each factorial column
#' to the predictions of a random forest, in which the binary target variable 
#' is used as dependent variable and the factor is used as the only predictor.
#' For each factorial column, a random probability forest is fitted with the 
#' ranger function of the ranger package (\code{probability = TRUE}), using  
#' \code{respect.unordered.factors = "order"} to handle the factorial feature 
#' and \code{replace = FALSE} to prevent bootstrap samples which include
#' observations with rare factor levels multiple times.
#' For training, the prediction for each observation is only computed based on
#' the "out-of-bag" trees to ensure that the same factor level does not always 
#' result in identical values in the converted numerical feature.
#' For prediction, the complete forest of the respective factor is used. 
#' For new factor levels, the mean prediction accross all observed levels is used
#' in each tree. Missing values are ignored by the CPO.
#' 
#' @section CPOTrained State:
#' The state's \code{$control} slot is a named list containing a prediction matrix 
#' for each factorial data column. The matrices contain the probability prediction 
#' of each tree in the forest (unnamed columns) for all unique levels of the 
#' respective factor observed during training (named rows).
#'
#' @param num.trees [\code{integer(1)}]\cr
#'   Number of trees used by ranger.
#' @param min.node.size [\code{integer(1)}]\cr
#'   Minimal node size used by ranger.   
#' @template cpo_doc_outro
#' @export
cpoRangerEncodeTwoClassif = makeCPOExtendedTrafo("ranger.encode.two.classif",
  pSS(num.trees = 10: integer [5, ], min.node.size = 10: integer [1, ]),
  dataformat = "factor",
  properties.data = c("missings", "numerics", "factors", "ordered"),
  properties.adding = c("factors", "ordered"),
  properties.needed = c("numerics", "missings.sometimes"),
  properties.target = c("twoclass", "classif"),
  packages = "ranger",
  fix.factors = FALSE,
  cpo.trafo = function(data, target, num.trees, min.node.size) {
    rf = sapply(colnames(data), function(cname) {
      na_ind = is.na(data[[cname]])
      ranger::ranger(dependent.variable.name = "y",
        # remove NAs for ranger fitting
        data = data.frame(x = data[[cname]][!na_ind], y = target[[1]][!na_ind]),
        probability = TRUE, num.trees = num.trees, mtry = 1, min.node.size = min.node.size, 
        replace = FALSE, respect.unordered.factors = "order", keep.inbag = TRUE, 
        num.threads = 1, save.memory = TRUE, verbose = FALSE)
    }, simplify = FALSE)
    control = sapply(colnames(data), function(cname) {
      pred_mat = predict(rf[[cname]], 
        # to save time predict only unique levels and remove NAs
        data = data.frame(x = unique(na.omit(data[[cname]]))), 
        predict.all = TRUE, type = "response", num.threads = 1, 
        save.memory = TRUE)$predictions
      # in ranger, class columns are based on "first appearance" order in the target
      na_ind = is.na(data[[cname]])
      colnames(pred_mat) = as.character(unique(target[[1]][!na_ind]))
      # convert array to matrix by keeping only probs for the first target level
      # NOTE: this is not necessarily the positive class defined in the task!
      pred_mat = pred_mat[, levels(target[[1]])[1], , drop = TRUE]
      rownames(pred_mat) = as.character(unique(na.omit(data[[cname]])))
      # for new levels compute the mean across all levels per tree
      new_lvl_pred = apply(pred_mat, 2, mean)
      dim(new_lvl_pred) = c(1, length(new_lvl_pred))
      rownames(new_lvl_pred) = "..new..level.."
      # add new level prediction
      pred_mat = rbind(pred_mat, new_lvl_pred)
      pred_mat
      }, simplify = FALSE)
    data[] = lapply(colnames(data), function(cname) {
      pred_mat = control[[cname]]
      # enlarge matrix to include predictions for complete data
      # does not work with NAs...
      na_ind = is.na(data[[cname]])
      pred_mat = pred_mat[as.character(data[[cname]][!na_ind]), , drop = FALSE]
      # for training data, compute mean oob predictions
      pred_mat[do.call(cbind, rf[[cname]]$inbag.counts) == 1] = NA
      preds = rowMeans(pred_mat, na.rm = TRUE)
      # use complete forest for observations which are oob in all trees
      if (anyNA(preds)) {
        pred_mat = control[[cname]]
        pred_mat = pred_mat[as.character(data[[cname]][!na_ind]), , drop = FALSE]
        preds[is.nan(preds)] = rowMeans(pred_mat[is.nan(preds), , drop = FALSE], 
          na.rm = TRUE)
      }
      # readd Nas
      preds_na = rep(NA, length(data[[cname]]))
      preds_na[!na_ind] = preds
      preds_na
      })
    data
  },
  cpo.retrafo = function(data, control, num.trees, min.node.size) {
    data[] = lapply(colnames(data), function(cname) {
      # rename new levels
      data[[cname]] = flagNewLvls(data[[cname]], rownames(control[[cname]]))
      na_ind = is.na(data[[cname]])
      pred_mat = control[[cname]]
      pred_mat = pred_mat[as.character(data[[cname]][!na_ind]), , drop = FALSE]
      # use complete forest
      preds = rowMeans(pred_mat, na.rm = TRUE)
      # readd Nas
      preds_na = rep(NA, length(data[[cname]]))
      preds_na[!na_ind] = preds
      preds_na
    })
    data
  })

#' @title Impact Encoding with Random Forests
#'
#' @template cpo_doc_intro
#'
#' @description
#' cpoRangerEncodeMultiClassif() converts factor levels of each factorial column
#' to the predictions of a random forest, in which the multiclass target variable 
#' is used as dependent variable and the factor is used as the only predictor.
#' For each factorial column, a random probability forest is fitted with the 
#' ranger function of the ranger package (\code{probability = TRUE}), using  
#' \code{respect.unordered.factors = "order"} to handle the factorial feature 
#' and \code{replace = FALSE} to prevent bootstrap samples which include
#' observations with rare factor levels multiple times.
#' For training, the prediction for each observation is only computed based on
#' the "out-of-bag" trees to ensure that the same factor level does not always 
#' result in identical values in the converted numerical feature.
#' For prediction, the complete forest of the respective factor is used. 
#' For new factor levels, the mean prediction accross all observed levels is used
#' in each tree. Missing values are ignored by the CPO.
#' 
#' @section CPOTrained State:
#' The state's \code{$control} slot is a named list containing a three dimensional
#' prediction array for each factorial data column. The arrays contain the 
#' probability prediction for each level of the target variable (named columns), 
#' of all unique levels of the respective factor observed during training 
#' (named rows), for each tree in the forest (unnamed third dimension).
#'
#' @param num.trees [\code{integer(1)}]\cr
#'   Number of trees used by ranger.
#' @param min.node.size [\code{integer(1)}]\cr
#'   Minimal node size used by ranger.   
#' @template cpo_doc_outro
#' @export
cpoRangerEncodeMultiClassif = makeCPOExtendedTrafo("ranger.encode.multi.classif",
  pSS(num.trees = 10: integer [5, ], # NOTE: code would break for num.trees = 1
    min.node.size = 10: integer [1, ]), 
  dataformat = "factor",
  properties.data = c("missings", "numerics", "factors", "ordered"),
  properties.adding = c("factors", "ordered"),
  properties.needed = c("numerics", "missings.sometimes"),
  properties.target = c("multiclass", "classif"),
  packages = c("ranger", "abind"),
  fix.factors = FALSE,
  cpo.trafo = function(data, target, num.trees, min.node.size) {
    rf = sapply(colnames(data), function(cname) {
      na_ind = is.na(data[[cname]])
      ranger::ranger(dependent.variable.name = "y",
        # remove NAs for ranger fitting
        data = data.frame(x = data[[cname]][!na_ind], y = target[[1]][!na_ind]),
        probability = TRUE, num.trees = num.trees, mtry = 1, min.node.size = min.node.size, 
        replace = FALSE, respect.unordered.factors = "order", keep.inbag = TRUE, 
        num.threads = 1, save.memory = TRUE, verbose = TRUE)
    }, simplify = FALSE)
    control = sapply(colnames(data), function(cname) {
      pred_arr = predict(rf[[cname]], 
        # to save time predict only unique levels and remove NAs
        data = data.frame(x = unique(na.omit(data[[cname]]))), 
        predict.all = TRUE, type = "response", num.threads = 1, 
        save.memory = TRUE)$predictions
      rownames(pred_arr) = as.character(unique(na.omit(data[[cname]])))
      # in ranger, class columns are based on "first appearance" order in the target
      na_ind = is.na(data[[cname]])
      colnames(pred_arr) = as.character(unique(target[[1]][!na_ind]))
      # reorder columns according to order of target levels
      pred_arr = pred_arr[, levels(target[[1]]), , drop = FALSE]
      # for new levels compute the mean across all levels per tree
      new_lvl_pred = apply(pred_arr, c(2,3), mean)
      dim(new_lvl_pred) = c(1, dim(new_lvl_pred))
      rownames(new_lvl_pred) = "..new..level.."
      # add new level prediction
      pred_arr = abind::abind(pred_arr, new_lvl_pred, along = 1)
      pred_arr
      }, simplify = FALSE)
    num_vals_list = sapply(colnames(data), function(cname) {
      na_ind = is.na(data[[cname]])
      sapply(levels(target[[1]]), function(lvl) {
        # convert array to matrix by keeping only probs for respective class
        pred_mat = control[[cname]]
        pred_mat = pred_mat[, lvl, , drop = TRUE]
        # enlarge matrix to include predictions for complete data
        # does not work with NAs...
        pred_mat = pred_mat[as.character(data[[cname]][!na_ind]), , drop = FALSE]
        # for training data, compute mean oob predictions
        pred_mat[do.call(cbind, rf[[cname]]$inbag.counts) == 1] = NA
        preds = rowMeans(pred_mat, na.rm = TRUE)
        # use complete forest for observations which are oob in all trees
        if (anyNA(preds)) {
          pred_mat = control[[cname]]
          pred_mat = pred_mat[, lvl, , drop = TRUE]
          pred_mat = pred_mat[as.character(data[[cname]][!na_ind]), , drop = FALSE]
          preds[is.nan(preds)] = rowMeans(pred_mat[is.nan(preds), , drop = FALSE], 
            na.rm = TRUE)
        }
        # readd Nas
        preds_na = rep(NA, length(data[[cname]]))
        preds_na[!na_ind] = preds
        preds_na
        }, simplify = FALSE)
      }, simplify = FALSE)
    # return df with new feature names: feature_name.target_level
    # NOTE: special symbols in target levels (e.g. "-") are transformed to "." 
    # by as.data.frame to allign with naming rules for data.frame columns
    as.data.frame(num_vals_list, row.names = rownames(data))
  },
  cpo.retrafo = function(data, control, num.trees, min.node.size) {
    target_lvls = colnames(control[[1]])
    num_vals_list = sapply(colnames(data), function(cname) {
      # rename new levels
      data[[cname]] = flagNewLvls(data[[cname]], rownames(control[[cname]]))
      na_ind = is.na(data[[cname]])
      sapply(target_lvls, function(lvl) {
        # convert array to matrix by keeping only probs for respective class
        pred_mat = control[[cname]]
        pred_mat = pred_mat[, lvl, , drop = TRUE]
        # enlarge matrix to include predictions for complete data
        # does not work with NAs...
        pred_mat = pred_mat[as.character(data[[cname]][!na_ind]), , drop = FALSE]
        # use complete forest
        preds = rowMeans(pred_mat, na.rm = TRUE)
        # readd Nas
        preds_na = rep(NA, length(data[[cname]]))
        preds_na[!na_ind] = preds
        preds_na
        }, simplify = FALSE)
      }, simplify = FALSE)
    # return df with new feature names: feature_name.target_level
    # NOTE: special symbols in target levels (e.g. "-") are transformed to "." 
    # by as.data.frame to allign with naming rules for data.frame columns
    as.data.frame(num_vals_list, row.names = rownames(data))
  })

#' @title Multivariate Impact Encoding with Random Forests
#'
#' @template cpo_doc_intro
#'
#' @description
#' cpoRangerMultivEncodeRegr() converts factor levels of all factorial columns to the
#' predictions of a random forest, in which the numeric target variable 
#' is used as dependent variable and the factors are used as predictors.
#' A random forest is fitted with the ranger function of the ranger package, 
#' using \code{respect.unordered.factors = "order"} to handle the factorial features 
#' and \code{replace = FALSE} to prevent bootstrap samples which include observations 
#' with rare factor levels multiple times.
#' For training, the prediction for each observation is only computed based on
#' the "out-of-bag" trees to ensure that the same factor level does not always 
#' result in identical values in the converted numerical feature.
#' For prediction, the complete forest of the respective factor is used. 
#' Missing values are ignored by the CPO.
#'  
#' @section CPOTrained State:
#' The state's \code{$control} slot contains a fitted ranger object.
#'
#' @param num.trees [\code{integer(1)}]\cr
#'   Number of trees, used by ranger.
#' @param min.node.size [\code{integer(1)}]\cr
#'   Minimal node size, used by ranger.   
#' @template cpo_doc_outro
#' @export
cpoRangerMultivEncodeRegr = makeCPOExtendedTrafo("ranger.encode.multiv.regr",
  pSS(num.trees = 10: integer [5, ], min.node.size = 10: integer [1, ]),
  dataformat = "factor",
  properties.data = c("numerics", "factors", "ordered"),
  properties.adding = c("factors", "ordered"),
  properties.needed = c("numerics", "missings.sometimes"),
  properties.target = "regr",
  packages = "ranger",
  fix.factors = FALSE,
  cpo.trafo = function(data, target, num.trees, min.node.size) {
    
     control = ranger::ranger(dependent.variable.name = "..target..", 
      data = data.frame(data, ..target.. = target[[1]]), 
      num.trees = num.trees, min.node.size = min.node.size, replace = FALSE, 
      respect.unordered.factors = "order", keep.inbag = TRUE, num.threads = 1, 
      save.memory = TRUE, verbose = FALSE)
    
    preds = predict(control, 
      data = data, predict.all = TRUE, 
      type = "response", num.threads = 1)$predictions
    inbag = do.call(cbind, control$inbag.counts) >= 1
    preds[inbag] = NA
    # compute mean oob predictions
    preds = rowMeans(preds, na.rm = TRUE)
    # use complete forest for observations which are oob in all trees
    if (anyNA(preds)) {
      preds[is.nan(preds)] = predict(control, data = data[is.nan(preds), , drop = FALSE], 
        predict.all = FALSE, type = "response", num.threads = 1)$predictions
    }
    data.frame(..encoded.. = preds)

  },
  cpo.retrafo = function(data, control, num.trees, min.node.size) {
    preds = predict(control, data = data, predict.all = FALSE, type = "response", 
      num.threads = 1, save.memory = TRUE)$predictions
    data.frame(..encoded.. = preds)
  })

#' @title Multivariate Impact Encoding with Random Forests
#'
#' @template cpo_doc_intro
#'
#' @description
#' cpoRangerMultivEncodeTwoClassif() converts factor levels of all factorial columns
#' to the predictions of a random forest, in which the binary target variable 
#' is used as dependent variable and all factors are used as predictors.
#' A random probability forest is fitted with the ranger function of the ranger 
#' package (\code{probability = TRUE}), using \code{respect.unordered.factors = "order"} 
#' to handle the factorial feature and \code{replace = FALSE} to prevent bootstrap 
#' samples which include observations with rare factor levels multiple times.
#' For training, the prediction for each observation is only computed based on
#' the "out-of-bag" trees to ensure that the same factor level does not always 
#' result in identical values in the converted numerical feature.
#' For prediction, the complete forest of the respective factor is used. 
#' Missing values are ignored by the CPO.
#'  
#' @section CPOTrained State:
#' The state's \code{$control} slot contains a fitted ranger object.
#'
#' @param num.trees [\code{integer(1)}]\cr
#'   Number of trees, used by ranger.
#' @param min.node.size [\code{integer(1)}]\cr
#'   Minimal node size, used by ranger.   
#' @template cpo_doc_outro
#' @export
cpoRangerMultivEncodeTwoClassif = makeCPOExtendedTrafo("ranger.encode.multiv.two.classif",
  pSS(num.trees = 10: integer [5, ], min.node.size = 10: integer [1, ]),
  dataformat = "factor",
  properties.data = c("numerics", "factors", "ordered"),
  properties.adding = c("factors", "ordered"),
  properties.needed = c("numerics", "missings.sometimes"),
  properties.target = c("twoclass", "classif"),
  packages = "ranger",
  fix.factors = FALSE,
  cpo.trafo = function(data, target, num.trees, min.node.size) {
    
    control = ranger::ranger(dependent.variable.name = "..target..", 
      data = data.frame(data, ..target.. = target[[1]]), probability = TRUE,
      num.trees = num.trees, min.node.size = min.node.size, replace = FALSE, 
      respect.unordered.factors = "order", keep.inbag = TRUE, num.threads = 1, 
      save.memory = TRUE, verbose = FALSE)
    
    preds = predict(control, data = data, predict.all = TRUE, 
      type = "response", num.threads = 1)$predictions
    # convert array to matrix by keeping only probs for the first target level
    # NOTE: this is not necessarily the positive class defined in the task!
    preds = preds[, 1, , drop = TRUE]
    inbag = do.call(cbind, control$inbag.counts) >= 1
    preds[inbag] = NA
    # compute mean oob predictions
    preds = rowMeans(preds, na.rm = TRUE)
    # use complete forest for observations which are oob in all trees
    if (anyNA(preds)) {
      preds[is.nan(preds)] = predict(control, data = data[is.nan(preds), , drop = FALSE], 
        predict.all = FALSE, type = "response", num.threads = 1)$predictions[,1]
    }
    data.frame(..encoded.. = preds)
  },
  cpo.retrafo = function(data, control, num.trees, min.node.size) {
    preds = predict(control, data = data, predict.all = FALSE, type = "response", 
      num.threads = 1, save.memory = TRUE)$predictions[,1]
    data.frame(..encoded.. = preds)
  })

#' @title Multivariate Impact Encoding with Random Forests
#'
#' @template cpo_doc_intro
#'
#' @description
#' cpoRangerMultivEncodeMultiClassif() converts factor levels of all factorial columns
#' to the predictions of a random forest, in which the multiclass target variable 
#' is used as dependent variable and the factors are used as predictors.
#' A random probability forest is fitted with the 
#' ranger function of the ranger package (\code{probability = TRUE}), using  
#' \code{respect.unordered.factors = "order"} to handle the factorial features 
#' and \code{replace = FALSE} to prevent bootstrap samples which include
#' observations with rare factor levels multiple times.
#' For training, the prediction for each observation is only computed based on
#' the "out-of-bag" trees to ensure that the same factor level does not always 
#' result in identical values in the converted numerical feature.
#' For prediction, the complete forest is used. 
#' Missing values are ignored by the CPO.
#'  
#' @section CPOTrained State:
#' The state's \code{$control} slot contains a fitted ranger object.
#'
#' @param num.trees [\code{integer(1)}]\cr
#'   Number of trees, used by ranger.
#' @param min.node.size [\code{integer(1)}]\cr
#'   Minimal node size, used by ranger.   
#' @template cpo_doc_outro
#' @export
cpoRangerMultivEncodeMultiClassif = makeCPOExtendedTrafo("ranger.encode.multiv.multi.classif",
  pSS(num.trees = 10: integer [5, ], min.node.size = 10: integer [1, ]),
  dataformat = "factor",
  properties.data = c("numerics", "factors", "ordered"),
  properties.adding = c("factors", "ordered"),
  properties.needed = c("numerics", "missings.sometimes"),
  properties.target = c("multiclass", "classif"),
  packages = "ranger",
  fix.factors = FALSE,
  cpo.trafo = function(data, target, num.trees, min.node.size) {
    
    control = ranger::ranger(dependent.variable.name = "..target..", 
      data = data.frame(data, ..target.. = target[[1]]), probability = TRUE,
      num.trees = num.trees, min.node.size = min.node.size, replace = FALSE, 
      respect.unordered.factors = "order", keep.inbag = TRUE, num.threads = 1, 
      save.memory = TRUE, verbose = FALSE)
    
    pred_arr = predict(control, data = data, predict.all = TRUE, 
      type = "response", num.threads = 1)$predictions
    
    colnames(pred_arr) = as.character(unique(target[[1]]))
    # reorder columns according to order of target levels
    pred_arr = pred_arr[, levels(target[[1]]), , drop = FALSE]
    
    pred_list = sapply(levels(target[[1]]), function(lvl) {
      # convert array to matrix by keeping only probs for respective class
      pred_mat = pred_arr[, lvl, , drop = TRUE]
      
      pred_mat[do.call(cbind, control$inbag.counts) >= 1] = NA
      preds = rowMeans(pred_mat, na.rm = TRUE)
      # use complete forest for observations which are oob in all trees
      if (anyNA(preds)) {
        pred_mat = pred_arr[, lvl, , drop = TRUE]
        preds[is.nan(preds)] = rowMeans(pred_mat[is.nan(preds), , drop = FALSE], 
          na.rm = TRUE)
        }
      preds
      }, simplify = FALSE)
    
    as.data.frame(pred_list, row.names = rownames(data), optional = TRUE) # optional prevents substituting whitespace with . in column names
    
  },
  cpo.retrafo = function(data, control, num.trees, min.node.size) {
    preds = predict(control, data = data, predict.all = FALSE, type = "response", 
      num.threads = 1, save.memory = TRUE)$predictions
    as.data.frame(preds, row.names = rownames(data))
  })

#' @title Leaf Encoding with CART
#'
#' @template cpo_doc_intro
#'
#' @description
#' cpoLeafEncode() converts factor levels of each factorial column to a new 
#' factor with possibly fewer levels that correspond to the terminal nodes of
#' a CART decision tree. Trees in which the target variable is used as dependent
#' variable and the factor is used as the only predictor are grown with the 
#' rpart function of the rpart package.
#' To speed up the computation for multiclass targets, pca ordering is used to 
#' convert factorial columns to integers before growing trees (adapted from the 
#' ranger package, see https://github.com/imbs-hl/ranger/blob/master/R/utility.R). 
#' For each factorial column, the respective tree are by default pruned based
#' on the value of the complexity parameter that corresponds to the minimal 
#' prediction error (estimated by ten-fold cross-validation). 
#' Missing values are sent along the tree's majority direction, thereby effectively
#' imputing NAs. During prediction, new factor levels are handled the same way.
#' Note that this CPO always returns factorial features which can be converted to 
#' numerical features by another CPO like cpoDummyEncode(). If the force.stump
#' option is not used when pruning the trees, the CPO sometimes returns constant
#' factorial features which can be removed by cpoDropConstants(). 
#'   
#' @section CPOTrained State:
#' The state's \code{$control} slot is a named list of named vectors for each
#' factorial data column. Each vector contains the estimates for each factor level
#' from the global model fitted to that factor with all observations during training. 
#'
#' @param prune [\code{logical}]\cr
#'   If \dQuote{prune} is \code{TRUE} (default), rpart::prune is used to prune
#'   each tree based on the value of the complexity parameter that corresponds
#'   to the minimal prediction error (estimated by ten-fold cross-validation).
#' @param force.stump [\code{logical}]\cr
#'   If \dQuote{force.stump} is \code{TRUE} and \dQuote{prune} is 
#'   \code{TRUE}, at least one split is performed to avoid constant features. 
#' @param minsplit [\code{integer(1)}]\cr
#'   The minimum number of observations that must exist in a node in order for 
#'   a split to be attempted, used by rpart.
#' @param minbucket [\code{integer(1)}]\cr
#'   The minimum number of observations in any terminal node, used by rpart. 
#' @template cpo_doc_outro
#' @export
cpoLeafEncode = makeCPOExtendedTrafo("leaf.encode",
  pSS(prune = TRUE: logical, 
    force.stump = FALSE: logical [[requires = expression(prune == TRUE)]], 
    minsplit = 20: integer [2, ], minbucket = 10: integer [1, ]),
  dataformat = "factor",
  properties.data = c("missings", "numerics", "factors", "ordered"),
  properties.needed = c("factors", "missings.sometimes"),
  packages = "rpart",
  fix.factors = TRUE,
  cpo.trafo = function(data, target, minsplit, minbucket, prune, force.stump) {
    control = sapply(colnames(data), function(cname) {
      # for multiclass, speedup computation by using pca ordering to create integers
      if (is.factor(target[[1]]) & nlevels(target[[1]]) > 2) {
        lvls.ordered = pca.order(target[[1]], data[[cname]])
        data[[cname]] = as.integer(factor(data[[cname]], levels = lvls.ordered, 
          ordered = TRUE))
      }
      # for NAs send observation in the majority direction (no surrogates)
      tree = rpart::rpart(y ~ x, data = data.frame(x = data[[cname]], 
        y = target[[1]]), control = rpart::rpart.control(minsplit = minsplit, 
          minbucket = minbucket, cp = 0, xval = 10, maxdepth = 30, 
          maxcompete = 0, maxsurrogate = 0, usesurrogate = 2))
      # complexity pruning (10-cv)
      if (prune) {
        cp = tree$cptable[max(which.min(tree$cptable[, "xerror"]), 
          # if CV would choose the first row (no splits), use the second (stump)
          ifelse(force.stump, 2, 1)), "CP"]
        tree = rpart::prune(tree, cp = cp)
      }
      if (is.factor(target[[1]]) & nlevels(target[[1]]) > 2) {
        return(list(tree = tree, lvls.ordered = lvls.ordered))
      }
      list(tree = tree, lvls.ordered = NULL)
    }, simplify = FALSE)
    
    data[] = lapply(colnames(data), function(cname) {
      if (!is.null(control[[cname]][["lvls.ordered"]])) {
        data[[cname]] = as.integer(factor(data[[cname]], 
          levels = control[[cname]][["lvls.ordered"]], ordered = TRUE))
      }
      preds = predict_nodes(control[[cname]][["tree"]], 
        newdata = data.frame(x = data[[cname]]), na.action = na.pass)
      factor(preds)
    })
    data
  },
  cpo.retrafo = function(data, control, minsplit, minbucket, prune, force.stump) {
    data[] = lapply(colnames(data), function(cname) {
      if (!is.null(control[[cname]][["lvls.ordered"]])) {
        data[[cname]] = as.integer(factor(data[[cname]], 
          levels = control[[cname]][["lvls.ordered"]], ordered = TRUE))
      }
      preds = predict_nodes(control[[cname]][["tree"]], 
        newdata = data.frame(x = data[[cname]]), na.action = na.pass)
      factor(preds)
    })
    data
  })

#' @title Cluster Encoding with hclust
#'
#' @template cpo_doc_intro
#'
#' @description
#' cpoClusterEncode() converts each factorial column to a new factor with a
#' prespecified number of levels that result from a hierarchical cluster analysis.
#' For each factor, a cluster analysis is fitted with the function hclust in the
#' fastcluster package. The distance matrix is based on the absolute deviation 
#' between the frequency of the factor level and the mean frequency - to encourage 
#' collapsing rare levels with frequent levels - and the mean response on the 
#' target variable (regression) or the frequency for each target level 
#' (classification) - to encourage collapsing levels with a similar main 
#' effect on the target variable.
#' Different methods can be used for the computation of the distance matrix as
#' well as the agglomeration in the cluster algorithm. The resulting tree from 
#' the cluster analysis is cut based on the prespecified desired number of 
#' levels for the new encoded factor variable. 
#' Note that this CPO always returns factorial features which can be converted to 
#' numerical features by another CPO like cpoDummyEncode(). New factor levels
#' cannot be handled. They are automatically converted to NAs during prediction,
#' which are ignored by the CPO. This introduces NAs for features which did not
#' originally have missing values.
#'   
#' @section CPOTrained State:
#' The state's \code{$control} slot is a named list of named vectors for each
#' factorial data column. Each vector contains the new factor level (value) for 
#' each of the old factor levels (names).
#'
#' @param max.levels [\code{integer(1)}]\cr
#'   The maximum number of levels for the newly encoded factorial columns, used by 
#'   cutree. If a cluster tree has less leafs than max.levels, no pruning is used
#'   (i.e. clustering has no effect).
#' @param distance.method [\code{character(1)}]\cr
#'   The distance measure, used by dist. Default is \dQuote{euclidean}. 
#' @param cluster.method [\code{character(1)}]\cr
#'   The agglomeration method, used by hclust. Default is \dQuote{complete}. 
#' @template cpo_doc_outro
#' @export
cpoClusterEncode = makeCPO("cluster.encode",
  pSS(max.levels = 10: integer [2, ], 
    distance.method = "euclidean": discrete [euclidean, maximum, manhattan, 
      canberra, binary, minkowski],
    cluster.method = "complete": discrete [ward.D, ward.D2, single, complete, 
      average, mcquitty, median, centroid]),
  dataformat = "factor",
  properties.data = c("missings", "numerics", "factors", "ordered"),
  properties.needed = c("factors", "missings.sometimes"),
  packages = c("data.table", "fastcluster"),
  fix.factors = TRUE,
  cpo.train = function(data, target, max.levels, distance.method, cluster.method) {
    cnames = colnames(data)
    data = data.table::data.table(data, target = target[[1]])
    # output a named list with a named vector for mapping the new levels for each feature
    sapply(cnames, function(cname) {
      if (nlevels(target[[1]]) > 0) {
        # compute frequency of each feature level for each target level
        # ignore cases with NA in factor feature
        dat = data[!is.na(data[[cname]]), .N, by = c(cname, "target"), with = TRUE]
        dat = data.table::dcast(dat, as.formula(paste(cname, "target", sep = "~")), 
        value.var = "N")
        # replace NAs with frequency 0
        for (col in levels(target[[1]])) dat[is.na(get(col)), (col) := 0]
        # compute absolute difference from the mean frequency of the factor level
        dat[, freq := rowSums(.SD), by = cname]
        dat[, freq := abs(freq - mean(freq))]
      } else {
        # compute the mean target value for each feature level
        # compute absolute difference from the mean frequency of the factor level
        # ignore cases with NA in factor feature
        dat = data[!is.na(data[[cname]]), .(mean = mean(target), freq = .N), 
          by = c(cname), with = TRUE]
        dat[, freq := abs(freq - mean(freq))]
      }
      # standardize columns except the one containing the feature name (cname)
      dat[, c(setdiff(colnames(dat), cname)) := lapply(.SD, scale, center = TRUE, scale = TRUE), 
        .SDcols = setdiff(colnames(dat), cname)]
      # perform cluster analysis and prune levels
      clust = fastcluster::hclust(dist(x = dat[,-1], method = distance.method), 
        method = cluster.method)
      # if the tree has less leafs than max.levels keep all levels
      max.levels = min(max.levels, nrow(clust$merge) + 1)
      clust = cutree(clust, k = max.levels)
      # add levels as names
      names(clust) = as.character(dat[[cname]])
      clust
    }, simplify = FALSE)
  },
  cpo.retrafo = function(data, control, max.levels, distance.method, cluster.method) {
    data[] = lapply(colnames(data), function(cname) {
      
      factor(unname(control[[cname]][as.character(data[[cname]])]))
    })
    data
  })

#' @title Feature Hashing for Factorial Features
#'
#' @template cpo_doc_intro
#'
#' @description
#' cpoHashEncode() uses feature hashing to convert factor levels of each factorial
#' column to dummy columns created by the function hashed.model.matrix from the 
#' FeatureHashing package.
#' During prediction, new factor levels are automatically encoded with the same
#' hashing function also used in training. Missing values are also encoded by 
#' the hashing function, thereby effectively imputing NAs. 
#' Hashed columns are sometimes constant. Those should be removed by a later
#' call of cpoDropConstants().
#' Note that due to an unsolved bug in the FeatureHashing package 
#' (see https://github.com/wush978/FeatureHashing/issues/81), the CPO sometimes
#' returns numerical values > 1 in the hashed columns. Usually, this should not
#' be a problem as hashed columns cannot be easily interpreted anyway.
#'  
#' @section CPOTrained State:
#' empty
#'
#' @param hash.size [\code{integer(1)}]\cr
#'   The hash size of feature hashing (number of dummy columns per original factor
#'   in the case of no constant hashed columns), used by hashed.model.matrix.
#' @template cpo_doc_outro
#' @export
cpoHashEncode = makeCPO("hash.encode",
  pSS(hash.size = 50: integer [2, ]),
  dataformat = "factor",
  properties.data = c("missings", "numerics", "factors", "ordered"),
  properties.adding = c("factors", "ordered"),
  properties.needed = c("numerics", "missings.sometimes"),
  packages = "FeatureHashing",
  fix.factors = FALSE,
  cpo.train = NULL,
  cpo.retrafo = function(data, hash.size) {
    hashed_feats = lapply(colnames(data), function(cname) {
      # NOTE: hashed.model.matrix sometimes returns numerical values > 1
      # https://github.com/wush978/FeatureHashing/issues/81
      hashed_feat = as.matrix(FeatureHashing::hashed.model.matrix(
        cname, data = data, hash.size = hash.size))
      colnames(hashed_feat) = paste(cname, 1:ncol(hashed_feat), sep = ".hash.")
      hashed_feat
    })
    as.data.frame(hashed_feats, row.names = row.names(data))
  })

#' @title Multivariate Feature Hashing for Factorial Features
#'
#' @template cpo_doc_intro
#'
#' @description
#' cpoMultivHashEncode() uses feature hashing to convert factor levels of all factorial
#' columns to dummy columns created by the function hashed.model.matrix from the 
#' FeatureHashing package.
#' During prediction, new factor levels are automatically encoded with the same
#' hashing function also used in training. Missing values are also encoded by 
#' the hashing function, thereby effectively imputing NAs. 
#' Hashed columns are sometimes constant. Those should be removed by a later
#' call of cpoDropConstants().
#' Note that due to an unsolved bug in the FeatureHashing package 
#' (see https://github.com/wush978/FeatureHashing/issues/81), the CPO sometimes
#' returns numerical values > 1 in the hashed columns. Usually, this should not
#' be a problem as hashed columns cannot be easily interpreted anyway.
#'  
#' @section CPOTrained State:
#' empty
#'
#' @param hash.size [\code{integer(1)}]\cr
#'   The hash size of feature hashing (number of dummy columns per original factor
#'   in the case of no constant hashed columns), used by hashed.model.matrix.
#' @template cpo_doc_outro
#' @export
cpoMultivHashEncode = makeCPO("multiv.hash.encode",
  pSS(hash.size = 50: integer [2, ]),
  dataformat = "factor",
  properties.data = c("missings", "numerics", "factors", "ordered"),
  properties.adding = c("factors", "ordered"),
  properties.needed = c("numerics", "missings.sometimes"),
  packages = "FeatureHashing",
  fix.factors = FALSE,
  cpo.train = NULL,
  cpo.retrafo = function(data, hash.size) {
    # NOTE: hashed.model.matrix sometimes returns numerical values > 1
    # https://github.com/wush978/FeatureHashing/issues/81
    hashed_feats = as.matrix(FeatureHashing::hashed.model.matrix(
        ~., data = data, hash.size = hash.size))
    colnames(hashed_feats) = paste0("..hash", 1:ncol(hashed_feats), sep = "..")
    as.data.frame(hashed_feats, row.names = row.names(data))
  })

# cpo helper functions

flagNewLvls = function(fac, lvls) {
  char = as.character(fac)
  char[!(char %in% lvls | is.na(char))] = "..new..level.."
  factor(char)
}

# order factor levels with PCA approach
# stolen from the ranger package at https://github.com/imbs-hl/ranger/blob/master/R/utility.R
# reference: Coppersmith, D., Hong, S.J. & Hosking, J.R. (1999) Partitioning Nominal Attributes in Decision Trees. Data Min Knowl Discov 3:197. \url{https://doi.org/10.1023/A:1009869804967}.
pca.order = function(y, x) {
  x = droplevels(x)
  if (nlevels(x) < 2) {
    return(as.character(levels(x)))
  }
  # Create contingency table of the nominal outcome with the nominal covariate
  N = table(x, droplevels(y))
  # PCA of weighted covariance matrix of class probabilites
  P = N/rowSums(N)
  S = cov.wt(P, wt = rowSums(N))$cov
  pc1 = prcomp(S, rank. = 1)$rotation
  score = P %*% pc1
  # Return ordered factor levels
  as.character(levels(x)[order(score)])
}

# predict terminal nodes for an rpart object
# stolen from https://stackoverflow.com/questions/29304349/how-to-get-terminal-nodes-for-a-new-observation-from-an-rpart-object
predict_nodes = function (object, newdata, na.action = na.pass) {
  where = if (missing(newdata)) {
    object$where
  } else {
    if (is.null(attr(newdata, "terms"))) {
      Terms = delete.response(object$terms)
      newdata = model.frame(Terms, newdata, na.action = na.action, 
        xlev = attr(object, "xlevels"))
      if (!is.null(cl <- attr(Terms, "dataClasses"))) {
        .checkMFClasses(cl, newdata, TRUE)
      }
    }
    rpart:::pred.rpart(object, rpart:::rpart.matrix(newdata))
  }
  as.integer(row.names(object$frame))[where]
}

