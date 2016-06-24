#' Calculate summary statistics for the representative peer of a given group of
#' observations.
#'
#' This function returns representative characteristics of a benchmark 
#' populations based on a shared characteristics. For example, this returns the
#' average characteristics of peers attending the same schools as youth
#' attending a given youth program, where those youth may attend multiple schools.
#' @param data This is the data frame that will be operated on
#' @param descvars Variables to summarize or ("describe")
#' @param focalcat True/False vector indicating which observations represent the focal subpopulation.
#' @param refcat Categorical designation of what determines the reference population.
#' @param byvars Variables to use to slice different calculations, such as year and organization.
#' @param id (Optional) Unique identifier to use in deduplicating observations.
#' @keywords data.table stat.desc pastecs reshape2
#' @export
#' @examples
#' slicestats()

library("data.table")
library("magrittr")

PeerStats <- function(data, descvars, focalcat, refcat, byvars = NULL, idvar){ # Consider whether idvar is a required argument. Perhaps could create a unique one if not supplied, e.g. based off of 1:nrow(data)
  
  ### Set up by shorthand
  cbyvars <- paste(byvars, collapse = ",")
  bf  <- paste(cbyvars, focalcat,         sep = ",")
  bfr <- paste(cbyvars, focalcat, refcat, sep = ",")
  br  <- paste(cbyvars,           refcat, sep = ",")
  bfr_plus <- paste(c(byvars, focalcat, refcat), collapse = " + ")
  dv_plus  <- paste(descvars, collapse = " + ")
  vBf <- c(unlist(strsplit(byvars, split = ",")), focalcat)
  
  ### Establish data as a properly-indexed data table
  if ("data.table" %in% class(data)){
    dt <- setkeyv(data, cols = c(focalcat, refcat, byvars))
  } else {
    dt <- data.table(data, key = paste(focalcat, refcat, cbyvars, sep = ","))
  }
  
  ### Remove duplicate rows
  bDup <- duplicated(dt, by = NULL) # If not set to NULL, duplicated.data.table looks for duplicates by the key. We want to look across all columns
  data <- dt[!bDup,]
  descvars <- unique(descvars)
  
  ### Calculate n's, first by all by/focal/reference categories, and then by
  # chaining in an aggregation to additionally also calculate by+focal Ns
  # Note that we are keeping the data in wide-in-variables format for two reasons:
  # 1. we do not have to make a potentially very large second copy of the data set,
  # 2. all data value types would be coereced to be the most general, i.e. decimal,
  #    which would significantly increase the size of the data object
  # The tradeoff is that we wind up having slightly complex means of using
  # lapply and variable assignments to accomplish tasks.
  # Also note that the melt() function below comes from the data.table package,
  # rather than the reshape2 package. It is one that runs more efficiently with
  # data.table objects, and also allows simpler handling of melting multiple columns.
  
  wgt_bfrv <- dt[, # Calculate by/foc/ref non-missing counts, which is the numerator for weights
                 lapply(.SD, function(dv) length(dv)), # /!\ Another way to build weights is based on how many of the focal youth have non-missing values of each variable--using sum(!is.na(dv)). However, that would lead to different peer composition across measures, which seems peculiar (and worth avoiding)
                 by = bfr,
                 .SDcols = descvars][,  # Chain in aggregation of by/foc/ref counts across ref groups to get a denominator.
                                     paste0(descvars, "_f") := lapply(.SD, function(n_bfr) as.integer(sum(n_bfr))),
                                     by = bf,
                                     .SDcols = descvars
                                     ] %>%
    melt(id.vars = c(byvars, focalcat, refcat), # This is a special version of melt (coming from the data.table package) allows for the measure.vars argument, which allows us to output multiple column values, so that the result is semi-wide
         measure.vars = list(descvars, paste0(descvars, "_f")),
         value.name = c("n_bfrv", "n_bfv"),
         variable.name = "descvar") %>%
    within({
      descvar <- descvars[descvar] # By default, melt() creates the "variable" field to have values 1, 2, etc. This line uses those numbers to select the names of the descvars
      wgt_bfrv  <- n_bfrv / n_bfv # This is the weight 
      wgt2_bfrv <- wgt_bfrv^2 # This is the weight used for averaging variances. This can be seen by the fact that Var(w1*x1 + w2*x2) = w1^2*Var(x1) + w2^2*Var(x2)
      #rm(n_bfrv, n_bfv)
    }) 
  #wgt_bfrv[with(wgt_bfrv, order(cbind(clef, program)))]
  
  ### Calculate avgs
  # Explanation for this method is:
  # (1) Establish an outer loop in the data table (the one that's by "bf" (which
  #     is by "byvars" and "focalcat"). Within each outer "b/f" combination...
  # (2) Run an inner loop that identifies the specific "by" and "foc" values and
  # (3) Subsets to rows that have the same by variables, by which do *not* 
  #     have the same focal category. E.g., still focus on males, but identify
  #     those that are *not* in the program of focus.
  # (4) Within this subset, calculate the mean, variance, and n of each dependent
  #     variable within each reference category (e.g. within each school). Note 
  #     that the two values output by function(dv) are output vertically rather
  #     than horizontally. Thus...
  # (5) We chain in an additional command which creates a new column of alternating
  #     values of "mean" and "var" to label the output of the f(dv) function.
  # (6) We then pipe the results into a melt, to get observations to the
  #     by variable by focal cat by reference cat by statistic by descriptive
  #     variable level.
  # (7) Which gets piped into a cast which creates variable/stat combinations to
  #     run wide.
  # /!\ NSM: I believe that data.table has optimized reshaping
  # commands that allows for a simultaneous melt/cast step.
  
  setkeyv(dt, c(vBf, idvar))
  bf_combos <- unique(dt[, vBf, with = FALSE])
  
  #bf_combos <- unique(dt[,.(lapply(c(byvars, focalcat), get, envir=as.environment(dt)))]) # the lapply() returns the focalcats as column names, and the unique() returns unique combinations among which 
  avg_bfrv <- dt[,
                 dt[{
                   # .GRP <- 1 ... auditing step
                   myByVars <- bf_combos[.GRP] # (2) Get values associated with the current "by"
                   byVarMatches <- apply(sapply(byvars, function(m) dt[, get(m)] %in% myByVars[,get(m)]), 1, function(b) mean(b) == 1)  
                   # This returns rows that match all by variables
                   myIds <- unique(dt[myByVars, get(idvar)]) # /!\ myByVars isn't subsetting properly
                   myFocCat <- myByVars[, .(get(focalcat))]
                   byVarMatches <- apply(sapply(byvars, function(m) dt[, get(m)] %in% myByVars[, get(m)]), 1, mean) == 1
                   !(get(focalcat) %in% myFocCat | get(idvar) %in% myIds) & byVarMatches # (3)
                   # attach(dt)
                   # cbind(dt, !(get(focalcat) %in% myFocCat | get(idvar) %in% myIds) & byVarMatches)
                   # detach(dt)
                 },
                 lapply(.SD, function(dv) c(mean(dv, na.rm = TRUE),
                                            var(dv, na.rm = TRUE),
                                            sum(!is.na(dv)))), # (4)
                 by = refcat,
                 .SDcols = descvars],
                 by = bf][ # (1)
                   , "stat":=c("mean", "var", "n")] %>% # (5)
    melt(id.vars = c(byvars, focalcat, refcat, "stat"),
         variable.name = "descvar") %>% # (6)
    dcast(as.formula(paste0(bfr_plus, " + descvar ~ stat")),
          value.var = "value") %>% # (7)
    data.table()
  
  ### Merge weights
  avgWgt_bfrv <- merge(x = avg_bfrv,
                       y = wgt_bfrv,
                       by = c(byvars, focalcat, refcat, "descvar"))
  
  ### Weight, calculate, output
  peerCalc <- avgWgt_bfrv[,
                          .(mean    = weighted.mean(x = mean, w = wgt_bfrv),
                            var     = weighted.mean(x = var,  w = wgt2_bfrv),
                            nbr.val = weighted.mean(x = n,    w = wgt_bfrv)), 
                          by = c(byvars, focalcat, "descvar")] %>%
    within({
      SE.mean <- sqrt(var)
      rm(var)
    })
  # Note: these variable names (e.g. "nbr.val" and "SE.mean) are chosen to be
  # consistent with the output of the SliceStats which, in turn, is consistent
  # with the output of the stat.desc() function, all of whose calculations are
  # available in the SliceStats function.
  
  return(peerCalc)
}
