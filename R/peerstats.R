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
#' getSlicedStats()

library("data.table")
library("magrittr")
library("pastecs")
#library("reshape2") ... data.table should have the reshape commands that we need

### New, data.table-based method -----------------------------------------------

#data = mtcars; descvars = c("disp", "hp", "drat", "wt", "mpg"); focalcat = "gear"; refcat = "cyl"; byvars = "vs"; id = "model"
source("sample-data-gen.R")
data = peerdata; descvars = c("value1", "value2"); focalcat = "program"; refcat = "school"; byvars = "flavor"; id = "id"

peerstats <- function(data, descvars, focalcat, refcat, byvars, id = NULL){
  # Establish data as a properly-indexed data table
  if ("data.table" %in% class(data)){
    dt <- data
  } else {
    dt <- data.table(data, key = paste(focalcat, refcat, byvars, sep = ","))
  }
  
  # Set up bys
  bf  <- paste(byvars, focalcat,         sep = ",")
  bfr <- paste(byvars, focalcat, refcat, sep = ",")
  br  <- paste(byvars,           refcat, sep = ",")
  bfr_plus <- paste(c(byvars, focalcat, refcat), collapse = " + ")
  dv_plus  <- paste(descvars, collapse = " + ")
  
  # Calculate n's, first by all by/focal/reference categories, and then by
  # chaining in an aggregation to additionally also calculate by+focal Ns
  n_bfrv <- dt[,
               lapply(.SD, function(dv) sum(!is.na(dv))),
               by = bfr,
               .SDcols = descvars][
                 ,
                 paste0(descvars, "_f") := lapply(.SD, function(n_bfr) sum(n_bfr)),
                 by = bf,
                 .SDcols = descvars
                 ] %>%
    melt(id.vars = c(byvars, focalcat, refcat)) %>%
    within({
      descvar <- gsub("_f$", "", variable)
      nfocflag   <- grepl("_f$", variable)
      ndesc <- ifelse(nfocflag, "n_bf", "n_bfr")
      rm(variable, nfocflag)
    }) %>%
    dcast(as.formula(paste0(bfr_plus, " + descvar ~ ndesc")),
          value.var = "value")
  
  # Calculate avgs
  # Explanation for this method is 
  # (1) Establish an outer loop in the data table (the one that's by "bf" (which
  #     is by "byvars" and "focalcat"). Within each outer "b/f" combination...
  # (2) Run an inner loop that identifies the specific "by.var" values, and
  #     focalcat value, and subsets to rows that have the same by variables,
  #     by which do *not* have the same focal category.
  # (3) Within this subset, calculate the mean and variance of each dependent
  #     variable. Note that the two values output by the f(dv) function are
  #     output "vertically". Thus...
  # (4) We chain in an additional command which creates a new column of alternating
  #     values of "mean" and "var" to label the output of the f(dv) function.
  # (5) We then pipe the results into a melt, to get observations to the
  #     by variable by focal cat by refrence cat by statistic by descriptive
  #     variable level.
  # (6) Which gets piped into a cast which creates variable/stat combinations to
  #     run wide.
  # /!\ NSM: I believe I'd read a way that data.table has optimized reshaping
  # commands that allows for a simultaneous melt/cast step.
  avg_bfrv <- dt[,
                 dt[{
                     myByVars <- unique(dt[,.(lapply(byvars, get),
                                              get(focalcat))])[.GRP] # Identify by/focal values relevant to the specific iteration of the outer loop
                     myIds <- unique(dt[myByVars, .(get(id.var))])
                     myFocCat <- myByVars[, .(get(focalcat))]
                     !(get(focalcat) %in% myFocCat | get(id.var) %in% myIds) # Exclude own rows from calculation
                    },
                    lapply(.SD, function(dv) c(mean(dv, na.rm = TRUE),
                                               var(dv, na.rm = TRUE))),
                    by = refcat,
                    .SDcols = descvars],
                 by = bf][,"stat":=c("mean", "var")] %>%
    melt(id.vars = c(byvars, focalcat, refcat, "stat"), variable.name = "descvar") %>%
    dcast(as.formula(paste0(bfr_plus, " + descvar ~ + stat")), value.var = "value")
  
  
  # Merge N's
  peerStats <- merge(avg, ns, by = c(byvars, focalcat, refcat)) %>%
    within({
      wgt_bfr  <- n_bfr / n_bf
      wgt2_bfr <- wgt_bfr^2
    })
  
  # Weight, calculate, output
  peerCalc <- peerStats[,
                        .(mean = weighted.mean(x = peerMean, w = wgt_g),
                          var  = weighted.mean(x = peerVar,  w = wgt2_g)),
                        by = "gear,meas"]
  
}
mtcars$model <- rownames(mtcars)
peerstats(data = mtcars,
          descvars = c("disp", "hp", "drat", "wt", "mpg"),
          focalcat = "gear",
          refcat   = "cyl",
          byvars = "vs",
          id = "model")

