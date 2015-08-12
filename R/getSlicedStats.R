#' Calculate multiple statistics for many variables and data subsets
#'
#' This function wraps use of data.table(), stat.desc() and helpful data
#' reshaping courtesy of reshape2.
#' @param data This is the data frame that will be operated on
#' @param vars Variables to summarize
#' @param slice.vars Vector of variables which subset the data set
#' @param stats Statistics to calculate. The valid list of values comes from the stat.desc() function
#' @keywords data.table stat.desc pastecs reshape2
#' @export
#' @examples
#' getSlicedStats()

# library("data.table")
# library("pastecs")
# library("reshape2")

getSlicedStats <- function(data, vars, slice.vars, stats = c("mean", "SE.mean", "nbr.val")){
  statNames <- names(stat.desc(runif(2)))
  vslice.vars <- strsplit(slice.var, ",")[[1]]
  nslice.vars <- length(vslice.vars)
  fmslice.var <- paste(vslice.vars, collapse = " + ")
  
  dtdata <- data.table(undupData, by = slice.var)
  dtStats <- dtdata[, lapply(.SD, stat.desc), by = slice.var, .SDcols = descVars]
  dfStats <- data.frame(dtStats)
  dfStats$stat <- statNames
  dfStats <- dfStats[dfStats$stat %in% stats,]
  
  dfStats_l <- melt(dfStats, id = c(vslice.vars, "stat"), variable = "variable")
  dfStats <- dcast(dfStats_l, as.formula(p0(fmslice.var, " + variable ~ stat")))
  
  colnames(dfStats)[1:nslice.vars] <- vslice.vars
  dfStats$sliceVars <- slice.vars
  
  dfStats
}
      
