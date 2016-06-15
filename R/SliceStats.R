#' Calculate multiple statistics for many variables and data subsets
#'
#' This function wraps use of data.table(), stat.desc() and helpful data
#' reshaping courtesy of reshape2.
#' @param data This is the data frame that will be operated on
#' @param vars Variables to summarize
#' @param slicevars Vector of variables which subset the data set
#' @param stats Statistics to calculate. The valid list of values comes from the stat.desc() function, including
#' @keywords data.table stat.desc pastecs reshape2
#' @export
#' @examples
#' SliceStats()

library("data.table")
library("pastecs")
library("magrittr")

data <- youthdata; vars = c("x1", "x2", "y"); slicevars <- c("org_A", "prog", "cat");
stats <- c("mean", "SE.mean", "nbr.val"); suppress_n = NULL; id_delim = "__"; label = FALSE
test <- SliceStats(data = youthdata,
                   vars = c("x1", "x2"),
                   slicevars = c("prog", "cat"),
                   stats = c("mean", "nbr.val"))
cartest <- SliceStats(data = mtcars,
                      vars = c("wt", "hp", "mpg"),
                      slicevars = c("cyl", "gear"),
                      stats = c("mean", "nbr.val"))
peertest <- SliceStats(data = peerdata, 
                       vars = c("val1", "val2"),
                       slicevars = c("clef", "program"),
                       suppress_n = 3)

SliceStats <- function(data, vars, slicevars, stats = c("mean", "SE.mean", "nbr.val"), suppress_n = NULL, id_delim = "__", label = FALSE){
  ### Set up inputs to the calculations
  statNames <- names(stat.desc(runif(2))) 
  vslicevars <- slicevars
  cslicevars <- paste(slicevars, collapse = ",") # strsplit(slicevars, ",")[[1]]
  nslicevars <- length(vslicevars)
  fmslice.var <- paste(vslicevars, collapse = " + ")
  
  ### Run calculations and select desired output
  dtData <- data.table(data, key = cslicevars)
  dtStats <- dtData[, lapply(.SD, stat.desc), by = cslicevars, .SDcols = vars]
  dtStats$stat <- statNames
  dtStats <- dtStats[stat %in% stats]
  
  ### Reshape data
  dtStats_l <- melt(dtStats,
                    id.vars = c(vslicevars, "stat"),
                    variable.name = "x") 
  dtStats_w <- dcast(dtStats_l,
                     formula = as.formula(paste0(fmslice.var, " + x ~ stat")),
                     value.name = "value",
                     variable.name = "stat")
  
  ### Generate structured ID variable for each row
  # Pattern is "slicevar1:sliceval1__slicevar2:sliceval2__" etc
  valPairs <- cbind(sapply(vslicevars,
                           function(sv) paste0(sv, ":", dtStats_w[, get(sv)])),
                    paste0("x:", dtStats_w$x))
  dtStats_w$id <- apply(valPairs, 1, paste, collapse = id_delim)
  
  ### Suppress values for anything except the number of valid values
  if (is.numeric(suppress_n) & sum(dtStats_w$nbr.val < suppress_n) > 0 ){
    set(x = dtStats_w,
        i = which(dtStats_w$nbr.val < suppress_n),
        j = which(colnames(dtStats_w) %in% c("mean", "SE.mean")),
        value = NA)  
  }
  
  dtStats_w
}

