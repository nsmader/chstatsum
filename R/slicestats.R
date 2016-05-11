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
#' slice.stats()

library("data.table")
library("pastecs")
library("reshape2")

data <- youthdata; vars = c("x1", "x2", "y"); slice.vars <- c("org_A", "prog", "cat"); stats <- c("mean", "SE.mean", "nbr.val"); label = FALSE
test <- slice.stats(data = youthdata,
                       vars = c("x1", "x2"),
                       slice.vars = c("prog", "cat"),
                       stats = c("mean", "nbr.val"))
cartest <- slice.stats(data = mtcars,
                       vars = c("wt", "hp", "mpg"),
                       slice.vars = c("cyl", "gear"),
                       stats = c("mean", "nbr.val"))

slice.stats <- function(data, vars, slice.vars, stats = c("mean", "SE.mean", "nbr.val"), suppress.n = NULL, label = FALSE){
  statNames <- names(stat.desc(runif(2)))
  vslice.vars <- slice.vars
  cslice.vars <- paste(slice.vars, collapse = ",") # strsplit(slice.vars, ",")[[1]]
  nslice.vars <- length(vslice.vars)
  fmslice.var <- paste(vslice.vars, collapse = " + ")
  
  ### Run calculations and select desired output
  dtData <- data.table(data, key = cslice.vars)
  dtStats <- dtData[, lapply(.SD, stat.desc), by = cslice.vars, .SDcols = vars]
  dfStats <- data.frame(dtStats) # /!\ Look for ways to do all operations in data.table
  dtStats$stat <- statNames
  dtStats <- dtStats[stat %in% stats]
  
  ### Reshape data
  dtStats_l <- melt(dtStats,
                    id.vars = c(vslice.vars, "stat"),
                    variable.name = "var") 
  dfStats <- dcast(dtStats_l,
                   formula = as.formula(paste0(fmslice.var, " + var ~ stat")),
                   value.name = "value",
                   variable.name = "stat") # Note that casting returns a data frame
  
  ### Generate structured ID variable for each row
  #colnames(dfStats)[1:nslice.vars] <- vslice.vars
  paste0("org_A:", dfStats$org, "&prog:", dfStats$prog)
  
  dfStats$id <- do.call(function(...) paste(..., sep = "&&"),
                        as.data.frame(cbind(sapply(1:nslice.vars, function(i) paste0(vslice.vars[i], ":", dfStats[, vslice.vars[i]])), paste0("var:", dfStats$var))))
  if (label == TRUE) dfStats$label <- gsub("&&", ", ", dfStats$id)
  
  ### Suppress values for anything except the number of valid values
  statsToSuppress <- statNames[!grepl("nbr", statNames)]
  if (!is.null(suppress.n)) dfStats[dfStats$nbr.val < suppress.n,
                                    colnames(dfStats) %in% statsToSuppress] <- NA
    
  dfStats
}

