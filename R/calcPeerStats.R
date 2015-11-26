#' Calculate summary statistics for the representative peer of a given group of
#' observations.
#'
#' This function returns representative characteristics of a benchmark 
#' populations based on a shared characteristics. For example, this returns the
#' average characteristics of peers attending the same schools as youth
#' attending a given youth program, where those youth may attend multiple schools.
#' @param data This is the data frame that will be operated on
#' @param desc.vars Variables to summarize or ("describe")
#' @param focal.ind True/False vector indicating which observations represent the focal subpopulation.
#' @param ref.cat Categorical designation of what determines the reference population.
#' @param by.vars Variables to use to slice different calculations, such as year and organization.
#' @param id (Optional) Unique identifier to use in deduplicating observations.
#' @keywords data.table stat.desc pastecs reshape2
#' @export
#' @examples
#' getSlicedStats()

library("data.table")
library("pastecs")
library("reshape2")

### peerStats.fn ---------------------------------------------------------------

peerStats.fn <- function(myProps, myVar, mySchStats){
  
  stats.props <- merge(mySchStats, myProps, by = "ref.cat")  # merge proportions into school-level stats data
  stats.props <- stats.props[!is.na(stats.props$mean), ] # remove school rows with missing calculations - XXX Could be refactored to be faster if dropping these all ahread of time
  stats.props$prop <- stats.props$prop / sum(stats.props$prop) # inflate the proportions to reflect any drops of schools with missing values
  stats.props$prop2 <- stats.props$prop^2 # This weight is necessary for variance calculations, indicated in the method notes
  
  mean <- weighted.mean(stats.props$mean, stats.props$prop, na.rm = T)
  nbr.val  <- weighted.mean(stats.props$n, stats.props$prop, na.rm = T)
  s2 <- sum(stats.props$var_mean * stats.props$prop2)
  
  out <- data.frame(variable = myVar, mean = mean, nbr.val = nbr.val, SE.mean = sqrt(s2))
  return(out)

}

### calcPeerStats --------------------------------------------------------------

set.seed(60637)
try(detach(data), silent = TRUE)
data <- data.frame(focal.ind = rep(c(T, F), each = 12),
                   ref.cat = rep(c("A", "B", "C"), times = 8),
                   val = sample(1:10, 24, replace = TRUE),
                   by.var = sample(c("sweet", "salty", "bitter", "sour"), 24, replace = TRUE),
                   stringsAsFactors = FALSE,
                   id = 1:24)
data <- data[order(data$ref.cat),]
attach(data)
desc.vars <- "val"
ref.cat <- "ref.cat"
by.vars <- "by.var"
id <- "id"

calcPeerStats <- function(data, desc.vars, focal.ind, ref.cat, by.vars, id = NULL){
  
  # Abbreivations are:
  # f - focal individual
  # p - peer
  # rc - reference category
  # dup - duplicates
  # s - stats
  # _l - suffix indicating that it's "long"
  
  # /!\ See if we can get speed-ups by using data.table(). This is largely
  # because the focal population can be very small, and relevant reference categories
  # also very few.
  # Note that df[, var] is equivalently accessed as dt[, var, with = F]
  # Also see example(data.table) for a long list of helpful examples
  # Also see the examples here: http://user2014.stat.ucla.edu/files/tutorial_Matt.pdf
  # ... do this by converting the code below, step by step, to check on equivalence
  
  # Identify schools attended by focal youth
  
    data           <- data[, c("focal.ind", ref.cat, desc.vars, by.vars)]
    dfFocs         <- data[focal.ind, ]
    if (!is.null(id)){
      fdup_ind <- duplicated(dfFocs[, c(id, ref.cat)])
      dfFocs <- dfFocs[!fdup_ind,]
    }
    dfFocRefCats.u <- unique(dfFocs$ref.cat)
    
  # Identify records of unserved served peers in these schools
    dfPeers <- subset(x = data,
                      subset = !focal.ind & 
                               ref.cat %in% dfFocRefCats)
    if (!is.null(id)){
      pdup_ind <- duplicated(dfPeers[, c(id, ref.cat)])
      dfPeers <- dfPeers[!pdup_ind,]
    }
  
  # Calculate the characteristics of these peers by school
    
    statnames <- names(stat.desc(runif(2))) # Gets statistic names direction from current design of stat.desc()
    dtPeers <- data.table(dfPeers, key = "ref.cat")
    dtPeerStats <- dtPeers[, lapply(.SD, stat.desc), by = "ref.cat", .SDcols = descVars]
    dfPeerStats <- data.frame(dtPeerStats)
    dfPeerStats$stat <- statNames
    dfPeerStats <- subset(dfPeerStats, subset = stat %in% c("mean", "SE.mean", "nbr.val"))
    
    dfPeerStats_l <- melt(dfPeerStats, id = c("ref.cat", "stat"), variable = "descVar")
    dfPeerStats <- dcast(dfPeerStats_l, ref.cat + descVar ~ stat)
  
  # Set up header for identifying calculations
    outHeader <- data.frame(population = population, year = year, sliceVar = sliceVar, sliceVal = sliceVal)

  # Calculate and gather peer characteristics
    # Note: this process must be done individually by variable since both the 
    # numbers of focal youth--and their peers--will vary based on the variable.
    # For example, while all focal youth may have gender recorded, only a subset
    # will have valid survey responses, or a measure of 9th grade on-track
    # status. In practice, this changes the proportions of reference cats 
    # that the focals represent.
    
    allVarCalcs <- NULL
    for (v in descVars){
      
      # Get proportions of schools represented by focal youth involved in the calculation
        frc <- dfFocals$ref.cat[!is.na(dfFocals[, v])]
        if (length(vFocalRefCats) == 0) next # Some variables are not available in a given year. In this case, skip that variable in the loop
        dfSchProps <- data.frame(prop.table(table(vFocalRefCats)))
        colnames(dfSchProps) <- c("ref.cat", "prop")
      
      # Calculate characteristics of peers, excluding focal youth
        peerStats_bySch <- dfPeerStats[!is.na(dfPeerStats$mean), ]
        try(peerStats_bySch[peerStats_bySch$n == 1, c("SE.mean")] <- 0, silent=T)
          # XXX This is a bit of a stop-gap, since some calculations have zero rows after
      
      # Get weighted average
        peerStat <- peerStats.fn(myProps = dfSchProps, myVar = v, mySchStats = peerStats_bySch[peerStats_bySch$descVar == v, ])
        allVarCalcs <- rbind(allVarCalcs, cbind(outHeader, peerStat))
      
    } # End of loop across descVars
}