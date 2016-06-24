#------------------------------------------------------------------------------#
#
# Test SliceStats and PeerStats function
#
#------------------------------------------------------------------------------#
rm(list = ls())
setwd("C:/Users/nmader/Documents/GitHub/chstatsum")
source("R/sample-data-gen.R")
source("R/SliceStats.R")
source("R/PeerStats.R")
library(plyr)

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
### Demo problems with fake data -----------------------------------------------
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

### Examine progdata -----------------------------------------------------------
# This is a small data set, with representative problems

# Variables have missings
summary(progdata)

# Youth are sometimes enrolled in multiple different programs
idcounts <- table(progdata$id)
dups <- subset(progdata, id %in% names(idcounts[idcounts>1]))
dups[order(dups$id),]

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
### Test Slice Stats Data ------------------------------------------------------
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

### Fake data, comparing results conventional methods by hand ------------------
slicetest <- SliceStats(data = progdata, 
                        vars = c("val1", "val2"),
                        slicevars = c("clef", "program"),
                        suppress_n = 3)

# Calculate proper data by hand
# mysub <- subset(progdata, clef == checkclef & program == checkprogram)
progdata_l <- melt(progdata, id.vars = c("id", "program", "school", "clef"))
dups <- duplicated(progdata_l)
progdata_l <- progdata_l[!dups, ]
byhand <- ddply(progdata_l, .(clef, program, variable), summarise,
                mu = mean(value, rm = TRUE),
                n = sum(!is.na(value)),
                semean = sd(value, na.rm = TRUE)/sqrt(n))

checkmerge <- merge(x = slicetest, y = byhand,
                    by.x = c("clef", "program", "x"),
                    by.y = c("clef", "program", "variable"))
with(checkmerge, all(mu == mean, na.rm = TRUE))
with(checkmerge, all(semean == SE.mean, na.rm = TRUE))
with(checkmerge, all(n == nbr.val, na.rm = TRUE))


#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
### Test Peer Stats Data -------------------------------------------------------
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

### Test Peer Stats code against fake data--------------------------------------
peertest <- PeerStats(data = progdata,
                      descvars = c("val1", "val2"),
                      focalcat = "program",
                      refcat   = "school",
                      byvars = "clef",
                      idvar = "id")
# Spot-checking value with treble Prog D, both variables, since there are
#   (1) missings between the two descriptive variables, and
#   (2) one of the enrolled students (id=1) is duplicate (also enrolled with program A)
check.clef = "treble"; check.program = "Prog D"
peertest.sub <- subset(peertest, clef == check.clef & program == check.program)

hand.focals <- subset(progdata, clef == check.clef & program == check.program)
hand.schprops <- prop.table(table(hand.focals$school))
hand.peers <- subset(progdata,
                     subset = clef == check.clef & program != check.program & # Check youth in same subset ("clef"), who are peers with respect to category ("program")
                              school %in% unique(hand.focals$school) & # ... subsetting to same reference group ("school") 
                              !(id %in% hand.focals$id)) # ... who aren't duplicates of my focal youth
hand.peers_l <- melt(hand.peers,
                     id.vars = c("id", "program", "school", "clef"))
hand.schstats <- ddply(hand.peers_l, .(school, variable), summarise,
                       mu_sch = mean(value, na.rm = TRUE),
                       n_sch = sum(!is.na(value)),
                       var_sch = var(value, na.rm = TRUE))
hand.schstats <- within(hand.schstats, {
  wgt <- hand.schprops[school]
  wgt2 <- wgt^2 # see "hand.peerstats step for explanation for why we need this squared weight term
})
hand.schstats
hand.peerstats <- ddply(hand.schstats, .(variable), summarise,
                        mu  = weighted.mean(x = mu_sch,  w = wgt),
                        n   = weighted.mean(x = n_sch,   w = wgt),
                        var_mu = weighted.mean(x = var_sch, w = wgt2),
                        se_mu = sqrt(var_mu)) %>% within(rm(var_mu))
  # Calculation of "var" is because, generally: Var(aX + bY) = a^2*Var(X) + b^2*Var(Y) (if X and Y are independent random variables)
  # When specifically calculating variance of a weighted mean: Var(w1*X1 + w2*X2 + ... + wkXk) = w1^2*Var(X1) + w2^2*Var(X2) + ... + wk*Var(Xk)
  # We know that the Xi's--which represent school means--are independent because they involve different groups of youth
check.stats <- merge(x = peertest.sub,
                     y = hand.peerstats,
                     by.x = "descvar",
                     by.y = "variable")
with(check.stats, all(mean == mu))
with(check.stats, all(SE.mean == se_mu))
with(check.stats, all(nbr.val == n))


### Test whether peer stats works without a focal cat (and only by vars) -------
# The result should be a calculation of 

nofoc.test <- PeerStats(data = progdata,
                        descvars = c("val1", "val2"),
                        refcat   = "school",
                        byvars = c("clef", "program"),
                        idvar = "id")



#------------------------------------------------------------------------------#
### Notes ----------------------------------------------------------------------
#------------------------------------------------------------------------------#
# - One question that came up along the way is how to construct peer weights.

#Warning: calculating focal proportions only for focals that have a non-missing value
#   Check what the current procedure is, and make a note of this option. Could see 
#   doing this either way.



