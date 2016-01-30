setwd("C:/Users/nmader/Documents/GitHub/tcbTools/R")
source("sample-data-gen.R")
source("getSlicedStats.R")
source("calcPeerStats.R")
p0 <- function(...) paste0(...)
cn <- function(x) cn(x)

library("data.table")
library("pastecs")
library("reshape2")
library("plyr")

norm01 <- function(x) (x - mean(x, na.rm = TRUE)) /
                        sd(x, na.rm = TRUE)

### Messing with ddply as another approach

# Try standard ddply

z <- ddply(youthdata, .(org_A, cat), mutate,
           x1_mean = mean(x1, na.rm = TRUE),
           x2_mean = mean(x2, na.rm = TRUE),
            y_mean = mean(y,  na.rm = TRUE),
           x1_norm = norm01(x1),
           x2_norm = norm01(x2),
            y_norm = norm01(y))
aggregate(cbind(x1, x1_norm) ~ org_A + cat,
          data = z,
          FUN = function(x) round(mean(x, na.rm = TRUE)))

# Mess with summarise_each/mutate_each

myBy <- youthdata %>% group_by(org_A, prog, cat)
mean.na <- function(x) mean(x, na.rm = TRUE)
se <- function(x) sd(x, na.rm = TRUE) / (sum(!is.na(x)) - 1)
nbr.val <- function(x) sum(!is.na(x))
as.data.frame(myBy %>% summarise_each_(funs(mean.na, se, nbr.val), vars))

# Horserace ddply summarize_by with my function
