#-------------------------------------------#
### Sandbox for peer statistics calculation #
#-------------------------------------------#

# The goal is to calculate the average characteristics for Mercs by gear, and
# also the average characteristics of all non-Merc cars with the same gear

### Workspace and data prep ----------------------------------------------------
rm(list=ls())
mtcars
library(data.table)
library(plyr)
mtcars
mtcars <- within(mtcars, {
  model <- rownames(mtcars)
  merc <- 1*grepl("Merc", model)
})

# 0. Make the data set long format 
cars_l <- melt(mtcars,
               id.vars = c("model", "merc", "gear"))
dt_l <- data.table(cars_l, key = "merc,gear,variable")

### Miscellaneous experiementation with automatic variables created in data.table ----
merc_l <- dt_l[merc == 1]
test_l <- merc_l[, list(dot_i = .I,
                        dot_grp = .GRP,
                        dot_N = .N,
                        test = .I[which.max(value)],
                        value = value), by = "gear,variable"] # dot_by = .BY, this has the values of the by variables at each point... returns mor ethan on ealue
wgt_l <- merc_l[, list(N_sameGear = sum(!is.na(value[gear==gear[.I]])), # Is returning something weird
                       N_AllGears = sum(!is.na(value))),  # Worked as expected
                by = "variable"]


### Experimenting with code from Andrew Brooks post on excluding certain rows from calculations ----
# http://brooksandrew.github.io/simpleblog/articles/advanced-data-table/#calculate-a-function-over-a-group-using-by-excluding-each-entity-in-a-second-category

### Andrew Brooks' straight average
dt <- data.table(mtcars, key = "cyl,gear")
avgMpg_c <- dt[, # gear != gear[.I]
               .(mpg_avg = mean(mpg)),
               by = "cyl"]
avgMpg_c
dt[cyl == 4, mean(mpg)]

# Andrew Brooks' average minus current category
avgMpgPeer_c <- dt[,
                   dt[!(gear %in% unique(dt$gear)[.GRP]), # 2. The "unique(dt$gear)[.GRP]" pulls the right gear value. This subsets away from the focal gear as desired.
                      mean(mpg),
                      by = "cyl"], # 3. The calculation is still by cylinder. It doesn't matter that this is happening in the inner loop, since it's our only condition.
                   by = "gear"] # 1. Separates the job by gear, which establishes GRP numbering of these values.
avgMpgPeer_c
dt[cyl == 4 & gear != 3, mean(mpg)]

# Adding a second condition to generalize -- calculation by cyl and vs
with(mtcars, table(cyl, vs))
avgMpgPeer_cv <- dt[,
                   dt[!(gear %in% unique(dt$gear)[.GRP]),
                      mean(mpg),
                      by = "cyl,vs"],
                   by = "gear"]
avgMpgPeer_cv
dt[cyl == 4 & vs == 1 & gear != 3, mean(mpg)]

# Note: in our peer stats application, we're only ever excluding based on one
# criterion, i.e. that student's aren't enrolled in a given program


# 1. Calculation of proportions of focal group by reference groups.  -----------
#    This can be done with a cross-row calculation: # focals in group (with var
#    measured)/# focals. So: subset to focals, total group 
#    Do this in its own object?

# Proportion is total of folks in focal, divided by number of folks in focal and reference
# Say, focal is gear, and reference is cyl.
# Then, we want, for a given gear, what % are in different cyl categories
dt_cgm <- melt(dt,
               id.vars = c("model", "cyl", "gear"),
               variable.name = "meas")
setkeyv(dt_cgm, c("cyl", "gear"))
# Randomly add missing values to ensure that our functions don't count those
mean(is.na(dt_cgm$value))
dt_cgm[runif(nrow(dt_cgm))<0.10, value:=NA]
mean(is.na(dt_cgm$value))

dt_cgm[, foc_n := sum(!is.na(value)),      by = "gear,meas"]
dt_cgm[, focInRef_n := sum(!is.na(value)), by = "gear,meas,cyl"]
dt_cgm[, wgt_c := focInRef_n / foc_n]
n_gm  <- dt_cgm[, .(n_foc      = sum(!is.na(value))), by = "gear,meas"]
n_cgm <- dt_cgm[, .(n_focInRef = sum(!is.na(value))), by = "gear,meas,cyl"]


### Check proportion calculations
# Note that the proportion calculations are across all observations in the 
# reference categories. Thus, they aren't direct weights for each observation,
# but rather weights for calculations across the observations
checkProps <- unique(dt_cgm[gear == 3 & meas == "mpg", .(gear, cyl, wgt_c)])
colSums(checkProps)


### 2. Calculate averages and variances across all non-focal observations
# Note: this procedure would be good for doing calculations for an entire slice
# var rather than just each slice value of a given slice var at a time. E.g.,
# instead of calculating the sch-based peer stats for the YMCA's CSI program,
# we could get the sch-based peer stats for CSI, Achievers, etc.
# Caveat for this is that this still needs handling of unique individuals.
# If we get a good speed-up, could we consider having multiple versions of the
# calculation, where in one we exclude all org-served youth in the peers, and one
# we don't.
# /!\ Still need to calculate the n of peer observations
avg <- dt_cgm[,
       dt_cgm[!(gear %in% unique(dt_cgm$gear)[.GRP]),
              .(peerMean = mean(value, na.rm = TRUE), peerVar = var(value, na.rm = TRUE)),
              by = "meas,cyl"],
       by = "gear"]

avg[cyl == 6 & meas == "mpg"]
dt_cgm[cyl == 6 & meas == "mpg" & gear != 3, var(value, na.rm = TRUE)]


### 3. Join weights and average calculations -----------------------------------
peerStats <- merge(avg, n_gm, by = c("gear", "meas")) %>%
  merge(n_cgm, by = c("gear", "meas", "cyl")) %>%
  within({
    wgt_g  <- n_focInRef / n_foc
    wgt2_g <- wgt_g^2
  })
  # Regarding the squared weight, note that the mean is mu = n1/N*v1 + n2/N*v2 + ... + nk/N*vk = 
  # w1*v1 + w2*v2 + ... + wk*vk, and that var(mu) = w1^2*var(v1) + w2^2*var(v2) + ... + wk^2*var(vk)
  # Thus, the squared weights are effectively the simple weights that we need for the variance 
  # calculations.
peerStats[meas == "mpg"]

### 4. Weight, calc, and done --------------------------------------------------
peerCalc <- peerStats[,
                      .(mean = weighted.mean(x = peerMean, w = wgt_g),
                        var  = weighted.mean(x = peerVar,  w = wgt2_g)),
                      by = "gear,meas"]
peerCalc[meas == "mpg"]
peerStats[meas == "mpg" & gear == 3]

# Check values
check <- dt_cgm[meas == "mpg" & gear != 3, .(meas, gear, cyl, value)]
checkMean <- aggregate(value ~ cyl, check, mean, na.rm = TRUE)
prop <- as.data.frame(prop.table(table(subset(dt_cgm, gear == 3 & !is.na(value) & meas == "mpg", cyl))))
prop$cyl <- as.numeric(as.character(prop$Var1))
mean_prop <- merge(checkMean, prop[, c("cyl", "Freq")], by = "cyl")
paste("The mean mpg for gear 3's peers in the same cyl =",
      with(mean_prop, weighted.mean(x = value, w = Freq)))
