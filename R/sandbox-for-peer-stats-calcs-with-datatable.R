#-------------------------------------------#
### Sandbox for peer statistics calculation #
#-------------------------------------------#

# The goal is to calculate the average characteristics for Mercs by gear, and
# also the average characteristics of all non-Merc cars with the same gear

#rm(list=ls())
### Workspace and data prep ----------------------------------------------------
mtcars
library(data.table)
library(dplyr)
mtcars
mtcars <- within(mtcars, {
  model <- rownames(mtcars)
  merc <- 1*grepl("Merc", model)
})

# 0. Make the data set long format 
cars_l <- melt(mtcars,
               id.vars = c("model", "merc", "gear"))
dt_l <- data.table(cars_l, key = "merc,gear,variable")

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


# 1. Need a calculation of proportions of focal group by reference groups. 
#    This can be done with a cross-row calculation: # focals in group (with var
#    measured)/# focals. So: subset to focals, total group 
#    Do this in its own object?

# Proportion is total of folks in focal, divided by number of folks in focal and reference
# Say, focal is gear, and reference is cyl.
# Then, we want, for a given gear, what % are in different cyl categories
dt[, ,
   by = "gear"]


merc_l <- dt_l[merc == 1]
test_l <- merc_l[, list(dot_i = .I,
                        dot_grp = .GRP,
                        dot_N = .N,
                        test = .I[which.max(value)],
                        value = value), by = "gear,variable"] # dot_by = .BY, this has the values of the by variables at each point... returns mor ethan on ealue
wgt_l <- merc_l[, list(N_sameGear = sum(!is.na(value[gear==gear[.I]])), # Is returning something weird
                       N_AllGears = sum(!is.na(value))),  # Worked as expected
                by = "variable"]




# 2. Subsetting by schools that focals are in, calculate average stats that are
#    Intermediates to the next weighted calculation.

# 3. Join weights and average calculations

# 4. Weight, and done.




# 2. Need just three calculations which are standard stats, but weighted by the
#    weight calculated in the above step.
#    Set key in order to do this?