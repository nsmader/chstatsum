# This is a sandbox for playing with features of data.table --------------------

library(data.table)
mtcars
mtcars <- within(mtcars, {
  model <- rownames(mtcars)
  merc <- 1*grepl("Merc", model)
})
cars_l <- melt(mtcars,
               id.vars = c("model", "merc", "gear"))
dt_l <- data.table(cars_l, key = "merc,gear,variable")

### Data table assigns variables like we're used to?
dt_l$five <- 5
dt_l <- within(dt_l, {
  eleven <- five + 6
  rm(five)
})
merc_l <- subset(dt_l,
                 subset = merc == 1,
                 select = !grepl("merc|eleven", colnames(dt_l)))
merc_w <- dcast(merc_l,
                model + variable ~ gear,
                value.var = "value")
class(merc_l) # Still a data.table
class(merc_w) # Just a data.frame now

### Benchmark time cost of different operations --------------------------------
# Specifically, we're looking at how calculation times are influenced by
# (1) data set size; (2) number of by variables; (3) whether a key is set
library(data.table)
# Set up data sets
n <- 1e7
dt_n1e7 <- data.table(clef   = sample(c("treble", "bass"), size = n, replace = TRUE),
                      flavor = sample(c("sweet", "salty", "bitter", "sweet"), size = n, replace = TRUE),
                      value = rnorm(n))
dtk_n1e7 <- setkey(dt_n1e7, "clef", "flavor")

# Compare run time by # of bys, and setkey
system.time( dt_n1e7[, mean(value)])
system.time( dt_n1e7[, mean(value), by = "clef"])
system.time( dt_n1e7[, mean(value), by = "clef,flavor"])
system.time(dtk_n1e7[, mean(value), by = "clef,flavor"])

# Compare run times by n
runtimes_byn <- NULL
for (size in c(1e2, 1e3, 1e4, 1e5, 1e6, 1e7, 1e7)){
  print(paste("Running loop for size", size))
  runtime <- system.time(for (b in 1:100) (dtk_n1e7[sample(1:n, size = size), mean(value), by = "clef,flavor"]))["elapsed"]
  runtimes_byn <- rbind(runtimes_byn, data.frame(size = size, time = runtime))
}
with(runtimes_byn, plot(x = size, time = runtime))
  # Looks like I'm generally having problems with memory

summary(lm(time ~ size + I(size^2), data = runtimes_byn))
  

### Work on passing string arguments to columns for "i" argument ---------------
dt_l
dt_l[grepl("Hornet", model)]
dt_l[grepl("Hornet", get("model"))] # works

### Working on merging ---------------------------------------------------------
head(dt_l)
sub <- subset(dt_l, subset = variable == "mpg")
dt_l_merge <- dt_l[sub] # Very weird results--didn't just subset to mpg... assigned more to mpg. Presumably created a permutation of records
# Trying to select on only the 
sub2 <- subset(dt_l, subset = variable == "mpg", select = variable)
dt_l_merge2 <- dt_l[sub2] # Yields error
# Trying to indicate keys
setkey(dt_l, "variable")
sub2 <- subset(dt_l, subset = variable == "mpg", select = variable)
dt_ltmerge3 <- dt_l[unique(sub2)] # Works. Lesson: need to set key for this.


### Working on calculation across columns --------------------------------------



