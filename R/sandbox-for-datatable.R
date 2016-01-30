# This is a sandbox for playing with features of data.table

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
