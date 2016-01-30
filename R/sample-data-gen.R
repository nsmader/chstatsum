### Generate sample data to demonstrate getSlicedStats and calcPeerStats functions ----

### Generate data to demonstrate getSlicedStats function -----------------------

set.seed(1313)
p <- 20 # Number of programs
n.per.p <- 40 # Observations per program
n <- p*n.per.p
proglist <- c(state.name[1:(p/2)], rep("None", p/2))

youthdata <- data.frame(id = 1:n,
                        prog = paste0(rep(proglist, each = n.per.p),
                                      " Center"),
                        sch = paste0(sample(presidents,
                                            size = n,
                                            replace = TRUE),
                                     " School"),
                        cat = sample(c("Bitter", "Sweet", "Sour", "Salty"),
                                     size = n,
                                     replace = TRUE),
                        x1 = runif(n)*10,
                        x2 = rnorm(n)*2 + 5)
progdata <- data.frame(prog = paste0(proglist, " Center"),
                       progfx = 1:p)
youthdata <- merge(x = youthdata,
                   y = progdata,
                   by = "prog")
youthdata <- within(youthdata, {
  x1[runif(n)<0.1] <- NA # Induce missings
  y <- x1 + x2 + progfx
  org <- substr(prog, 1, 1)
  org[org == "N"] <- "None"
  for (o in org){
    assign(paste0("org_", o), 1*(org == o))
  }
  rm(progfx, o, org_NA)
})



### Generate data to demonstrate calcPeerStats function ------------------------
set.seed(60637)
data <- data.frame(focal.ind = rep(c(T, F), each = 12),
                   ref.cat = rep(c("A", "B", "C"), times = 8),
                   val = sample(1:10, 24, replace = TRUE),
                   by.var = sample(c("sweet", "salty", "bitter", "sour"), 24, replace = TRUE),
                   stringsAsFactors = FALSE,
                   id = 1:24)
data <- data[order(data$ref.cat),]
# attach(data)
# desc.vars <- "val"
# ref.cat <- "ref.cat"
# by.vars <- "by.var"
# id <- "id"