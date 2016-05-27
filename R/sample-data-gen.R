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
                        cat = sample(c("Treble", "Bass"),
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



### Generate data to demonstrate peerstats function ----------------------------
set.seed(60637)
n <- 24
peerdata <- data.frame(id = 1:24,
                       program = rep(c("Prog A", "Prog B", "Prog C"), each  = n/3),
                       school = rep(c("Sch 1", "Sch 2", "Sch 3"), times = n/3),
                       val1 = 1:n,
                       val2 = sample(1:10, n, replace = TRUE),
                       flavor = sample(c("treble", "bass"), n, replace = TRUE),
                       stringsAsFactors = FALSE)
# Add in missings
peerdata$val1[runif(n) < 0.1] <- NA
peerdata$val2[runif(n) < 0.1] <- NA

# Add in duplicate youth
dup <- peerdata[c(1, 10, 20),]
dup$program <- "Prog D"
peerdata <- rbind(peerdata,dup)


peerdata <- peerdata[order(peerdata$school),]
