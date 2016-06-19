### Generate sample data to demonstrate getSlicedStats and calcPeerStats functions ----

### Generate data to demonstrate PeerStats and SliceStats function -------------
set.seed(60637)
n <- 24
progdata <- data.frame(id = 1:24,
                       program = rep(c("Prog A", "Prog B", "Prog C"), each  = n/3),
                       school = rep(c("Sch 1", "Sch 2", "Sch 3"), times = n/3),
                       val1 = 1:n,
                       val2 = sample(1:10, n, replace = TRUE),
                       clef = sample(c("treble", "bass"), n, replace = TRUE),
                       stringsAsFactors = FALSE)
# Add in missings
progdata$val1[runif(n) < 0.1] <- NA
progdata$val2[runif(n) < 0.1] <- NA

# Add in duplicate youth program enrollments
dup <- progdata[c(1, 10, 20),]
dup$program <- "Prog D"
progdata <- rbind(progdata,dup)

# Add in straight-up duplicate rows
progdata <- rbind(progdata,dup) # doing this a second time
progdata <- rbind(progdata, progdata[c(2,12),])

# Sort records
progdata <- progdata[order(progdata$id),]
