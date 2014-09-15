# A function for identifying individuals with alleles that occur in fewer than
# n individuals. This function is useful for identifying low frequency alleles
# which may actually be artifact peaks, etc.

# Written by Kevin Keenan, 2013
getLowFreq <- function(database, marker, n) {
  db <- database
  loc <- marker
  rd <- BinStat(db, loc)
  dat <- cbind(rd$Min[rd$N <= n], rd$Max[rd$N <= n])
  if (nrow(dat) == 0L) {
    cat("No valid samples")
  } else {
    # Add and subtract a small quantity
    for (i in 1:nrow(dat)) {
      dat[i, 1] <- dat[i, 1] - 0.01
      dat[i, 2] <- dat[i, 2] + 0.01
    }
    output <- apply(dat, 1, function(x) {
      return(subdataBase(db, loc, x[1], x[2]))
    })
    return(do.call("rbind", output))
  }
}