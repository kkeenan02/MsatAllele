DefBinLim<-function (marker, append2 = NULL, alleles) {
    if (is.null(append2)) {
        BinLimits <- NULL
    }
    else {
        BinLimits <- append2[, 3]
    }
    nall <- length(alleles)
    for (i in 1:nall) {
        print(paste("Click on lower bin limit for allele", alleles[i]))
        
               if (.Platform$OS.type == "windows") flush.console()
            
        
        BinLimits <- c(BinLimits, locator(1)$y)
        if (length(BinLimits) > 1) {
            if (any(BinLimits[length(BinLimits)] <= BinLimits[-length(BinLimits)])) 
                stop("Bin limit is less than previously defined for smaller allele")
        }
    }
    if (is.null(append2)) {
        BinLimitsDF <- data.frame(rep(marker, nall), alleles, 
            BinLimits)
        names(BinLimitsDF) <- c("marker", "alleles", "LowerBin")
    }
    else {
        BinLimitsDF <- data.frame(rep(marker, (nrow(append2) + 
            nall)), c(append2[, 2], alleles), BinLimits)
        names(BinLimitsDF) <- c("marker", "alleles", "LowerBin")
    }
    BinLimitsDF
}
