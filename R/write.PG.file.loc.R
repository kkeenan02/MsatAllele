

write.PG.file.loc<-function (DB, refDB = DB, outfile = "Population genetics file.txt") 
{
    Samples <- levels(as.factor(DB[, 2]))
    Nsamples <- nlevels(as.factor(DB[, 2]))
    Markers <- levels(DB[, 1])
    Nmarkers <- nlevels(DB[, 1])
    PopGenTab <- 1:((Nsamples + 1) * (Nmarkers + 1))
    dim(PopGenTab) <- c(Nsamples + 1, Nmarkers + 1)
    PopGenTab[1, 1] <- ""
    PopGenTab[2:(Nsamples + 1), 1] <- Samples
    PopGenTab[1, 2:(Nmarkers + 1)] <- Markers
    PopGenTab[2:(Nsamples + 1), 2:(Nmarkers + 1)] <- NA
    AlleleCounts <- PopGenTab
    ind <- 2
    m <- 1
    alleles <- 1:2
    repeat {
        LocusDBF <- OrderByLocus(refDB, Markers[m])
        repeat {
            c1 <- LocusDBF[, 1] == PopGenTab[ind, 1]
            if (length(c1[c1 == TRUE]) > 0) {
                temp <- ifelse(is.logical(c1), alleles <- LocusDBF[c1, 
                  2], alleles <- c("", ""))
                g <- 1
                i <- 1
                Genotype <- 1:2
                while (g <= 2) {
                  c2 <- (LocusDBF[, 2] >= alleles[i] - 0.8 & 
                    LocusDBF[, 2] <= alleles[i] + 0.8)
                  Checkrange <- LocusDBF[c2, 2]
                  j <- 1
                  dif <- 0
                  Lcheck <- length(Checkrange)
                  while (j < Lcheck) {
                    dif <- Checkrange[j + 1] - Checkrange[j]
                    if (dif >= 0.4) 
                      break
                    j <- j + 1
                  }
                  ifelse(j < Lcheck - j, Allrange <- Checkrange[(j + 
                    1):Lcheck], Allrange <- Checkrange[1:j])
                  Genotype[g] <- round(median(Allrange), digits = 0)
                  g <- g + 1
                  ifelse(length(alleles) > 1, i <- i + 1, i)
                }
                PopGenTab[ind, m + 1] <- as.numeric(paste(Genotype[1], 
                  Genotype[2], sep = ""))
                AlleleCounts[ind, m + 1] <- length(alleles)
                ind <- ind + 1
            }
            else {
                (ind <- ind + 1)
            }
            if (ind > (length(Samples) + 1)) 
                break
        }
        m <- m + 1
        ind <- 2
        if (m > Nmarkers) 
            break
    }
    write.table(PopGenTab, paste(outfile, ".PG.loci.txt", sep = ""), 
        sep = "\t", quote = FALSE, na = "000000", col.names = FALSE, 
        row.names = FALSE)
    AlleleCounts<-AlleleCounts[,2:length(AlleleCounts[1,])]
    
    
    s<-2
    l<-1
    warn<-NA
    
    repeat{
    repeat{
    
    if(is.na(AlleleCounts[s,l])){l<-l+1}else{
    			    if(AlleleCounts[s,l]>2)
    				{
    				warn<-c(warn,AlleleCounts[1,l]);
    				l<-l+1}else{l<-l+1}
    				}
    				
    					
    if(l>length(AlleleCounts[1,]))break
    }
    
    
    warn<-warn[!is.na(warn)]
    
    
    if(length(warn)>0)
    {
    write.table(c(Samples[s-1],warn,"\n"),"WARNING.txt",sep="\t",append=TRUE,quote=FALSE,row.names = FALSE,col.names = FALSE)
    }
    
    
    warn<-NA
    l<-1
    s<-s+1
    
    if(s>length(AlleleCounts[,1]))break}
    
    

}


