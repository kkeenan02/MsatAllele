

subdataBase<-function (DataBase, marker, MIN=min(DataBase[,3]), MAX=max(DataBase[,3])) 
{
    ShortDB <- DataBase[(DataBase == marker), ]
    c1 <- ShortDB[, 3] > MIN & ShortDB[, 3] < MAX
    rnames <- row.names(ShortDB[c1, ])
    o <- order(ShortDB[c1, 3])
    dataV <- ShortDB[c1, 3]
    dataN <- ShortDB[c1, 2]
    dataGel <- ShortDB[c1, 5]
    dataVo <- dataV[o]
    dataNo <- dataN[o]
    dataGo <- dataGel[o]
    rnameso <- rnames[o]
    subData <- data.frame(DFrow = as.numeric(rnameso), Sample = as.character(dataNo), 
        Reading = dataVo, Gel = dataGo)
    subData
}

