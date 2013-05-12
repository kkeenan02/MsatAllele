

getpoints<-function(Database,marker){

print("Click first on the lower and then on the higher range limits for the observations of interest")
loc<-locator(2)
table<-subdataBase(Database,marker,loc$y[1],loc$y[2])

table}

