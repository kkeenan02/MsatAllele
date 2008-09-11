`read.ah.file` <-
function(ahfile,date,plate){

STRandOUT<-read.table(file=ahfile, header = FALSE, sep = "\t", quote="\"", dec=".",fill = TRUE)

data<-date
run<-plate

DataBase<-transDB(STRandOUT,data,run)


DataBase
}

