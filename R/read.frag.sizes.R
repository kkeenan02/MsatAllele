#reads fragment data from other formats than .ah (STrand)
 read.frag.sizes<-function (in.file, date, plate) {
   
 GMdata <- read.table(file = in.file, sep = "\t", header = TRUE)

nr<-nrow(GMdata)
TAB<-NULL



for(r in 1:nr){

    if (is.na(GMdata[r, 4])) {
        r <- r + 1
    }
    else {
        if (GMdata[r, 4] == GMdata[r, 5]) {
            Tab <- data.frame(Marker = GMdata[r, 3], 
                              Sample = GMdata[r,1],
                              Fragment = GMdata[r, 4],
                              Date = date,
                              Plate = plate)
        }
        else {
            Tab <- rbind(data.frame(Marker = GMdata[r, 3],
                                     Sample = GMdata[r,1], 
                                     Fragment = GMdata[r, 4], 
                                     Date = date,
                                     Plate = plate), 
                         data.frame(Marker = GMdata[r, 3],
                                    Sample = GMdata[r,1],
                                    Fragment = GMdata[r, 5], 
                                    Date = date,
                                    Plate = plate)
                         )
        }
        r <- r + 1
    }
if(exists("Tab")){  TAB <- rbind(TAB, Tab)
		     rm(Tab)
                  }

  }
  TAB
}




