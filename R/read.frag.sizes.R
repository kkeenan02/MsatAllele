

read.frag.sizes<-function (in.file, date, plate) 
{
    GMdata <- read.table(file = in.file, sep = "\t", header = TRUE)
    r <- 1
    if (is.na(GMdata[r, 4])) {
        r <- r + 1
    }
    else {
        if (GMdata[r, 4] == GMdata[r, 5]) {
            TAB <- data.frame(Marker = GMdata[r, 3], Sample = GMdata[r, 1], Fragment = GMdata[r, 4], Date = date, Plate = plate)
        }
        else {
            TAB <- rbind(data.frame(Marker = GMdata[r, 3], Sample = GMdata[r, 1], Fragment = GMdata[r, 4], Date = date, Plate = plate),  data.frame(Marker = GMdata[r, 3], Sample = GMdata[r, 1], Fragment = GMdata[r, 5], Date = date, Plate = plate))
        }
    }
    r <- r + 1
    repeat {
        if (!is.na(GMdata[r, 4])) {
            if (GMdata[r, 4] == GMdata[r, 5]) {
                Tab <- data.frame(Marker = GMdata[r, 3], Sample = GMdata[r,1], Fragment = GMdata[r, 4], Date = date, Plate = plate)
                TAB <- rbind(TAB, Tab)
            }
            else {
                Tab <- rbind(data.frame(Marker = GMdata[r, 3],  Sample = GMdata[r, 1], Fragment = GMdata[r, 4], Date = date, Plate = plate), data.frame(Marker = GMdata[r,3], Sample = GMdata[r, 1], Fragment = GMdata[r, 5], Date = date, Plate = plate))
                TAB <- rbind(TAB, Tab)
            }
        }
        r <- r + 1
        if (r > length(GMdata[, 1])) 
            break
    }
    TAB
}

