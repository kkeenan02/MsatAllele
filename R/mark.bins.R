

mark.bins<-function(bins,text.size=1,yscale=5,offtext=0.5){


i<-1
repeat{
lines(c(bins$limits[i],bins$limits[i]),c(1,yscale),col="red",)
i<-i+1
if(i>length(bins$limits))break
}


text(bins$bin,rep(yscale,length(bins$bin)),labels=bins$bin,srt=90,pos=4,cex=text.size,offset=offtext)




}

