elbow_viusal<-function(visual){

  timespan=unique(as.character(visual$time))

  r=NA
  for (t in timespan){
    time_filter<-visual%>%filter(as.character(time)==t)
    con_filter<-visual%>%filter(is.na(visual)==FALSE & as.character(time)==t)
    percentage<-cbind(con_percentage=nrow(con_filter)/nrow(time_filter),time=unique(t),count=nrow(con_filter))
    percentage<-as.data.frame(percentage)
    r<-rbind(r,percentage)}
  r<-as.data.frame(r)
  r<-r[-1,]

  r$con_percentage<-as.numeric(as.character(r$con_percentage))
  r$time1 <-as.POSIXct(as.character(r$time),format="%H:%M:%S")
  return(r)


}
