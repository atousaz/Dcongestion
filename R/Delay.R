delay=function(x,len){
  
  traveltime<-x[1]
  ref<-x[2]
  dif<-(traveltime/60)-(rep(len,times=nrow(traveltime))/ref)
  arg_dif<-apply(dif,1,function(x){max(x,0)})
  arg_dif<-as.data.frame(arg_dif)
  sm<-sum(arg_dif)
  ddd<-sm/60
  return(ddd)
}
