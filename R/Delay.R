#'@title Delay calculation.
#'
#' @param dataset including travel time, reference speed and actual speed
#' @return Delay value
#' @author Atousa Zarindast

#' @export
#' @return Calculated delay value
#' @examples
#' \dontrun{
#' library("dcongestion")
#' visual <- load(package = "Dcongestion", "visual")
#' delay(visual)}

delay=function(x,len){
  x=visual
  traveltime<-x[8]
  ref<-x[10]
  dif<-(traveltime/60)-(rep(len,times=length(traveltime))/ref)
  arg_dif<-apply(dif,1,function(x){max(x,0)})
  arg_dif<-as.data.frame(arg_dif)
  sm<-sum(arg_dif)
  ddd<-sm/60
  return(ddd)
}
