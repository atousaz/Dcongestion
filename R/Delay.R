#' Delay calculation.
#'
#' @param dataset including travel time, reference speed and actual speed
#' @return Delay value
#' @author Atousa Zarindast

#' @export
#' @return A balanced dataframe
#' @examples
#' \dontrun{
#' library("SentiAnalyzer")
#' direction <- system.file(package = "SentiAnalyzer", "extdata/Imbalance_Restaurant_Reviews.tsv")
#' imbalance_data<- read.delim(direction,quote='',stringsAsFactors = FALSE)
#' BalanceData(imbalance_data)}

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
