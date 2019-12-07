
#' Clean text and build term matrix for bag of words model or TF DFI.
#'
#' @param dataset unbalanced dataset, a dataframe : two column: first text reviews and second binary class, label: negative =0 and positive=1.
#' @return balanced_dataframe balanced dataframe containing two columns: review texts and binary class , label: negative =0 and positive=1.
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

  traveltime<-x[1]
  ref<-x[2]
  dif<-(traveltime/60)-(rep(len,times=nrow(traveltime))/ref)
  arg_dif<-apply(dif,1,function(x){max(x,0)})
  arg_dif<-as.data.frame(arg_dif)
  sm<-sum(arg_dif)
  ddd<-sm/60
  return(ddd)
}
