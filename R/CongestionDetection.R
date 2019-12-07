Congestion_detection<-function(Segments){



  for (sts in Segments ){

    ad<-sprintf("%s%s%s%s%s", "../CP/",sts,"/",sts, ".csv")
    dataset<-read_csv(ad)
    rm(ad)
    dataset<-as.data.frame(dataset[,-1])
    dataset$cp<-0
    dataset$group<-"NA"

    n=1

    while (n < (nrow(dataset)-1)){
      n=n+1
      #print(n)
      if((n+1)==(nrow(dataset)+1)){

        break()}

      else if(!dataset$Prob[n]<0.01 ){

        for (j in (n+1):nrow(dataset)){

          #print(n)


          if (dataset$Prob[j]<0.01){

            d=max(dataset$Prob[n:j-1])
            #print(d)
            t<-dataset$Prob[n:j-1]==d
            #print(t[-1])
            dataset$cp[n:j-1][which(t==TRUE)]<-1


            break()

            #print(n)

          }

        }

        n<-j
      }

    }





    rm(d)
    rm(j)
    rm(n)
    rm(t)



    counter=1
    l=1
    for (i in 1:nrow(dataset)){

      if (dataset$cp[i]==1){

        dataset$group[l:i]<-counter
        #print(counter)
        counter=counter+1
        l=i+1
        #print(l)
      }
      # check<-dataset$cp[i+1:nrow(dataset)]==0
      # if (dataset$cp[i]==1 & unique(check)=="TRUE"){
      #
      #   dataset$group[i:nrow(dataset)]<-counter+1
      # }
      #
    }


    dataset$group[dataset$group=="NA"]<-counter
    rm(counter,i,l)
    #write.csv(dataset,"1485655352/result/grouping_test.csv")
    total=NULL
    group=unique(dataset$group)
    for (g in group){
      fil_data<-dataset%>%filter(group==g)%>%mutate(Q15=quantile(Speed,0.15), Q50=quantile(Speed,0.50),Q85=quantile(Speed,0.85))
      total<-rbind(total,fil_data)

    }

    rm(g,group,fil_data)
    #write.csv(total,"total.csv")

    clusters<-kmeans(total[,c('Q15','Q50','Q85')],2)

    d<-cbind(cluster=clusters$cluster,total)

    m<-d%>%group_by(cluster)%>%mutate(avg=mean(Speed))%>%group_by(cluster,avg)%>%summarize()
    m<-as.data.frame(m)
    clu<-m%>%filter(avg==min(avg))%>%select(cluster)#finding the cluster

    d$visual <- ifelse(d$cluster==clu$cluster, d$Speed, 0)
    rm(clusters,m)

    dataset<-d
    rm(d,total)


  }
  return(dataset)
}
