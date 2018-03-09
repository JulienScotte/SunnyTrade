
############## Base Function for Clustering ##############

Clustering<-function(ts,NbCluster,NbStart,NbSteps,NbCentile){
  
  #Cluster Params
  NbStart<-10 
  NbSteps<-10000
  
  GetCluster<-function(ts,nbInput,NbCluster){
    
    set.seed(888)
    dataCl<-kmeans(ts[,1:nbInput],centers=NbCluster,iter.max=NbSteps,nstart=NbStart)
    ts$cluster <- dataCl$cluster
    
    return(list(ts=ts,centers=dataCl$centers))
  }
  
  FindOptNbCluster<-function(ts){
    
    wss <- (nrow(ts)-1)*sum(apply(ts,2,var))
    for (i in 2:20) {
      wss[i] <- sum(kmeans(ts,centers=i)$withinss)
    }
    pl<-plot(1:20, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")
    
    return(pl)
  }
  
  ClusterStats<-function(NumClust,ts){
    
    filterts<-ts[ts$cluster==NumClust,]
    maxs <- max(filterts[,1])
    mins <- min(filterts[,1])
    sds <- sd(filterts[,1])
    means <- mean(filterts[,1])
    numPositive<-nrow(filterts[filterts[,1]>0,])/nrow(filterts)
    numNegative<-nrow(filterts[filterts[,1]<0,])/nrow(filterts)
    nbPoints<-nrow(filterts)
    p20 <- quantile(filterts[,1],NbCentile)
    p80 <- quantile(filterts[,1],1-NbCentile)
    
    return(c(maxs,mins,sds,means,numPositive,numNegative,nbPoints,p20,p80))
  }
  
  ClStats<-vector('double')
  ClStats<-as.data.frame(matrix(0,nrow=NbCluster,9))
  colnames(ClStats)<-c("maxs","mins","sds","means","numPositive","numNegative","nbPoints","p20","p80")
  
  sts<-ts[,2:ncol(ts)]
  nbInput<-ncol(sts)
  
  #Get Cluster / add centers serie to timeserie
  tsClust<-GetCluster(sts,nbInput,NbCluster)
  ts$cluster <- tsClust$ts$cluster
  
  for (i in 1:NbCluster){
    u<-ClusterStats(i,ts)
    ClStats[i,]<-u
  }
  
  return(list(ts,ClStats,tsClust$centers))
}


FindClosestCluster<-function(newdata,centers){
  
  temp<-vector('double')
  for (i in 1:nrow(centers)) {
    u<-0
    for (j in 1:ncol(centers)){
      u<-u+(newdata[j]-centers[i,j])^2
    }
    temp<-rbind(temp,sqrt(u))
  }
  n<-which.min(temp)
  
  return(n)
}

####################################################














