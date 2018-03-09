
LaunchBacktest<-function(MktList,NbCluster,Thresh,NbPMin,StartRow,SDteStat,Scaling,NbCentile){

  setwd("C:/Users/i885564/Documents/Model/K-Means")
  
  library(readxl)
  library("MASS")
  library("ggplot2")
  library("munsell")
  library("labeling")
  library("devtools")
  library("plot3D")
  library("NbClust")
  library("lubridate")
  
  source("Clustering.R")
  source("PerfAnalytics_Func.R")
  
  Pricepath<-"C:/Users/i885564/Documents/Model/K-Means/PriceSeries.xlsx" 
  FondaPath<-"C:/Users/i885564/Documents/Model/K-Means/FondaSeries.xlsx"
  
  sdte<-as.Date(SDteStat)
  srow<-year(sdte)*10000+month(sdte)*100+day(sdte)
  
  ############################ Data Series Retreatment ############################
  
  AllPricets<-list()
  AllFondats<-list()
  for (i in 1:length(MktList)){
    MktName = MktList[i]
    
    Pricets<-as.data.frame(read_excel(Pricepath,MktName,col_names=TRUE))
    Pricets<-data.frame(Pricets[,-1],row.names=Pricets[,1])
    
    Fondats<-as.data.frame(read_excel(FondaPath,MktName,col_names=TRUE))
    tmp<-matrix(0,nrow=nrow(Fondats)-1,ncol(Fondats))
    
    for (j in 2:nrow(Fondats)){
      tmp[j-1,1]=Fondats[j,1]
      for (k in 2:ncol(Fondats)){
        tmp[j-1,k]=Fondats[j,k]/Fondats[j-1,k]
        # tmp[j-1,2]=Fondats[j,2]/Fondats[j-1,2]
        # tmp[j-1,3]=Fondats[j,3]/Fondats[j-1,3]
      }
    }
    Fondats<-tmp
    
    AllPricets[[i]]<-Pricets
    AllFondats[[i]]<-Fondats
  }
  
  tmp<-vector('double')
  MonthPricets<-list()
  for (i in 1:length(MktList)){
    
    tmp<-matrix(0,nrow=nrow(Fondats),1)
    for (j in 1:nrow(Fondats)-1){
      dte=as.character(AllFondats[[i]][j+1,1])
      dtePrev=as.character(AllFondats[[i]][j,1])
      tmp[j,1]=AllPricets[[i]][dte,"DClose"]/AllPricets[[i]][dtePrev,"DClose"]-1
    }
    
    MonthPricets[[i]]<-tmp
  }
  
  ##################### Rolllllinnnnnnnnnnng Clustering #######################
  
  stats<-list()
  NewFondats<-list()
  Clts<-data.frame(matrix(0,nrow=((nrow(Fondats)-2)*length(MktList)),ncol=ncol(Fondats)))
  colnames(Clts)<-c("P","F1","F2")
  
  pb<-winProgressBar(title="Computing Cluster",label="0% done",min=0,max=100,initial=0)
  
  for (i in StartRow:nrow(Fondats)){
    f<-vector('double')
    Clts<-data.frame(matrix(0,nrow=((i-2)*length(MktList)),ncol=ncol(Fondats)))
    
    for (j in 1:length(MktList)){
      
      tmp<-matrix(0,nrow=i,2:ncol(Fondats))
      
      #Scaling for Cluster
      tmp<-data.frame(AllFondats[[j]][1:i-1,2:ncol(Fondats)])
      if (Scaling == "MinMax") {
        maxs <- sapply(tmp[,1:ncol(tmp)], max)
        mins <- sapply(tmp[,1:ncol(tmp)], min)
        scaled <- as.data.frame(scale(tmp[,1:ncol(tmp)], center = mins, scale = maxs-mins))
        tmp<-scaled
      }
      
      Clts[((i-1)*(j-1)+1):((i-1)*j),1]=MonthPricets[[j]][1:i-1]
      Clts[((i-1)*(j-1)+1):((i-1)*j),2:ncol(Fondats)]=tmp[1:i-1,]
      
      stats[[i]]<-Clustering(Clts,NbCluster,10,10,NbCentile)
      # f<-rbind(f,tmp[i,])
      
      tp<-vector('double')
      tp<-AllFondats[[j]][i,2:ncol(Fondats)]
      if (Scaling =="MinMax"){
        for (k in 1:length(tp))
          tp[k]<-max(0,min(1,(tp[k]-mins[k])/(maxs[k]-mins[k])))
      }
      
      f<-rbind(f,tp)
    }
    NewFondats[[i]]<-f
    
    
    info<-sprintf("%d%% done",round((i-StartRow)/(nrow(Fondats)-StartRow)*100))
    setWinProgressBar(pb,(i-StartRow)/(nrow(Fondats)-StartRow)*100,label=info)
    
  }
  
  close(pb)
  
  ################## Searching for closest Cluster and Pos ####################
  
  Pos<-vector('double')
  Pos<-matrix(0,nrow=nrow(Fondats),length(MktList)+1)
  Stp<-vector('double')
  Stp<-matrix(0,nrow=nrow(Fondats),length(MktList)+1)
  
  k=1
  for (k in 1:length(MktList)){
    
    Fondats<-AllFondats[[k]]
    
    for (i in (StartRow):(nrow(Fondats))){
      newdata1<-vector('double')
      newdata1<-NewFondats[[i]][k,]
      
      NumClust=FindClosestCluster(newdata1,data.frame(stats[[i]][3]))
      u<-data.frame(stats[[i]][2])
      
      if (u$nbPoints[NumClust]>NbPMin){
        if(u$numPositive[NumClust]/u$numNegative[NumClust]>(1+Thresh)){
          Pos[i,k+1]<-1
          Stp[i,k+1]<-u$p20[NumClust]
        }else if(u$numPositive[NumClust]/u$numNegative[NumClust]<(1-Thresh)){
          Pos[i,k+1]<-(-1)
          Stp[i,k+1]<-u$p80[NumClust]
        }
      }
      Pos[i,1]=Fondats[i,1]
    }
  }
  
  ##################### Apply Pos + Eq Curve Stats #########################
  
  pb<-winProgressBar(title="Computing EqCurve",label="0% done",min=0,max=100,initial=0)
  
  EqC<-vector('double')
  DPos<-vector('double')
  DStp<-vector('double')
  Perf<-vector('double')
  
  EqC<-matrix(0,nrow=nrow(AllPricets[[1]]),length(MktList)+2)
  DPos<-matrix(0,nrow=nrow(AllPricets[[1]]),length(MktList)+1)
  DStp<-matrix(0,nrow=nrow(AllPricets[[1]]),length(MktList)+1)
  Perf<-matrix(0,nrow=length(MktList)+1,3)
  
  for (k in 1:length(MktList)){
    
    tmpEqC<-matrix(0,nrow=nrow(AllPricets[[k]]),1)
    
    dte<-data.frame(row.names(AllPricets[[k]]))
    tmpPos<-0
    tmpStp<-0
    EqC[1,k+1]<-0
    
    for (i in 2:nrow(EqC)){
      
      tmpdte=which(Pos[,1]==as.character(dte[i,1]),arr.ind = TRUE)
      
      if(length(tmpdte)>0){
        EqC[i,k+1]<-EqC[i-1,k+1]+tmpPos*(AllPricets[[k]][i,"DClose"]/AllPricets[[k]][i-1,"DClose"]-1)
        
        tmpPos=as.double(Pos[tmpdte,k+1])
        DPos[i,k+1]<-tmpPos
        
        tmpStp=as.double((1+Stp[tmpdte,k+1])*AllPricets[[k]][i,"DClose"])
        DStp[i,k+1]<-tmpStp
        
      }else{
        EqC[i,k+1]<-EqC[i-1,k+1]+tmpPos*(AllPricets[[k]][i,"DClose"]/AllPricets[[k]][i-1,"DClose"]-1)
        
        DPos[i,k+1]<-tmpPos
        DStp[i,k+1]<-tmpStp
        
        if(tmpPos>0){
          if(AllPricets[[k]][i,"DClose"]<tmpStp){
            tmpPos<-0
          }
        }else if(tmpPos<0){
          if(AllPricets[[k]][i,"DClose"]>tmpStp){
            tmpPos<-0
          }
        }
        
      }
      EqC[i,length(MktList)+2]<-EqC[i,length(MktList)+2]+EqC[i,k+1]/length(MktList)
      
    }
    
    linedte=which(srow==dte,arr.ind = FALSE)
    
    tmpEqC<-EqC[,k+1]
    Perf[k,1]<-SharpeRatio(tmpEqC,linedte,length(tmpEqC))
    Perf[k,2]<-SortinoRatio(tmpEqC,linedte,length(tmpEqC))
    Perf[k,3]<-SterlingRatio(tmpEqC,linedte,length(tmpEqC))
    
    info<-sprintf("%d%% done",round(k/length(MktList)*100))
    setWinProgressBar(pb,k/length(MktList)*100,label=info)
    
  }
  
  DStp<-data.frame(DStp[,-1],row.names=rownames(AllPricets[[1]]))
  DPos<-data.frame(DPos[,-1],row.names=rownames(AllPricets[[1]]))
  
  #Total EqCurve Perf
  tmpEqC<-EqC[,length(MktList)+2]
  Perf[length(MktList)+1,1]<-SharpeRatio(tmpEqC,linedte,length(tmpEqC))
  Perf[length(MktList)+1,2]<-SortinoRatio(tmpEqC,linedte,length(tmpEqC))
  Perf[length(MktList)+1,3]<-SterlingRatio(tmpEqC,linedte,length(tmpEqC))
  
  close(pb)
  
  colnames(Perf)<-c("Sharpe","Sortino","Sterling")
  
  Oput<-list()
  Oput<-list(EqC,DPos,DStp,AllPricets,Perf,dte)
  
  return(Oput)
  # return(EqC[,8])

}





