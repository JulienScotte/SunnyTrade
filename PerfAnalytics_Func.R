
SharpeRatio<-function(EqC,StartRow,EndRow){
  
  tmpEqC<-vector('double')
  lagtmpEqC<-vector('double')
  DEqc<-vector('double')
  
  tmpEqC<-matrix(0,nrow=EndRow-StartRow+1,ncol=1)
  lagtmpEqC<-matrix(0,nrow=EndRow-StartRow+1,ncol=1)
  
  tmpEqC<-EqC[StartRow:EndRow]
  lagtmpEqC<-EqC[(StartRow-1):(EndRow-1)]
  
  DEqc<-tmpEqC[]-lagtmpEqC[]
  
  Std<-sd(DEqc)
  Avg<-mean(DEqc)
  
  SR<-Avg*sqrt(255)/Std
  
  return(SR)
  
}

SortinoRatio<-function(EqC,StartRow,EndRow){
  
  tmpEqC<-vector('double')
  lagtmpEqC<-vector('double')
  DEqc<-vector('double')
  
  tmpEqC<-matrix(0,nrow=EndRow-StartRow+1,ncol=1)
  lagtmpEqC<-matrix(0,nrow=EndRow-StartRow+1,ncol=1)
  
  tmpEqC<-EqC[StartRow:EndRow]
  lagtmpEqC<-EqC[(StartRow-1):(EndRow-1)]
  
  DEqc<-tmpEqC[]-lagtmpEqC[]
  
  NegRet<-DEqc[DEqc[]<0]
  
  Std<-sd(NegRet)
  Avg<-mean(DEqc)
  
  SR<-Avg*sqrt(255)/Std
  
  return(SR)
  
}

Drawdown <- function(EqC) {
  
  EqC.pnl  <- c(0, cumsum(EqC))
  Drawdown <- EqC.pnl - cummax(EqC.pnl)
  
  return(tail(Drawdown, -1))
  
}
  
SterlingRatio<-function(EqC,StartRow,EndRow){
  
  tmpEqC<-vector('double')
  lagtmpEqC<-vector('double')
  DEqc<-vector('double')
  tmp<-vector('double')
  DDD<-vector('double')
  
  tmpEqC<-matrix(0,nrow=EndRow-StartRow+1,ncol=1)
  lagtmpEqC<-matrix(0,nrow=EndRow-StartRow+1,ncol=1)
  
  tmpEqC<-EqC[StartRow:EndRow]
  lagtmpEqC<-EqC[(StartRow-1):(EndRow-1)]
  
  DEqc<-tmpEqC[]-lagtmpEqC[]
  
  DDD<-Drawdown(DEqc)
  MDD<-min(DDD)
  
  Avg<-mean(DEqc)
  
  SR<-(-Avg*sqrt(255)/MDD)
  
  return(SR)
  
}


  







