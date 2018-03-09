
library(quantmod)
library(dygraphs)
source("BackTesting_Func.R")

server <- function(input,output,session) {
  
  # MktList<-c("Corn","Wheat","KWheat","Soybean","SMeal","BOil")
  OutputList<-list()
  
  observe({
      if(input$selectallMkt == 0) return(NULL)
      else if (input$selectallMkt%%2 == 0){
          updateCheckboxGroupInput(session,"markets","choose markets",choices=MktList)
      }
      else{
          updateCheckboxGroupInput(session,"markets","choose markets",choices=MktList,selected=MktList)
      }

      if(input$selectallAlgo == 0) return(NULL)
      else if (input$selectallAlgo%%2 == 0){
          updateCheckboxGroupInput(session,"Algo","choose Algo",choices=AlgoList)
      }
      else{
          updateCheckboxGroupInput(session,"Algo","choose Algo",choices=AlgoList,selected=AlgoList)
      }
  })
  
  observeEvent(input$LaunchAlgo,{
    OutputList<-LaunchBacktest(MktList,input$nb,input$thresh,input$nbPmin,50,input$daterange[1],"MinMax",input$NbCentile)
    
    observeEvent(input$DispChart,{
      output$plot1<-renderDygraph({
        
        # Find which market to Display   renderPlot
        tmpcol<-as.integer(which(c(MktList,"All")==input$MktDisp)+1)
        
        tEqc<-data.frame(OutputList[[1]][,tmpcol])
        st<-OutputList[[3]][tmpcol-1]
        sdt<-rownames(st)
        rownames(tEqc)<-as.Date(sdt, format = '%Y%m%d')

        #Find the line of the start date
        sdte<-as.Date(input$daterange[1])
        srow<-year(sdte)*10000+month(sdte)*100+day(sdte)
        linedte=which(srow==OutputList[[6]],arr.ind = FALSE)
        
        #Choose which to plot
        if (input$MktDisp != "All"){
          ts<-data.frame(OutputList[[4]][[tmpcol-1]][,1:4])
          dt<-rownames(ts)
          rownames(ts)<-as.Date(dt, format = '%Y%m%d')
        }
        
        # return(dygraph(ts) %>%
        #   dyRangeSelector())
        
        if((input$plot_select==2)&(input$addStp==TRUE)){
          # st<-OutputList[[3]][tmpcol-1]
          # sdt<-rownames(st)
          # rownames(st)<-as.Date(sdt, format = '%Y%m%d')
          # lines(st[linedte:nrow(st),],col="red")
          ts<-cbind(ts,OutputList[[3]][tmpcol-1])
        }
        
        plot_func<-switch(input$plot_select,
              "1" = "dygraph(tEqc) %>% dyRangeSelector()",
              "2" = "dygraph(ts) %>% dyCandlestick() %>% dyRangeSelector()")


              # "1" = "plot(OutputList[[1]][linedte:nrow(OutputList[[1]]),tmpcol],type='l',ylab='Equity Curve',xlab=input$MktDisp)",
              # "2" = "barChart(ts[linedte:nrow(ts),],theme = chartTheme('white'),ylab='Price Curve',xlab=input$MktDisp)")
              # "2" = "plot(OutputList[[4]][[tmpcol-1]]$DClose[linedte:nrow(OutputList[[4]][[tmpcol-1]])],type='l',ylab='Price Curve',xlab=input$MktDisp)")
        
        return(eval(parse(text = paste0(plot_func))))

        #Output Stats
        output$Perf<-renderTable({
          
          OutStat<-data.frame(matrix(0,nrow=3,ncol=2))
          OutStat[,1]<-c("Sharpe","Sortino","Sterling")
          
          OutStat[1,2]<-SharpeRatio(OutputList[[1]][1:nrow(OutputList[[1]]),tmpcol],linedte,nrow(OutputList[[1]]))
          OutStat[2,2]<-SortinoRatio(OutputList[[1]][1:nrow(OutputList[[1]]),tmpcol],linedte,nrow(OutputList[[1]]))
          OutStat[3,2]<-SterlingRatio(OutputList[[1]][1:nrow(OutputList[[1]]),tmpcol],linedte,nrow(OutputList[[1]]))
          
          colnames(OutStat)<-c("Ratios","")
          OutStat
          
          })
        
      })
    })
    
    
  })
  
}






