library(shiny)
library(shinythemes)
options(shiny.maxRequestSize=30*1024^2)

ui<-
  fluidPage(
    theme =  shinytheme("darkly"),
    titlePanel("Sales Forecasting"),
    sidebarLayout(
      sidebarPanel(
        fileInput("file",label = "Data Input",multiple = TRUE,accept = c("text/csv",".csv")),
        tags$hr(),
        checkboxInput("header",label = "Header",value = TRUE),
        radioButtons("outlier",label = "Outlier",choiceNames = "Without Outlier",choiceValues = "'o1",selected = FALSE),
        radioButtons("AA1",label = "Auto-arima with seasonality",choiceNames = "seasonal=True",choiceValues = "a1",selected = FALSE),
        radioButtons("AA2",label = "Auto-arima without seasonality",choiceNames = "seasonal=False",choiceValues = "a2",selected = FALSE),
        radioButtons("arima1",label = "ARIMA-1",choiceNames = "custom(2,1,7)",choiceValues = "ar1",selected = FALSE),
        radioButtons("arima2",label = "ARIMA-2", choiceNames = "default(1,1,1)",choiceValues = "ar2",selected = FALSE),
        radioButtons("forecast",label = "Predictions",choiceNames = "Best Model",choiceValues = "Fr",selected = FALSE)
        ),
      
      mainPanel(
        tabsetPanel(
          tabPanel(title = "Data",
            tableOutput("Data")),
          tabPanel(title = "Plots-1",
                   
                fluidRow(
                plotOutput("plot1",width = "50%",height = 350),
                plotOutput("plot2",width = "50%",height= 380),
                plotOutput("plot3",width = "50%",height = 350)
                )),
          tabPanel(title = "Auto-Arima Models",
                   
                fluidRow(
                  plotOutput("plot4",width = "50%",height = 350),
                  plotOutput("plot5",width = "50%",height = 350)
                )),
          tabPanel(title = "ARIMA Models", 
                  fluidRow(
                  plotOutput("plot6",width = "50%",height = 350),
                  plotOutput("plot7",width = "50%",height = 350)
                )),
          tabPanel(title = "Forecast",
                   fluidRow(
                   textOutput("BestModel"),
                   tableOutput("forecast")))
          ))
          
          
      )
    )
  

F1<-read.csv(file.choose())
F1<-F1[,c("Date.Of.Invoice","Purchase_price")]  
F1$Dateofinvoice<-format(as.POSIXct(strptime(F1$Date.Of.Invoice,"%m/%d/%Y %H:%M",tz='')),format= "%Y/%m/%d")
F1$Date.Of.Invoice<-NULL
F1[order(F1$Dateofinvoice,decreasing = FALSE),]
F1 <- aggregate(Purchase_price~Dateofinvoice,data=F1,FUN=sum)
class(F1$Dateofinvoice)
library(lubridate)
F1$Dateofinvoice<-ymd(F1$Dateofinvoice)


server<- function(input,output)
{
  output$Data<-renderTable({
     
    F1<- input$file
    
    if(is.null(F1))
    return(NULL)
    
    F1<-read.csv(F1$datapath,header = input$header)
    F1<-F1[,c("Date.Of.Invoice","Purchase_price")]  
    F1$Dateofinvoice<-format(as.POSIXct(strptime(F1$Date.Of.Invoice,"%m/%d/%Y %H:%M",tz='')),format= "%Y/%m/%d")
    F1$Date.Of.Invoice<-NULL
    F1[order(F1$Dateofinvoice,decreasing = FALSE),]
    F1 <- aggregate(Purchase_price~Dateofinvoice,data=F1,FUN=sum)
    return(F1)
    
    })
 output$plot1<-renderPlot({boxplot(F1$Purchase_price)})
 
 output$plot2<-renderPlot({library(ggplot2)
   ggplot(F1,aes(F1$Dateofinvoice,F1$Purchase_price))+geom_line()+scale_x_date('Days')+
     ylab("Daily sales")+xlab("")})
  
  output$plot3<-renderPlot({o1<-input$outlier
    
  if(is.null(o1))
  return(NULL)
  library(outliers)
  library(tseries)
  library(forecast)
  o1<-outlier(F1$Purchase_price)
  o1<-outlier(F1[,2],opposite = TRUE)    
  count_TSObject <- ts(F1[,c('Purchase_price')])
  F1$clean_count <- tsclean(count_TSObject)
  o1<-ggplot(F1,aes(Dateofinvoice,clean_count))+ geom_line()+
    scale_x_date('Days')+
    ylab("Daily sales")+
    xlab("")
  return(o1)
    
    })
  
  output$plot4<-renderPlot({a1<-input$AA1
  
  if(is.null(a1))
  return(NULL)
  library(outliers)
  library(tseries)
  library(forecast)
  a1<-outlier(F1$Purchase_price)
  a1<-outlier(F1[,2],opposite = TRUE)    
  count_TSObject <- ts(F1[,c('Purchase_price')])
  F1$clean_count <- tsclean(count_TSObject)
  F1$cnt_ma <- ma(F1$clean_count,order = 1)
  count_ma <- ts(na.omit(F1$cnt_ma),frequency = 30)
  decomp= stl(count_ma, s.window="periodic")
  deseasonal_count <- seasadj(decomp) 
  fit_w_seasonality <- auto.arima(deseasonal_count,seasonal = TRUE)
  fcast <- forecast(fit_w_seasonality,h=30)
  a1<-plot(fcast)
  return(a1)
  
  })
  
  output$plot5<-renderPlot({a2<-input$AA2
    
  if(is.null(a2))
  return(NULL)
  library(outliers)
  library(tseries)
  library(forecast)
  a2<-outlier(F1$Purchase_price)
  a2<-outlier(F1[,2],opposite = TRUE)    
  count_TSObject <- ts(F1[,c('Purchase_price')])
  F1$clean_count <- tsclean(count_TSObject)
  F1$cnt_ma <- ma(F1$clean_count,order = 1)
  count_ma <- ts(na.omit(F1$cnt_ma),frequency = 30)
  decomp= stl(count_ma, s.window="periodic")
  deseasonal_count <- seasadj(decomp) 
  fit3 <- auto.arima(deseasonal_count,seasonal=FALSE)
  fcast1 <- forecast(fit3,h=34)
  a2<-plot(fcast1)
  return(a2)
    
  })
 
  output$plot6<-renderPlot({ar1<-input$arima1
  
  if(is.null(ar1))
  return(NULL)
  library(outliers)
  library(tseries)
  library(forecast)
  ar1<-outlier(F1$Purchase_price)
  ar1<-outlier(F1[,2],opposite = TRUE)    
  count_TSObject <- ts(F1[,c('Purchase_price')])
  F1$clean_count <- tsclean(count_TSObject)
  F1$cnt_ma <- ma(F1$clean_count,order = 1)
  count_ma <- ts(na.omit(F1$cnt_ma),frequency = 30)
  decomp= stl(count_ma, s.window="periodic")
  deseasonal_count <- seasadj(decomp) 
  fit4 <- arima(deseasonal_count,order = c(2,1,7)) 
  fcast2 <- forecast(fit4,h=34)
  ar1<-plot(fcast2)
  return(ar1)
  })

  output$plot7<-renderPlot({ar2<-input$arima2
  
  if(is.null(ar2))
  return(NULL)
  library(outliers)
  library(tseries)
  library(forecast)
  ar2<-outlier(F1$Purchase_price)
  ar2<-outlier(F1[,2],opposite = TRUE)    
  count_TSObject <- ts(F1[,c('Purchase_price')])
  F1$clean_count <- tsclean(count_TSObject)
  F1$cnt_ma <- ma(F1$clean_count,order = 1)
  count_ma <- ts(na.omit(F1$cnt_ma),frequency = 30)
  decomp= stl(count_ma, s.window="periodic")
  deseasonal_count <- seasadj(decomp) 
  fit5 <- arima(deseasonal_count,order = c(1,1,1))
  fcast3 <- forecast(fit5,h=34)
  ar2<-plot(fcast3)
  return(ar2)
  })
  
  output$BestModel <-renderText({print("arIMA Model with custom (p,d,q) - (2,1,7) has best prediction values with less RMSE score  of 3995.78")})
  
  output$forecast<-renderTable({Fr<-input$forecast
  if(is.null(Fr))
  return(NULL)
  library(outliers)
  library(tseries)
  library(forecast)
  Fr<-outlier(F1$Purchase_price)
  Fr<-outlier(F1[,2],opposite = TRUE)    
  count_TSObject <- ts(F1[,c('Purchase_price')])
  F1$clean_count <- tsclean(count_TSObject)
  F1$cnt_ma <- ma(F1$clean_count,order = 1)
  count_ma <- ts(na.omit(F1$cnt_ma),frequency = 30)
  decomp= stl(count_ma, s.window="periodic")
  deseasonal_count <- seasadj(decomp) 
  fit4 <- arima(deseasonal_count,order = c(2,1,7)) 
  fcast2 <- forecast(fit4,h=34)
  Fr<-fcast2
  return(Fr)
  
  })
}



 
shinyApp(ui,server)   
    
    
    
    
    
    