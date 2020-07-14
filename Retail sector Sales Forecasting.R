##### Importing data#######

#Train <- read.csv("C:/Users/admin/Desktop/Zeehan/Train.csv")

Train <- read.csv(file.choose())
View(Train)
retail <- Train  
View(retail)



#changing date format and removing time stamp
Dateofinvoice<-format(as.POSIXct(strptime(retail$Date.Of.Invoice,"%m/%d/%Y %H:%M",tz='')),format= "%Y/%m/%d")
retail$Dateofinvoice<-Dateofinvoice

#Removing previous Date.of.Invoice column
retail$Date.Of.Invoice<-NULL

#arranging column by date in increasing order
retail[order(retail$Dateofinvoice,decreasing = FALSE),]

View(retail)

retail1 <- retail[,c("Dateofinvoice","Product_Quantity","Purchase_price")]
View(retail1)
str(retail1)
summary(retail1)

###converting negative values to positive in Product_Quantity

retail1$Product_Quantity <- abs(retail1$Product_Quantity) 
View(retail1)




###### order of day to day ####


retail2 <- retail1[,c(1,3)]
View(retail2)
retail2 <- aggregate(Purchase_price~Dateofinvoice,data=retail1,FUN=sum)
View(retail2)
#as.Date.character(retail2$Dateofinvoice)
summary(retail2)
class(retail2$Dateofinvoice)

#converting character class to date class for date of invoice
library('lubridate')
retail2$Dateofinvoice <- ymd(retail2$Dateofinvoice)
View(retail2)
class(retail2$Dateofinvoice)

write.csv(retail2,file = "Training set.csv")


###### EDA for Forcasting ###########
library('ggplot2')
library('forecast')
library('tseries')


ggplot(retail2,aes(Dateofinvoice,Purchase_price))+ geom_line()+scale_x_date('Days')+
  ylab("Daily sales")+
  xlab("")


boxplot(retail2$Purchase_price)


plot.ts(retail1$Product_Quantity)
plot(retail2)

hist(retail2$Purchase_price)

###Identifying ouliers

library('car')
boxplot(retail2)
boxplot.stats(retail2[,2])$out
boxplot.stats(retail1[,3])$out
boxplot.stats(retail1[,2])$out



library('outliers')
outlier(retail2$Purchase_price)
outlier(retail2[,2],opposite = TRUE)


#Computing outliers  ##
library('ggplot2')
library('forecast')
library('tidyverse')
library('tseries')
library('rio')

#plot sales over time
ggplot(retail2,aes(Dateofinvoice,Purchase_price))+ geom_line()+
  scale_x_date('Days')+
  ylab("Daily sales")+
  xlab("")

ggplot(retail2,aes(Dateofinvoice,Purchase_price))+geom_point(color="navyblue")+
  scale_x_date('month')+ylab("Daily Sales")+xlab("")

##create a time series object based on purchase to pass to tsclean
count_TSObject <- ts(retail2[,c('Purchase_price')])

##tsclean function to id and replace any outliers
retail2$clean_count <- tsclean(count_TSObject)




### graph of sales with cleaned data =no outliers
ggplot()+geom_line(data=retail2,aes(x=Dateofinvoice,y=clean_count))+
  ylab('cleaned count')

ggplot(retail2,aes(Dateofinvoice,clean_count))+ geom_line()+
  scale_x_date('Days')+
  ylab("Daily sales")+
  xlab("")

##getting moving average 

retail2$cnt_ma <- ma(retail2$clean_count,order = 1)
retail2$cnt_ma30 <- ma(retail2$clean_count,order = 30)
ggplot()+
  geom_line(data=retail2,aes(x=Dateofinvoice,y=clean_count,color ="Counts"))+
  geom_line(data=retail2,aes(x=Dateofinvoice,y=cnt_ma,color ="daily Moving Average"))+
  geom_line(data=retail2,aes(x=Dateofinvoice,y=cnt_ma30,color ="Monthly Moving Average"))+
  ylab("Purchase")

##########################################################
### Decomposition of data taking trend, seasonality into account#####
 
count_ma <- ts(na.omit(retail2$cnt_ma),frequency = 30)
decomp= stl(count_ma, s.window="periodic")
deseasonal_count <- seasadj(decomp) 
plot(decomp)

##Test for stationarity-1st visual check for stationary variance
#2nd- Augmented Dicky Fuler test
adf.test(count_ma, alternative = "stationary")  ##p_value < 0.05 indicates TS is stationary
kpss.test(count_ma)
##############################################

###AUTOCORRELATION AND CHOOSING MODEL ORDER#####
#ACF plot shows correlation between a series and its lags 
Acf(count_ma,main='')

#PACF displays correlation between a series and its lags explained by previous lags
Pacf(count_ma,main='')

#difference of 1 is sufficient
count_d1 <- diff(deseasonal_count, differences = 1)
plot(count_d1)




adf.test(count_d1, alternative = "stationary")

#Look for spikes at specific lag points of the differenced series
Acf(count_d1,main='ACF for differenced series')
Pacf(count_d1,main='PACF for differenced series')

#### FITTING AN ARIMA MODEL ####
#get auto fit p,d,q values
auto.arima(deseasonal_count, seasonal = FALSE)

##EVALUATE AND ITERATE -does the model make sense
fit <- auto.arima(deseasonal_count, seasonal = FALSE)
tsdisplay(residuals(fit),lag.max = 20,main = '1,1,1 Model Residuals')

#Graph shows serious lag at 4, so modifying model for por q =4 
fit2 <- arima(deseasonal_count,order=c(1,1,7))
tsdisplay(residuals(fit),lag.max = 15,main = 'seasonal Model residuals')

#Forcast new fit model h=7 (7 days)
fcast <- forecast(fit2,h=10)
plot(fcast)
##############################################

#############################################
###TEST MODEL PERFORMANCE WITH A HOLDOUT SET#####

hold <- window(ts(deseasonal_count),start=60)
fit_no_holdout = arima(ts(deseasonal_count[-c(60:73)]),order = c(2,1,7))
fcast_no_holout <- forecast(fit_no_holdout,h=13)
plot(fcast_no_holout,main= "")
lines(ts(deseasonal_count))

#Model needs seasonality added back into it
fit_w_seasonality <- auto.arima(deseasonal_count,seasonal = TRUE)
seas_fcast <- forecast(fit_w_seasonality,h=7)
plot(seas_fcast)

lines(ts(count_ma))
lines(ts(deseasonal_count))

###FURTHER ANALYSIS AND TESTING######
##TEST AGAINST ORIGINAL AUTO ARIMA MODEL
##fit=arima(seas_fcast,order=c(212),#c(217)))
tsdisplay(residuals(fit_w_seasonality),lag.max = 15,main="seasonal model residuals")

fit3 <- auto.arima(deseasonal_count,seasonal=FALSE)
tsdisplay(residuals(fit3),lag.max = 15,main='seasonal model residuals')

##Evidance exists at a lag , a higher order q value (q=7) might be necessarey
fit4 <- arima(deseasonal_count,order = c(2,1,7)) #c(2,1,7)
tsdisplay(residuals(fit4),lag.max = 15,main='seasonal model residuals')

#Default arima (1,1,1) =(p,d,q) for most apps
fit5 <- arima(deseasonal_count,order = c(1,1,1))
tsdisplay(residuals(fit5),lag.max = 15,main='seasonal model residuals')

#Final fit and Test for Arima
par(mfrow=c(2,2))
#auto.arimo (2,1,2) fit with seasonality
fcast <- forecast(fit_w_seasonality,h=30)
plot(fcast)
#auto arima (2,1,2) fit without seasonality
fcast1 <- forecast(fit3,h=34)
plot(fcast1)
#custom arima (2,1,7)fit
fcast2 <- forecast(fit4,h=34)
plot(fcast2)
#general default arima (1,1,1) fit
fcast3 <- forecast(fit5,h=34)
plot(fcast3)

par(mfrow=c(1,1))## to get the graphs to normal
class(fcast2)



write.csv(fcast2, file = "results_1.csv")

