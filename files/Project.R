library("data.table")
library("ggplot2")
library("xts")
require(jsonlite)
require(httr)
require(data.table)

get_token <- function(username, password, url_site){
  
  post_body = list(username=username,password=password)
  post_url_string = paste0(url_site,'/token/')
  result = POST(post_url_string, body = post_body)
  
  # error handling (wrong credentials)
  if(result$status_code==400){
    print('Check your credentials')
    return(0)
  }
  else if (result$status_code==201){
    output = content(result)
    token = output$key
  }
  
  return(token)
}

get_data <- function(start_date='2020-03-20', token, url_site){
  
  post_body = list(start_date=start_date,username=username,password=password)
  post_url_string = paste0(url_site,'/dataset/')
  
  header = add_headers(c(Authorization=paste('Token',token,sep=' ')))
  result = GET(post_url_string, header, body = post_body)
  output = content(result)
  data = data.table::rbindlist(output)
  data[,event_date:=as.Date(event_date)]
  data = data[order(product_content_id,event_date)]
  return(data)
}


send_submission <- function(predictions, token, url_site, submit_now=F){
  
  format_check=check_format(predictions)
  if(!format_check){
    return(FALSE)
  }
  
  post_string="list("
  for(i in 1:nrow(predictions)){
    post_string=sprintf("%s'%s'=%s",post_string,predictions$product_content_id[i],predictions$forecast[i])
    if(i<nrow(predictions)){
      post_string=sprintf("%s,",post_string)
    } else {
      post_string=sprintf("%s)",post_string)
    }
  }
  
  submission = eval(parse(text=post_string))
  json_body = jsonlite::toJSON(submission, auto_unbox = TRUE)
  submission=list(submission=json_body)
  
  print(submission)
  # {"31515569":2.4,"32939029":2.4,"4066298":2.4,"6676673":2.4,"7061886":2.4,"85004":2.4} 
  
  if(!submit_now){
    print("You did not submit.")
    return(FALSE)      
  }
  
  
  header = add_headers(c(Authorization=paste('Token',token,sep=' ')))
  post_url_string = paste0(url_site,'/submission/')
  result = POST(post_url_string, header, body=submission)
  
  if (result$status_code==201){
    print("Successfully submitted. Below you can see the details of your submission")
  } else {
    print("Could not submit. Please check the error message below, contact the assistant if needed.")
  }
  
  print(content(result))
  
}

check_format <- function(predictions){
  
  if(is.data.frame(predictions) | is.data.frame(predictions)){
    if(all(c('product_content_id','forecast') %in% names(predictions))){
      if(is.numeric(predictions$forecast)){
        print("Format OK")
        return(TRUE)
      } else {
        print("forecast information is not numeric")
        return(FALSE)                
      }
    } else {
      print("Wrong column names. Please provide 'product_content_id' and 'forecast' columns")
      return(FALSE)
    }
    
  } else {
    print("Wrong format. Please provide data.frame or data.table object")
    return(FALSE)
  }
  
}

# this part is main code
subm_url = 'http://167.172.183.67'

u_name = "Group4"
p_word = "hlsXD7LPfzNTLKh2"
submit_now = FALSE

username = u_name
password = p_word

token = get_token(username=u_name, password=p_word, url=subm_url)
data = get_data(token=token,url=subm_url)

predictions=unique(data[,list(product_content_id)])
predictions[,forecast:=2.3]

send_submission(predictions, token, url=subm_url, submit_now=F)

head(data)
data = data.table(data)

#install.packages("data.table")
library("data.table")
#install.packages("ggplot2")
library("ggplot2")
#install.packages("plotly")
library("plotly")
#install.packages("xts")
library("xts")
#install.packages("forecast")
library("forecast")

posay<- product8_data
posay$event_date<- as.Date(posay$event_date)
acf(posay$sold_count,main="Autocorrelation of Sold Count")

plot_ly(x=posay$event_date,y=posay$sold_count, mode="lines")
plot(posay$event_date,posay$sold_count,type='l')

#2019-05-08 tarihinde stok yok
posay[event_date=="2019-05-08"]$sold_count <-as.integer((product8_data[event_date=="2019-05-07"]$sold_count+product8_data[event_date=="2019-05-10"]$sold_count) /2)
posay[event_date=="2019-05-08"]$price <-(product8_data[event_date=="2019-05-07"]$price+product8_data[event_date=="2019-05-10"]$price) /2

#2019-05-09 tarihinde stok yok
posay[event_date=="2019-05-09"]$sold_count <-as.integer((product8_data[event_date=="2019-05-07"]$sold_count+product8_data[event_date=="2019-05-10"]$sold_count) /2)
posay[event_date=="2019-05-09"]$price <-(product8_data[event_date=="2019-05-07"]$price+product8_data[event_date=="2019-05-10"]$price) /2

#2019-06-26 tarihinde stok yok
posay[event_date=="2019-06-26"]$sold_count <-as.integer((product8_data[event_date=="2019-06-25"]$sold_count+product8_data[event_date=="2019-06-27"]$sold_count) /2)
posay[event_date=="2019-06-26"]$price <-(product8_data[event_date=="2019-06-25"]$price+product8_data[event_date=="2019-06-27"]$price) /2

#2019-07-13 tarihinde stok yok
posay[event_date=="2019-07-13"]$sold_count <-as.integer(product8_data[event_date=="2019-07-12"]$sold_count*0.25+product8_data[event_date=="2019-07-14"]$sold_count*0.75) 
posay[event_date=="2019-07-13"]$price <-product8_data[event_date=="2019-07-12"]$price*0.75+product8_data[event_date=="2019-07-14"]$price*0.25

#2019-09-01 tarihinde stok yok
posay[event_date=="2019-09-01"]$sold_count <-as.integer(product8_data[event_date=="2019-08-31"]$sold_count*(1/3)+product8_data[event_date=="2019-09-02"]$sold_count*(2/3)) 
posay[event_date=="2019-09-01"]$price <-product8_data[event_date=="2019-08-31"]$price*(2/3)+product8_data[event_date=="2019-09-02"]$price*(1/3)


#LINEAR REGRESSION

train<- posay[c(1:357)]
test<- posay[c(358:424)]
train$product_content_id<- NULL
test$product_content_id<- NULL
lm_model <- lm(sold_count~.,data=train)
summary(lm_model)

prediction_posay <- predict(lm_model,newdata = test)

plot(test$sold_count,type='l',ylab="sold_count ")
par(new=TRUE)
plot(prediction_posay,col='Red',type = 'l',ylab=" ")
legend(20,900, legend=c("Actual Sold Count", "Predicted Sold Count"),
       col=c("black", "red"), lty=1:1, cex=0.5,
       box.lty=0)

RMSE_lr<- sqrt(mean((prediction_posay-test$sold_count)^2))
RMSE_lr

MAE_lr<- mean(abs(prediction_posay-test$sold_count))
MAE_lr

MAPE<- 100*mean(abs(prediction_posay-test$sold_count)/test$sold_count)
MAPE
vif(lm_model)

ppp<- train
ppp$event_date<- as.numeric(ppp$event_date)
#normalize <- function(x){ return((x-min(x)) / (max(x)-min(x))) }
#ppp$event_date<- normalize(ppp$event_date)
#ppp$category_visits<- normalize(ppp$category_visits)

ppt<- test
#ppt$event_date<- as.numeric(ppt$event_date)
#ppt$event_date<- normalize(ppt$event_date)

lm_ppp <- lm(sold_count~price+visit_count+favored_count,data=ppp)
summary(lm_ppp)
cor(ppp)
vif(lm_ppp)

ppp_prediction_posay <- predict(lm_ppp,newdata = ppt)
#ppp_prediction_posay[8]<-80
#ppp_prediction_posay[9]<-80
plot(ppt$sold_count,type='l',ylab=" ",ylim=c(0,300))
par(new=TRUE)
plot(ppp_prediction_posay,col='Red',ylim=c(0,300),type ='l',main="Actual vs Predicted Sold Counts")
legend("topright", legend=c("Actual Sold Count", "Predicted Sold Count"),
       col=c("black", "red"), lty=1:1, cex=0.5,text.width = 15
       ,box.lty=1)

RMSE_lr<- sqrt(mean((ppp_prediction_posay-ppt$sold_count)^2))
RMSE_lr

MAE_lr<- mean(abs(ppp_prediction_posay-ppt$sold_count))
MAE_lr

MAPE<- 100*mean(abs(ppp_prediction_posay-ppt$sold_count)/ppt$sold_count)
MAPE


#naive
library("knitr")
naive_posay<- posay$sold_count[3:424]
naive_posay

lagged_posay <- posay$sold_count[1:422]
lagged_posay

RMSE_lm_arima <- sqrt(mean((lagged_posay-naive_posay)^2))
RMSE_lm_arima

MAE_lm_arima<- mean(abs(lagged_posay-naive_posay))
MAE_lm_arima

MAPE_naive <- 100*mean(abs(lagged_posay-naive_posay)/naive_posay)
MAPE_naive
mape(naive_posay,lagged_posay)

last67_naive_posay<- naive_posay[356:422]
last67_lagged_posay <- lagged_posay[(356):422]
mape(last67_naive_posay,last67_lagged_posay)

RMSE_lm_arima <- sqrt(mean((last67_lagged_posay-last67_naive_posay)^2))
RMSE_lm_arima

MAE_lm_arima<- mean(abs(last67_lagged_posay-last67_naive_posay))
MAE_lm_arima
MAPE_naive <- 100*mean(abs(last67_lagged_posay-last67_naive_posay)/last67_naive_posay)
MAPE_naive

plot(last67_naive_posay,type='l',ylab=" ")
par(new=TRUE)
plot(last67_lagged_posay,col='Red',type = 'l',main="Actual vs Predicted Sold Counts")
legend("bottomleft", legend=c("Actual Sold Count", "Predicted Sold Count"),
       col=c("black", "red"), lty=1:1, cex=0.5,text.width = 15
       ,box.lty=1)


#averaging naive and linear

double_mean_forecast <- (last67_lagged_posay+ppp_prediction_posay)/2
double_mean_forecast

plot(test$sold_count,type='l',ylim=c(0,185),ylab=" ")
par(new=TRUE)
plot(double_mean_forecast,col='Red',type = 'l',ylim=c(0,185),main="Actual vs Predicted Sold Counts")
legend("bottomleft", legend=c("Actual Sold Count", "Predicted Sold Count"),
       col=c("black", "red"), lty=1:1, cex=0.5,text.width = 15
       ,box.lty=1)

RMSE_double<- sqrt(mean((double_mean_forecast-test$sold_count)^2))
RMSE_double

MAE_double<- mean(abs(double_mean_forecast-test$sold_count))
MAE_double

MAPE_double <- 100*mape(test$sold_count,double_mean_forecast)
MAPE_double
plot(double_mean_forecast,test$sold_count,main="Double Model vs Actual Sold Count")
abline(0,1,col="red")


# 11 Kasým Ýndirimler (9-10-11 Kasým)
posay[event_date == "2019-11-09"]$sold_count <-product8_data[event_date == "2019-11-08"]$sold_count

posay[event_date == "2019-11-10"]$sold_count <- product8_data[event_date == "2019-11-12"]$sold_count 

posay[event_date == "2019-11-11"]$sold_count <- posay[event_date == "2019-11-09"]$sold_count

# Efsane Günler
posay[event_date == "2019-11-25"]$sold_count <-as.integer((product8_data[event_date == "2019-11-24"]$sold_count+product8_data[event_date=="2019-11-23"]$sold_count)/2)

posay[event_date == "2019-11-26"]$sold_count <-as.integer((posay[event_date == "2019-11-25"]$sold_count+posay[event_date == "2019-11-27"]$sold_count)/2)

posay[event_date == "2019-11-27"]$sold_count <-product8_data[event_date == "2019-11-23"]$sold_count

posay[event_date == "2019-11-29"]$sold_count <-product8_data[event_date == "2019-11-27"]$sold_count


#ARIMA MODEL 

st_arima_posay <- data.table()
st_arima_posay$date <- as.Date(posay$event_date)
st_arima_posay$sold_count <- posay$sold_count
st_arima_posay <- as.xts(st_arima_posay)

train_arima_posay_1<- st_arima_posay[1:357]
train_arima_posay_1
test_arima_posay_1<- st_arima_posay[358:424]
test_arima_posay_1

product8_data_train <- as.xts(train_arima_posay_1)
product8_data_test <- as.xts(test_arima_posay_1)
arima_model_product8 <- auto.arima(product8_data_train)
summary(arima_model_product8)
checkresiduals(arima_model_product8)

forecast_product8 <- forecast(arima_model_product8, h=nrow(product8_data_test))
plot(forecast_product8)
forecast_product8
forecast_numeric <- as.numeric(forecast_product8$mean)
test_numeric <- as.numeric(product8_data_test$sold_count)

RMSE_arima <- sqrt(mean((forecast_numeric-ppt$sold_count)^2))
RMSE_arima

mae_arima <- mean(abs(forecast_numeric - test_numeric))
mae_arima
mape_arima <- 100*mean(abs(forecast_numeric - test_numeric)/test_numeric)
mape_arima

plot(test_numeric,type='l',ylab="sold_count ",ylim=c(0,100))
par(new=TRUE)
plot(forecast_numeric,col='Red',type = 'l',ylab=" ",ylim=c(0,100),main="Actual vs Predicted Sold Counts")
legend("bottomleft", legend=c("Actual Sold Count", "Predicted Sold Count"),
       col=c("black", "red"), lty=1:1, cex=0.5,text.width = 15,
       box.lty=1)



oralb<- product2_data
#Eliminate the non sold days at the beginning
oralb <- product2_data[event_date>"2019-11-22"]

acf(product2_data$sold_count, lag.max = 60)
plot(oralb$event_date,oralb$sold_count,type='l',main="Sold Count in Time")

#DATA MANIPULATION

#"2019-10-12" date has price = -1.00
oralb[event_date=="2019-11-30"]$price <- (product2_data[event_date=="2019-11-29"]$price + product2_data[event_date=="2019-12-01"]$price)/2
oralb[event_date=="2019-11-30"]$sold_count <- as.integer((product2_data[event_date=="2019-11-29"]$sold_count + product2_data[event_date=="2019-12-01"]$sold_count)/2)

#"2019-12-04" date has price = -1.00
oralb[event_date=="2019-12-04"]$price <- (product2_data[event_date=="2019-12-03"]$price + product2_data[event_date=="2019-12-05"]$price)/2
oralb[event_date=="2019-12-04"]$sold_count <- as.integer((product2_data[event_date=="2019-12-03"]$sold_count + product2_data[event_date=="2019-12-05"]$sold_count)/2)

#"2019-12-08" date has price = -1.00
oralb[event_date=="2019-12-08"]$price <- (product2_data[event_date=="2019-12-07"]$price + product2_data[event_date=="2019-12-09"]$price)/2
oralb[event_date=="2019-12-08"]$sold_count <- as.integer((product2_data[event_date=="2019-12-07"]$sold_count + product2_data[event_date=="2019-12-09"]$sold_count)/2)

#"2019-12-21" date has price = -1.00
oralb[event_date=="2019-12-21"]$price <- (product2_data[event_date=="2019-12-20"]$price + product2_data[event_date=="2019-12-22"]$price)/2
oralb[event_date=="2019-12-21"]$sold_count <- as.integer((product2_data[event_date=="2019-12-20"]$sold_count + product2_data[event_date=="2019-12-22"]$sold_count)/2)

#"2020-06-22" date has price = -1.00
oralb[event_date=="2020-06-22"]$price <- product2_data[event_date=="2020-06-21"]$price 
oralb[event_date=="2020-06-22"]$sold_count <- 1

#"2020-06-23" date has price = -1.00
oralb[event_date=="2020-06-23"]$price <- product2_data[event_date=="2020-06-21"]$price 
oralb[event_date=="2020-06-23"]$sold_count <- 1 

#"2020-06-24" date has price = -1.00
oralb[event_date=="2020-06-24"]$price <- oralb[event_date=="2020-06-23"]$price 
oralb[event_date=="2020-06-24"]$sold_count <- 1

#"2020-06-25" date has price = -1.00
oralb[event_date=="2020-06-25"]$price <- oralb[event_date=="2020-06-24"]$price 
oralb[event_date=="2020-06-25"]$sold_count <- 1

#LINEAR REGRESSION

train_oralb<- oralb[c(1:150)]
test_oralb<- oralb[c(151:217)]
train_oralb$product_content_id<- NULL
test_oralb$product_content_id<- NULL
lm_model_oralb <- lm(sold_count~price+visit_count+basket_count+favored_count,data=train_oralb)
summary(lm_model_oralb)

prediction_oralb <- predict(lm_model_oralb,newdata = test_oralb)
prediction_oralb[prediction_oralb<0]<-0
plot(test_oralb$sold_count,type='l',ylab="sold_count",ylim=c(0,500),main="Actual vs Predicted Sold Counts")
par(new=TRUE)
plot(prediction_oralb,col='Red',type = 'l',ylab=" ",ylim=c(0,500))
legend("topright", legend=c("Actual Sold Count", "Predicted Sold Count"),
       col=c("black", "red"), lty=1:1, cex=0.5,
       box.lty=1)

RMSE_lr<- sqrt(mean((prediction_oralb-test_oralb$sold_count)^2))
RMSE_lr

MAE_lr<- mean(abs(prediction_oralb-test_oralb$sold_count))
MAE_lr

MAPE<- 100*mean(abs(prediction_oralb-test_oralb$sold_count)/test_oralb$sold_count)
MAPE

vif(lm_model_oralb)

ppp_oralb<-train_oralb
ppp_oralb$event_date<- as.numeric(ppp_oralb$event_date)
normalize <- function(x){ return((x-min(x)) / (max(x)-min(x))) }
ppp_oralb$event_date<- normalize(ppp_oralb$event_date)

cor(ppp_oralb)


#naive
naive_oralb<- oralb$sold_count[3:217]
naive_oralb

lagged_oralb <- oralb$sold_count[1:215]
lagged_oralb

RMSE_lm_arima_oralb <- sqrt(mean((lagged_oralb-naive_oralb)^2))
RMSE_lm_arima_oralb

MAE_lm_arima_oralb<- mean(abs(lagged_oralb-naive_oralb))
MAE_lm_arima_oralb

MAPE_naive_oralb <- 100*mean(abs(lagged_oralb-naive_oralb)/naive_oralb)
MAPE_naive_oralb
mape(naive_oralb,lagged_oralb)

last67_naive_oralb<- naive_oralb[149:215]
last67_lagged_oralb <- lagged_oralb[(149):215]

RMSE_naive_oralb <- sqrt(mean((last67_lagged_oralb-last67_naive_oralb)^2))
RMSE_naive_oralb

MAE_naive_oralb<- mean(abs(last67_lagged_oralb-last67_naive_oralb))
MAE_naive_oralb

MAPE_naive_oralb <- 100*mean(abs(last67_lagged_oralb-last67_naive_oralb)/last67_naive_oralb)
MAPE_naive_oralb

plot(last67_naive_oralb,type='l',ylab=" ")
par(new=TRUE)
plot(last67_lagged_oralb,col='Red',type = 'l',main="Actual vs Predicted Sold Counts")
legend("topright", legend=c("Actual Sold Count", "Predicted Sold Count"),
       col=c("black", "red"), lty=1:1, cex=0.5,text.width = 15
       ,box.lty=1)



#4-5-6 ÞUBAT ÝNDÝRÝMLERÝ
oralb[event_date == "2020-02-04"]$sold_count <- product2_data[event_date == "2020-02-03"]$sold_count
oralb[event_date == "2020-02-05"]$sold_count <- oralb[event_date == "2020-02-04"]$sold_count
oralb[event_date == "2020-02-06"]$sold_count <- oralb[event_date == "2020-02-05"]$sold_count
oralb[event_date == "2020-02-07"]$sold_count <- oralb[event_date == "2020-02-06"]$sold_count

# 11 Kasým Ýndirimler (9-10-11 Kasým)
oralb[event_date == "2019-11-09"]$sold_count <-product2_data[event_date == "2019-11-08"]$sold_count

oralb[event_date == "2019-11-10"]$sold_count <- product2_data[event_date == "2019-11-09"]$sold_count 

oralb[event_date == "2019-11-11"]$sold_count <- product2_data[event_date == "2019-11-10"]$sold_count

#Decomposition
library("xts")
ts_product <- data.table()
ts_product$date <- oralb$event_date
ts_product$sold_count <- oralb$sold_count
ts_product <- as.ts(as.xts(ts_product))

plot(ts_product)
acf(ts_product)
acf(oralb$sold_count)

ts_product = ts(ts_product, frequency = 7)
decompose_product = decompose(ts_product, "multiplicative")

plot(decompose_product)
Box.test(ts_product)


#ARIMA MODEL 

st_arima_oralb <- data.table()
st_arima_oralb$date <- as.Date(oralb$event_date)
st_arima_oralb$sold_count <- oralb$sold_count
st_arima_oralb <- as.xts(st_arima_oralb)

st_arima_oralb_1<- st_arima_oralb[1:150]
st_arima_oralb_1
st_arima_oralb_test<- st_arima_oralb[151:217]
st_arima_oralb_test

product2_data_train <- as.xts(st_arima_oralb_1)
product2_data_test <- as.xts(st_arima_oralb_test)
arima_model_product2 <- auto.arima(product2_data_train)
summary(arima_model_product2)
checkresiduals(arima_model_product2)


forecast_product2 <- forecast(arima_model_product2, h=nrow(product2_data_test))
plot(forecast_product2)
forecast_product2
forecast_numeric <- as.numeric(forecast_product2$lower)
forecast_numeric[forecast_numeric<0]<-0
test_numeric <- as.numeric(product2_data_test$sold_count)

RMSE_arima_oralb <- sqrt(mean((forecast_numeric-test_numeric)^2))
RMSE_arima_oralb

mae_arima_oralb <- mean(abs(forecast_numeric - test_numeric))
mae_arima_oralb

mape_arima_oralb <- 100*mean(abs(forecast_numeric - test_numeric)/test_numeric)
mape_arima_oralb

plot(test_numeric,type='l',ylab="sold_count",ylim=c(0,500),main="Actual vs Predicted Sold Counts")
par(new=TRUE)
plot(forecast_numeric,col='Red',type = 'l',ylab=" ",ylim=c(0,500))
legend("topright", legend=c("Actual Sold Count", "Predicted Sold Count"),
       col=c("black", "red"), lty=1:1, cex=0.5,
       box.lty=1)


#averaging

mean_lm_arima_oralb <- (prediction_oralb+forecast_numeric)/2

RMSE_lm_arima <- sqrt(mean((mean_lm_arima_oralb-ppt$sold_count)^2))
RMSE_lm_arima

MAE_lm_arima<- mean(abs(mean_lm_arima_oralb-ppt$sold_count))
MAE_lm_arima

MAPE_lm_arima<- 100*mean(abs(mean_lm_arima_oralb-ppt$sold_count)/ppt$sold_count)
MAPE_lm_arima

plot(ppt$sold_count,type='l',ylab=" ")
par(new=TRUE)
plot(mean_lm_arima_oralb,col='Red',type = 'l',ylab="sold_count",main="Actual vs Predicted Sold Count")
legend("bottomleft", legend=c("Actual Sold Count", "Predicted Sold Count"),
       col=c("black", "red"), lty=1:1, cex=0.5,text.width = 15
       ,box.lty=1)

plot(mean_lm_arima_oralb,test$sold_count,main="Combined Model vs Actual Sold Count")
abline(0,1,col="red")



#triple average model
triple_mean_forecast_oralb <- (last67_lagged_oralb+2*test_numeric+prediction_oralb)/4
triple_mean_forecast_oralb

plot(test$sold_count,type='l')
par(new=TRUE)
plot(triple_mean_forecast_oralb,col='Red',type = 'l')

RMSE_triple<- sqrt(mean((triple_mean_forecast_oralb-test$sold_count)^2))
RMSE_triple

MAE_triple<- mean(abs(triple_mean_forecast_oralb-test$sold_count))
MAE_triple

MAPE_triple <- 100*mape(test$sold_count,triple_mean_forecast_oralb)
MAPE_triple
plot(triple_mean_forecast_oralb,test$sold_count,main="Triple Model vs Actual Sold Count")
abline(0,1,col="red")


#TAYT DATA 31515569
data$event_date <- as.Date(data$event_date)
tayt_data <- data[product_content_id == 31515569]

#No data before 2019-09-21
tayt_data <- tayt_data[event_date>"2019-09-21"]
tayt_data <- tayt_data[event_date<"2020-05-15"]
tayt_data$price <- as.double(tayt_data$price)
tayt_data$product_content_id <- as.double(tayt_data$product_content_id)
tayt_data$sold_count <- as.double(tayt_data$sold_count)
tayt_data$visit_count <- as.double(tayt_data$visit_count)
tayt_data$favored_count <- as.double(tayt_data$favored_count)
tayt_data$basket_count <- as.double(tayt_data$basket_count)
tayt_data$category_sold <- as.double(tayt_data$category_sold)
tayt_data$category_brand_sold <- as.double(tayt_data$category_brand_sold)
tayt_data$category_visits <- as.double(tayt_data$category_visits)
tayt_data$ty_visits <- as.double(tayt_data$ty_visits)

#days price = -1 no tayt sold because of no tayt in the stock
tayt_data$sold_count <- as.double(tayt_data$sold_count)
ifelse(tayt_data$price == (-1), index(tayt_data) , 0)
#To change numbers find best way

# Code for Figure 21

acf(tayt_data$sold_count, main="Correlation of Sold Count")



tayt_data[15,4:11]=(tayt_data[16,4:11]+tayt_data[14,4:11])/2
tayt_data[15,"price"]=(tayt_data[16,"price"]+tayt_data[14,"price"])/2

tayt_data[22,4:11]=(tayt_data[23,4:11]+tayt_data[21,4:11])/2
tayt_data[22,"price"]=(tayt_data[23,"price"]+tayt_data[21,"price"])/2

tayt_data[51,4:11]=tayt_data[50,4:11]
tayt_data[51,"price"]=tayt_data[50,"price"]
tayt_data[52,4:11]=tayt_data[53,4:11]
tayt_data[52,"price"]=tayt_data[53,"price"]

tayt_data[100,4:11]=tayt_data[99,4:11]
tayt_data[100,"price"]=tayt_data[99,"price"]

tayt_data[101,4:11]=tayt_data[99,4:11]
tayt_data[101,"price"]=tayt_data[99,"price"]

tayt_data[102,4:11]=tayt_data[104,4:11]
tayt_data[102,"price"]=tayt_data[104,"price"]

tayt_data[103,4:11]=tayt_data[104,4:11]
tayt_data[103,"price"]=tayt_data[104,"price"]

tayt_data[239,4:11]=(tayt_data[238,4:11]+tayt_data[240,4:11])/2
tayt_data[239,"price"]=(tayt_data[238,"price"]+tayt_data[240,"price"])/2



#Manipulating Data with Google Data


Trendyol_google_yearly <-read.csv("E:/Study/2019-20/Spring/IE 360/TrendyolndirimGunleri.csv", header = T)
Trendyol_google_yearly = data.table(Trendyol_google_yearly)
Trendyol_google_yearly$event_date <- as.Date(Trendyol_google_yearly$event_date)
TrendyolGraph <-Trendyol_google_yearly[event_date<"2020-05-15"]
TrendyolGraph <-TrendyolGraph[event_date>"2020-01-22"]
#Deneysel basmak için 
temp <- tayt_data[event_date<"2020-05-15"]
temp <-temp[event_date>"2020-01-22"]

# Code for Figure 22

plot(TrendyolGraph$event_date,TrendyolGraph$amount, type="l",xlab=" ",ylab=" ")
par(new=TRUE)
plot(temp$event_date,temp$ty_visits, type="l", col="blue",yaxt = "n",main = "Google vs ty_visit Comparison",xlab="Event Date",ylab="Values")
legend("topleft", legend=c("Google Data", "ty_visit"), col=c("black", "blue"), lty=1:1, cex=0.8)


#Generate ty_visit with using google data
Trendyol_google_yearly <-  Trendyol_google_yearly[event_date<"2020-05-15"]
tayt_data<-merge(x=tayt_data,y=Trendyol_google_yearly,by="event_date",all=TRUE)
tayt_data$ty_visit_div_google <- tayt_data$ty_visits / tayt_data$amount

#ty_visit info starts at 2020-01-23  and we want to find mean of this ratio
mean_of_tyvisit_div_google <- mean(tayt_data[124:236,ty_visit_div_google])
tayt_data[1:123,"ty_visits"]<- tayt_data[1:123,amount]*mean_of_tyvisit_div_google

TrendyolGraph <-Trendyol_google_yearly[event_date<"2020-05-15"]
TrendyolGraph <-TrendyolGraph[event_date>"2019-09-21"]
temp <- tayt_data[event_date<"2020-05-15"]


plot(TrendyolGraph$event_date,TrendyolGraph$amount, type="o",xlab=" ",ylab=" ")
par(new=TRUE)
plot(temp$event_date,temp$ty_visits, type="l", col="blue",yaxt = "n")

plot(tayt_data$event_date,tayt_data$sold_count,type="l",main = "sold_count by event_date",xlab="event_date",ylab="sold_count")


# Ýndirim günleri data düzenlemeleri
tayt_data_temp <- tayt_data
tayt_data[event_date == "2019-11-09",4:13] <- (tayt_data[event_date == "2019-11-08",4:13]+tayt_data[event_date == "2019-11-10",4:13])/2
tayt_data[event_date == "2019-11-09", "price"] <- (tayt_data[event_date == "2019-11-08","price"]+tayt_data[event_date == "2019-11-10", "price"])/2

# Efsane Günler
tayt_data[event_date == "2019-11-25",4:13]<-tayt_data[event_date == "2019-11-24",4:13]
tayt_data[event_date == "2019-11-26",4:13]<-tayt_data[event_date == "2019-11-24",4:13]
tayt_data[event_date == "2019-11-27",4:13]<-tayt_data[event_date == "2019-11-24",4:13]/2+tayt_data[event_date == "2019-11-30",4:13]/2
tayt_data[event_date == "2019-11-28",4:13]<-tayt_data[event_date == "2019-11-30",4:13]
tayt_data[event_date == "2019-11-29",4:13]<-tayt_data[event_date == "2019-11-30",4:13]

tayt_data[event_date == "2019-11-25","price"]<-tayt_data[event_date == "2019-11-24","price"]
tayt_data[event_date == "2019-11-26","price"]<-tayt_data[event_date == "2019-11-24","price"]
tayt_data[event_date == "2019-11-27","price"]<-tayt_data[event_date == "2019-11-24","price"]/2+tayt_data[event_date == "2019-11-30","price"]/2
tayt_data[event_date == "2019-11-28","price"]<-tayt_data[event_date == "2019-11-30","price"]
tayt_data[event_date == "2019-11-29","price"]<-tayt_data[event_date == "2019-11-30","price"]

plot(tayt_data$event_date,tayt_data$sold_count,type="l",main = "sold_count by event_date after manipulation",xlab="event_date",ylab="sold_count")
acf(tayt_data$sold_count)

# Code for Figure 23

plot(tayt_data_temp$event_date,tayt_data_temp$sold_count,type="l",main = "sold_count by event_date",xlab="event_date",ylab="sold_count", ylim=c(0, 1900))
par(new=TRUE)
plot(tayt_data$event_date,tayt_data$sold_count,type="l",col = "blue" ,main = "",xlab="",ylab="",ylim=c(0, 1900))
legend("topleft", legend=c("Original Data", "After Manipulation"), col=c("black", "blue"), lty=1:1, cex=0.8)


#delete unreliable columns
tayt_data$basket_count=NULL
tayt_data$category_visits=NULL
tayt_data$ty_visit_div_google = NULL
tayt_data$amount <-  NULL
tayt_data$category_brand_sold=NULL
tayt_data$category_sold=NULL



tayt_data$lag2_sold_count<- NA
tayt_data$lag3_sold_count<- NA
tayt_data$lag4_sold_count<- NA
tayt_data$lag7_sold_count<- NA
tayt_data$lag1_price<- NA
tayt_data$lag2_visit_count<- NA
tayt_data$lag2_ty_visits <- NA
tayt_data$lag2_favored_count<- NA

for(i in 1:(nrow(tayt_data)-2)){
  tayt_data$lag2_sold_count[i+2] <- tayt_data$sold_count[i]
}

for(i in 1:(nrow(tayt_data)-3)){
  tayt_data$lag3_sold_count[i+3] <- tayt_data$sold_count[i]
}
for(i in 1:(nrow(tayt_data)-4)){
  tayt_data$lag4_sold_count[i+4] <- tayt_data$sold_count[i]
}

for(i in 1:(nrow(tayt_data)-7)){
  tayt_data$lag7_sold_count[i+7] <- tayt_data$sold_count[i]
}

for(i in 1:(nrow(tayt_data)-1)){
  tayt_data$lag1_price[i+1] <- tayt_data$price[i]
}

for(i in 1:(nrow(tayt_data)-2)){
  tayt_data$lag2_ty_visits[i+2] <- tayt_data$ty_visits[i]
}

for(i in 1:(nrow(tayt_data)-2)){
  tayt_data$lag2_visit_count[i+2] <- tayt_data$visit_count[i]
}


for(i in 1:(nrow(tayt_data)-2)){
  tayt_data$lag2_favored_count[i+2] <- tayt_data$favored_count[i]
}


library(forecast)
library(caTools)

set.seed(44)
split=sample.split(tayt_data$sold_count,SplitRatio=0.7)
train=subset(tayt_data,split==TRUE)
test=subset(tayt_data,split==FALSE)
train <- train[7:165,]

pred <- lm(sold_count~lag2_sold_count+lag3_sold_count+lag4_sold_count+lag7_sold_count+lag2_ty_visits+lag1_price+lag2_visit_count+lag2_favored_count, data = train)
pred
summary(pred)
forecast_values <- forecast(pred, test)
forecast_values <- as.numeric(forecast_values$mean)


#create temp to calculate mape for lm (ilk üç NA veri olduðu için 
temp_test<-test[2:71,"sold_count"]
temp_forecast_values<-forecast_values[2:71]

temp_test$forecast_values <-NA
temp_test$forecast_values <- forecast_values[2:71]
temp_test$mape_value <- NA
temp_test$mape_value <- 100*abs(temp_test$sold_count-temp_test$forecast_values)/temp_test$sold_count

mean(temp_test$mape_value , na.rm = T)
max(temp_test$mape_value)
min(temp_test$mape_value)


temp_test$mae_value <- NA
temp_test$mae_value <- abs(temp_test$sold_count-temp_test$forecast_values)

mean(temp_test$mae_value , na.rm = T)
max(temp_test$mae_value)
min(temp_test$mae_value)



#NAÝVE APPROACH BETTER THAN LM 
temp_tayt_data <- tayt_data[-(1:2),]
temp_tayt_data$naive_mape <- 100*abs(temp_tayt_data$sold_count-temp_tayt_data$lag2_sold_count)/temp_tayt_data$sold_count
mean(temp_tayt_data$naive_mape)
max(temp_tayt_data$naive_mape)
min(temp_tayt_data$naive_mape)

temp_tayt_data$naive_mae <- abs(temp_tayt_data$sold_count-temp_tayt_data$lag2_sold_count)
mean(temp_tayt_data$naive_mae)
max(temp_tayt_data$naive_mae)
min(temp_tayt_data$naive_mae)












library("xts")
ts_tayt <- data.table()
ts_tayt$date <- tayt_data$event_date
ts_tayt$sold_count <- tayt_data$sold_count
ts_tayt <- as.ts(as.xts(ts_tayt))
class(ts_tayt)
plot(ts_tayt)


trend_tayt = ma(ts_tayt, order = 7, centre = T)
trend_tayt
plot(ts_tayt)
lines(trend_tayt)
plot(trend_tayt)


detrend_tayt<- ts_tayt-trend_tayt
class(detrend_tayt)
plot(detrend_tayt)

#seasonality
m_tayt = t(matrix(data = detrend_tayt, nrow = 4))
seasonal_tayt = colMeans(m_tayt, na.rm = T)
plot(as.ts(rep(seasonal_tayt,5)))

#randomness
random_tayt = ts_tayt - trend_tayt - seasonal_tayt
plot(as.ts(random_tayt))

#DECOMPOSE FUNCTION
ts_tayt = ts(ts_tayt, frequency = 7)
decompose_tayt = decompose(ts_tayt, "additive")


# Code for Figure 24

plot(decompose_tayt)











#Analyzing product 3904356
mont_data <- data[product_content_id == 3904356]
mont_data <- mont_data[event_date<"2020-05-15"]
mont_data$price <- as.double(mont_data$price)
mont_data$product_content_id <- as.double(mont_data$product_content_id)
mont_data$sold_count <- as.double(mont_data$sold_count)
mont_data$visit_count <- as.double(mont_data$visit_count)
mont_data$favored_count <- as.double(mont_data$favored_count)
mont_data$basket_count <- as.double(mont_data$basket_count)
mont_data$category_sold <- as.double(mont_data$category_sold)
mont_data$category_brand_sold <- as.double(mont_data$category_brand_sold)
mont_data$category_visits <- as.double(mont_data$category_visits)
mont_data$ty_visits <- as.double(mont_data$ty_visits)
mont_data$event_date <- as.Date(mont_data$event_date)

acf(mont_data$sold_count)

plot(mont_data$event_date,mont_data$sold_count, type="l",xlab=" ",ylab=" ")


#data manipulation
mont_data[event_date == "2019-11-09",4:11] <- (mont_data[event_date == "2019-11-08",4:11]+mont_data[event_date == "2019-11-10",4:11])/2
mont_data[event_date == "2019-11-09", "price"] <- (mont_data[event_date == "2019-11-08","price"]+mont_data[event_date == "2019-11-10", "price"])/2


# Code for Figure 27

plot(mont_data$event_date,mont_data$sold_count, type="l",main = "Sold Count of Coat By Event Date",xlab="Event Date ",ylab="Sold Count")


#Trendyol yýllýk arama


mont_google <-read.csv("E:/Study/2019-20/Spring/IE 360/google.csv", header = T)
mont_google = data.table(mont_google)
mont_google$event_date <- as.Date(mont_google$event_date)

# Code for Figure 25

plot(mont_google$event_date,mont_google$amount, type="l",main = "Google Search of MONT ",xlab="Event Date",ylab="Amount")


ts_montgoogle <- data.table()
ts_montgoogle$date <- mont_google$event_date
ts_montgoogle$sold_count <- mont_google$amount
ts_montgoogle <- as.ts(as.xts(ts_montgoogle))
plot(ts_montgoogle)



#DECOMPOSE FUNCTION
ts_montgoogle = ts(ts_montgoogle, frequency = 52)
decompose_mont = decompose(ts_montgoogle, "additive")

# Code for Figure 26

plot(decompose_mont)

mont_daily <-read.csv("E:/Study/2019-20/Spring/IE 360/montdaily.csv", header = T)
mont_daily$event_date <- as.Date(mont_daily$event_date)
mont_data<-merge(x=mont_data,y=mont_daily,by="event_date",all=TRUE)

 mont_data$price <- ifelse(mont_data$price == -1 , NA , mont_data$price)

 set_nearest_value_to_na <- function(data) {
   N <- length(data)
   index <- which(is.na(data))
   if (length(index) %in% c(0, N)) {
     return(data)
   }
   not_index <- which(!is.na(data))
   intervals  <- findInterval(index, not_index,
                              all.inside = TRUE)
   old_pos   <- not_index[pmax(1, intervals)]
   new_pos  <- not_index[pmin(N, intervals+1)]
   old_dist  <- index - old_pos
   new_dist <- new_pos - index
   
   data[index] <- ifelse(old_dist <= new_dist,
                         data[old_pos], data[new_pos])
   return(data)
 }
 
 
 mont_data$price <- set_nearest_value_to_na(mont_data$price)
 
 plot(mont_data$event_date, mont_data$amount, type="l",xlab=" ",ylab=" ")
 par(new=TRUE)
 plot(mont_data$event_date, mont_data$sold_count, type="l", col="blue",yaxt = "n")
 

#delete unreliable columns
mont_data$category_visits=NULL
mont_data$product_content_id=NULL
mont_data$ty_visits=NULL


mont_data$lag2_sold_count<- NA
mont_data$lag3_sold_count<- NA
mont_data$lag4_sold_count<- NA
mont_data$lag7_sold_count<- NA
mont_data$lag1_price<- NA
mont_data$lag2_visit_count<- NA
mont_data$lag2_favored_count<- NA
mont_data$lag2_basket_count<- NA
mont_data$lag2_category_sold<- NA
mont_data$lag2_category_brand_sold<- NA
mont_data$lag2_amount <-NA


for(i in 1:(nrow(mont_data)-2)){
  mont_data$lag2_sold_count[i+2] <- mont_data$sold_count[i]
}

for(i in 1:(nrow(mont_data)-3)){
  mont_data$lag3_sold_count[i+3] <- mont_data$sold_count[i]
}
for(i in 1:(nrow(mont_data)-4)){
  mont_data$lag4_sold_count[i+4] <- mont_data$sold_count[i]
}

for(i in 1:(nrow(mont_data)-7)){
  mont_data$lag7_sold_count[i+7] <- mont_data$sold_count[i]
}

for(i in 1:(nrow(mont_data)-1)){
  mont_data$lag1_price[i+1] <- mont_data$price[i]
}

for(i in 1:(nrow(mont_data)-2)){
  mont_data$lag2_visit_count[i+2] <- mont_data$visit_count[i]
}

for(i in 1:(nrow(mont_data)-2)){
  mont_data$lag2_favored_count[i+2] <- mont_data$favored_count[i]
}


for(i in 1:(nrow(mont_data)-2)){
  mont_data$lag2_basket_count[i+2] <- mont_data$basket_count[i]
}


for(i in 1:(nrow(mont_data)-2)){
  mont_data$lag2_category_sold[i+2] <- mont_data$category_sold[i]
}


for(i in 1:(nrow(mont_data)-2)){
  mont_data$lag2_category_brand_sold[i+2] <- mont_data$category_brand_sold[i]
}

for(i in 1:(nrow(mont_data)-2)){
  mont_data$lag2_amount[i+2] <- mont_data$amount[i]
}




set.seed(4)
split=sample.split(mont_data$sold_count,SplitRatio=0.7)
train=subset(mont_data,split==TRUE)
test=subset(mont_data,split==FALSE)
train <- train[7:268,]

pred <- lm(sold_count~lag2_sold_count+lag3_sold_count+lag4_sold_count+lag7_sold_count+lag1_price+lag2_visit_count+lag2_favored_count+lag2_basket_count+lag2_category_sold+lag2_category_brand_sold+lag2_amount, data = train)
pred
summary(pred)
forecast_values <- forecast(pred, test)
forecast_values <- as.numeric(forecast_values$mean)
forecast_values
forecast_values <- round(forecast_values)
forecast_values <- ifelse(forecast_values<0 , 0 , forecast_values)
forecast_values

#create temp to calculate mape for lm (ilk üç NA veri olduðu için 
temp_test<-test[2:113,]
temp_forecast_values<-forecast_values[2:113]

temp_test$forecast_values <-NA
temp_test$forecast_values <- forecast_values[2:113]



#NAÝVE APPROACH BETTER THAN LM 
temp_test$diff_lm <- abs(temp_test$forecast_values-temp_test$sold_count)
temp_test$diff_naive <- abs(temp_test$lag2_sold_count-temp_test$sold_count)
temp_test$lm_better <- NA
temp_test$lm_better <- ifelse(temp_test$diff_lm<=temp_test$diff_naive,1,0)
mean(temp_test$lm_better, na.rm = T)


temp_test$naive_mae <- abs(temp_test$sold_count-temp_test$lag2_sold_count)
mean(temp_test$naive_mae)
max(temp_test$naive_mae)
min(temp_test$naive_mae)


temp_test$mae_value <- NA
temp_test$mae_value <- abs(temp_test$sold_count-temp_test$forecast_values)

mean(temp_test$mae_value , na.rm = T)
max(temp_test$mae_value)
min(temp_test$mae_value)




#PRODUCT4

### CODE FOR FIGURE 28
fig <- plot_ly(x=product4_data$event_date,y=product4_data$sold_count, mode="lines")
fig <- fig %>% layout(title = 'Sold Count by Date',
                      xaxis = list(title = 'Date'),
                      yaxis = list(title = 'Sold Count'))
fig

#Remove the part where it is not sold
unique(product4_data$product_content_id)
product4_data <- product4_data[event_date>"2019-09-08"]
product4_data <- product4_data[event_date<="2020-05-15"]

plot_ly(x=product4_data$event_date,y=product4_data$sold_count, mode="lines")
plot_ly(x=product4_data$event_date,y=product4_data$price, mode="lines")

acf(product4_data$sold_count, lag.max = 60)

#DATA MANIPULATION

#"2019-10-12" date has price = -1.00 AND sold_count = 0 but 0 value is bad for our measure calculations
product4_data[event_date=="2019-10-12"]$price = (product4_data[event_date=="2019-10-11"]$price + product4_data[event_date=="2019-10-13"]$price)/2
product4_data[event_date=="2019-10-12"]$sold_count = 1


# 11 Kasým Ýndirimler (9-10-11 Kasým)
product4_data[event_date == "2019-11-09"]$sold_count <- 
  (product4_data[event_date == "2019-11-06"]$sold_count + product4_data[event_date == "2019-11-12"]$sold_count) / 2

product4_data[event_date == "2019-11-10"]$sold_count <-
  (product4_data[event_date == "2019-11-07"]$sold_count + product4_data[event_date == "2019-11-13"]$sold_count) / 2

product4_data[event_date == "2019-11-11"]$sold_count <-
  (product4_data[event_date == "2019-11-08"]$sold_count + product4_data[event_date == "2019-11-14"]$sold_count) / 2

# Efsane Günler
product4_data[event_date == "2019-11-25"]$sold_count <-
  (product4_data[event_date == "2019-11-20"]$sold_count + product4_data[event_date == "2019-11-30"]$sold_count) / 2

product4_data[event_date == "2019-11-26"]$sold_count <- 
  (product4_data[event_date == "2019-11-21"]$sold_count + product4_data[event_date == "2019-12-01"]$sold_count) / 2


product4_data[event_date == "2019-11-27"]$sold_count <- 
  (product4_data[event_date == "2019-11-22"]$sold_count + product4_data[event_date == "2019-12-02"]$sold_count) / 2


product4_data[event_date == "2019-11-28"]$sold_count <-
  (product4_data[event_date == "2019-11-23"]$sold_count + product4_data[event_date == "2019-12-03"]$sold_count) / 2


product4_data[event_date == "2019-11-29"]$sold_count <- 
  (product4_data[event_date == "2019-11-24"]$sold_count + product4_data[event_date == "2019-12-04"]$sold_count) / 2


#Süper Mart 10-11-12 Mart
product4_data[event_date == "2020-03-10"]$sold_count <- 
  (product4_data[event_date == "2020-03-07"]$sold_count + product4_data[event_date == "2020-03-13"]$sold_count) / 2
product4_data[event_date == "2020-03-11"]$sold_count <-
  (product4_data[event_date == "2020-03-08"]$sold_count + product4_data[event_date == "2020-03-14"]$sold_count) / 2
product4_data[event_date == "2020-03-12"]$sold_count <- 
  (product4_data[event_date == "2020-03-09"]$sold_count + product4_data[event_date == "2020-03-15"]$sold_count) / 2

plot_ly(x=product4_data$event_date,y=product4_data$sold_count, mode="lines")
plot_ly(x=product4_data$event_date,y=product4_data$price, mode="lines", color = "red")
plot_ly(x=product4_data$event_date, y=product4_data$category_visits , mode="lines", color = "green")


# 5 February
product4_data[event_date == "2020-02-05"]$sold_count <- 
  (product4_data[event_date == "2020-02-04"]$sold_count + product4_data[event_date == "2020-02-06"]$sold_count) / 2



#PLOT
### CODE FOR FIGURE 29
fig2 <- plot_ly(x=product4_data$event_date,y=product4_data$sold_count, mode="lines")
fig2 <- fig2 %>% layout(title = 'Sold Count by Date',
                        xaxis = list(title = 'Date'),
                        yaxis = list(title = 'Sold Count'))
fig2

### CODE FOR FIGURE 30
acf(product4_data$sold_count, lag.max = 60, main = "Autocorrelation for Sold Count")

#BUILDING MODELS

#Lag2_sold count
product4_data[,lag2_sold_count:=shift(sold_count,2)]
#Lag3_sold_count
product4_data[,lag3_sold_count:=shift(sold_count,3)]
#lag2_price
product4_data[,lag2_price:=shift(price,2)]

#ADD GOOGLE TRENDS
#Sleepy Islak Mendil Search
google_search <- read.csv("/Users/enesozeren/Downloads/google_search_product4.csv")
data.table(google_search)
google_search$event_date <- as.Date(google_search$event_date)
product4_data <- merge(x = product4_data ,y = google_search, by="event_date", all=TRUE)

#The correlation between google_search and sold_count
cor(product4_data[event_date <= "2020-05-15", list(sold_count)], product4_data[event_date <= "2020-05-15", list(google_search)])

#lag4_google_search
product4_data[,lag4_google_search:=shift(google_search,4)]

#LINEAR MODEL

lr_model = lm(sold_count~ lag2_sold_count + lag3_sold_count + lag2_price + lag4_google_search, data = product4_data)
summary(lr_model)


#DECOMPOSING
#Model the trend
filtered_product4 <- product4_data[,list(sold_count, event_date)]
filtered_product4[,time_index:=1:.N] 
trend_lr <- lm(sold_count~time_index, filtered_product4)
summary(trend_lr)

filtered_product4[,lr_trend:=trend_lr$fitted]

### CODE FOR FIGURE 31
matplot(filtered_product4[,list(sold_count,lr_trend)], type="l", 
        xlab = "Time Index", ylab = "Sold Count", main = "The Trend in Sold Count")
legend("topright" , legend = c("Sold Count", "Trend"), col = c("black", "red"), cex=0.5, lty=1:1)



filtered_product4[,detrend_lr_sold_count:=sold_count - lr_trend]
matplot(filtered_product4$detrend_lr_sold_count, type = "l")


filtered_product4[,day_of_the_week:=weekdays(event_date)]
filtered_product4[,day_of_the_week_effect_lr:=mean(detrend_lr_sold_count, na.rm = T), by=list(day_of_the_week)]

### CODE FOR FIGURE 32
matplot(filtered_product4$day_of_the_week_effect_lr[1:7], type = "l",
        ylab = "Daily Effect", xlab = "Day of the week", main = "The day of the week effect")


filtered_product4[,residual:=detrend_lr_sold_count - day_of_the_week_effect_lr]
matplot(x=filtered_product4$event_date, y=filtered_product4$residual, type="l", color = "red", ylab = "Residuals", xlab = "Date")





#Forecast with model created by decomposition
test_start_date <- as.Date("2020-03-01")
test_end_date <- max(product4_data$event_date)

number_of_test_days=as.numeric(test_end_date - test_start_date)+1
predictions=vector('list', number_of_test_days)
predictions2=vector('list', number_of_test_days)
predictions3=vector('list', number_of_test_days)

#LOOP
for(i in 1:number_of_test_days){
  
  #TS MODEL
  #Model the trend
  forecast_day = test_start_date + i - 1
  filtered_product4 <- product4_data[event_date <= (forecast_day - 2) ,list(sold_count, event_date)]
  filtered_product4[,time_index:=1:.N] 
  trend_lr <- lm(sold_count~time_index, filtered_product4)
  filtered_product4[,lr_trend:=trend_lr$fitted]
  
  filtered_product4[,detrend_lr_sold_count:=sold_count - lr_trend]
  
  
  filtered_product4[,day_of_the_week:=weekdays(event_date)]
  filtered_product4[,day_of_the_week_effect_lr:=mean(detrend_lr_sold_count, na.rm = T), by=list(day_of_the_week)]
  
  #Calculate the Residuals
  filtered_product4[,residual:=detrend_lr_sold_count - day_of_the_week_effect_lr]
  
  my_prediction <- 
    as.numeric(trend_lr$coefficients[1] + trend_lr$coefficients[2]*(max(filtered_product4$time_index)+2)) +
    as.numeric(filtered_product4[event_date==forecast_day %m+% weeks(-1), list(day_of_the_week_effect_lr)])
  
  
  datam = product4_data[event_date==forecast_day, list(event_date)]
  datam[,Model_by_Trend_and_Seasonality:=my_prediction]
  #store in the list
  predictions[[i]] = datam
  
  
  ##LINEAR MODEL
  forecast_day = test_start_date + i - 1
  # filtering the forecast date's predictor values
  forecast_day_data = 
    product4_data[event_date==forecast_day,list(event_date, lag2_sold_count, lag3_sold_count, lag2_price, lag4_google_search)]
  
  # use available data which is data starting from yesterday (i.e. forecast_day - 2)
  lr_model_1 = lm(sold_count~ lag2_sold_count + lag3_sold_count + lag2_price + lag4_google_search, product4_data[event_date<=(forecast_day-2)])
  # predict using learned linear regression model
  lr_model_1_forecast = predict(lr_model_1, forecast_day_data)
  
  # add it to result data.table as a column
  forecast_day_data[,Linear_Regression_Model:=lr_model_1_forecast]
  
  #store in the list
  predictions2[[i]] = forecast_day_data
  
  
  #MEAN OF THE LR_MODEL AND TS_MODEL
  
  my_prediction2 <- 0.5*my_prediction + 0.5*as.numeric(forecast_day_data$Linear_Regression_Model)
  datam2 = product4_data[event_date==forecast_day, list(event_date)]
  datam2[,Ensembled_Model:=my_prediction2]
  
  predictions3[[i]] = datam2
}



# the list will be transformed to table and actual quantities are added
pred_datatable = rbindlist(predictions)
pred_datatable2 = rbindlist(predictions2)
pred_datatable3 = rbindlist(predictions3)
merged_pred_datatable = merge(product4_data[,list(event_date,sold_count)], pred_datatable[,list(event_date, Model_by_Trend_and_Seasonality)], by='event_date')
merged_pred_datatable = merge(merged_pred_datatable[,list(event_date, sold_count, Model_by_Trend_and_Seasonality)], pred_datatable2[, list(event_date, Linear_Regression_Model, lag2_sold_count, lag3_sold_count)], by="event_date")
merged_pred_datatable = merge(merged_pred_datatable[,list(event_date, sold_count, Model_by_Trend_and_Seasonality, Linear_Regression_Model, lag2_sold_count, lag3_sold_count)], pred_datatable3[,list(event_date, Ensembled_Model)], by="event_date")
head(merged_pred_datatable,5)

melted = melt(merged_pred_datatable, id.vars=c(1,2))
head(melted[event_date=="2020-04-24"])

#CALCULATING THE ERRORS
# first calculate daily errors for each model (i.e. variable) and date (i.e. event_data)
# you can check group by in the script below
summary_result = melted[,list(se=(value-sold_count)^2,
                              ad=abs(value-sold_count),
                              ape=abs(value-sold_count)/sold_count,sold_count),by=list(event_date,variable)]

# then summarize it by model level (i.e. variable)

### CODE FOR TABLE 6
summary_result[,list(mse=mean(se),mad=mean(ad),mape=mean(ape),ape_quantile90=quantile(ape,0.9)),by=list(variable)]


# WE TAKE THE MEAN OF ts_model and lr_model BECAUSE
### CODE FOR FIGURE 33
plot(merged_pred_datatable$event_date, merged_pred_datatable$sold_count, type = "l", col = "red", xlab = "Date", ylab = "Sold Count", main = "Sold Count and Predictions")
lines(merged_pred_datatable$event_date, merged_pred_datatable$Model_by_Trend_and_Seasonality, type = "l", col = "blue")
lines(merged_pred_datatable$event_date, merged_pred_datatable$Linear_Regression_Model, type = "l", col = "green")
legend("topright" , legend = c("Sold Count", "Linear Regression Model Prediction", "Model by Trend and Seasonality"), col = c("red", "green", "blue"), cex=0.5, lty=1:1)








#PRODUCT 6
unique(product6_data$product_content_id)
#Satýlmayan zamanlarý çýkar

### CODE FOR FIGURE 35
fig <- plot_ly(x=product6_data$event_date,y=product6_data$sold_count, mode="lines")
fig <- fig %>% layout(title = 'Sold Count by Date',
                      xaxis = list(title = 'Date'),
                      yaxis = list(title = 'Sold Count'))
fig

product6_data <- product6_data[event_date>"2019-06-19"]
product6_data <- product6_data[event_date<="2020-05-15"]
#Anlamsýz veriyi çýkar yani 2019-09-14 den baþlasýn 
plot_ly(x=product6_data$event_date,y=product6_data$sold_count, mode="lines")
product6_data <- product6_data[event_date>"2019-09-13"]
plot_ly(x=product6_data$event_date,y=product6_data$sold_count, mode="lines")


plot_ly(x=product6_data$event_date,y=product6_data$price, mode="lines")


#Price -1 olan günü düzelt
product6_data[event_date=="2019-12-12"]$price = (product6_data[event_date=="2019-12-11"]$price + product6_data[event_date=="2019-12-13"]$price)/2
product6_data[event_date=="2019-12-12"]$sold_count = (product6_data[event_date=="2019-12-11"]$sold_count + product6_data[event_date=="2019-12-13"]$sold_count)/2


# 11 Kasým Ýndirimler (9-10-11 Kasým)
product6_data[event_date == "2019-11-09"]$sold_count <-
  0.5*product6_data[event_date == "2019-11-06"]$sold_count + 0.5*product6_data[event_date == "2019-11-12"]$sold_count 

product6_data[event_date == "2019-11-10"]$sold_count <- 
  0.5*product6_data[event_date == "2019-11-07"]$sold_count + 0.5*product6_data[event_date == "2019-11-13"]$sold_count 

product6_data[event_date == "2019-11-11"]$sold_count <- 
  0.5*product6_data[event_date == "2019-11-08"]$sold_count + 0.5*product6_data[event_date == "2019-11-14"]$sold_count 


# Efsane Günler (a little overflowed)
product6_data[event_date == "2019-11-22"]$sold_count <-
  0.5*product6_data[event_date == "2019-11-14"]$sold_count + 0.5*product6_data[event_date == "2019-11-30"]$sold_count 

product6_data[event_date == "2019-11-23"]$sold_count <-
  0.5*product6_data[event_date == "2019-11-15"]$sold_count + 0.5*product6_data[event_date == "2019-12-01"]$sold_count 

product6_data[event_date == "2019-11-24"]$sold_count <-
  0.5*product6_data[event_date == "2019-11-16"]$sold_count + 0.5*product6_data[event_date == "2019-12-02"]$sold_count 

product6_data[event_date == "2019-11-25"]$sold_count <-
  0.5*product6_data[event_date == "2019-11-17"]$sold_count + 0.5*product6_data[event_date == "2019-12-03"]$sold_count 

product6_data[event_date == "2019-11-26"]$sold_count <-
  0.5*product6_data[event_date == "2019-11-18"]$sold_count + 0.5*product6_data[event_date == "2019-12-04"]$sold_count 

product6_data[event_date == "2019-11-27"]$sold_count <-
  0.5*product6_data[event_date == "2019-11-19"]$sold_count + 0.5*product6_data[event_date == "2019-12-05"]$sold_count 

product6_data[event_date == "2019-11-28"]$sold_count <-
  0.5*product6_data[event_date == "2019-11-20"]$sold_count + 0.5*product6_data[event_date == "2019-12-06"]$sold_count 

product6_data[event_date == "2019-11-29"]$sold_count <-
  0.5*product6_data[event_date == "2019-11-21"]$sold_count + 0.5*product6_data[event_date == "2019-12-07"]$sold_count 

#Süper Mart 10-11-12 Kasým
product6_data[event_date == "2020-03-10"]$sold_count <- 
  (product6_data[event_date == "2020-03-07"]$sold_count + product6_data[event_date == "2020-03-13"]$sold_count) / 2
product6_data[event_date == "2020-03-11"]$sold_count <-
  (product6_data[event_date == "2020-03-08"]$sold_count + product6_data[event_date == "2020-03-14"]$sold_count) / 2
product6_data[event_date == "2020-03-12"]$sold_count <- 
  (product6_data[event_date == "2020-03-09"]$sold_count + product6_data[event_date == "2020-03-15"]$sold_count) / 2


### CODE FOR FIGURE 36
fig2 <- plot_ly(x=product6_data$event_date,y=product6_data$sold_count, mode="lines")
fig2 <- fig2 %>% layout(title = 'Sold Count by Date',
                        xaxis = list(title = 'Date'),
                        yaxis = list(title = 'Sold Count'))
fig2

plot_ly(x=product6_data$event_date,y=product6_data$price, mode="lines")

### CODE FOR FIGURE 37
acf(product6_data$sold_count, lag.max = 60, main = "Autocorrelation for Sold Count")

#Lag2_sold count
product6_data[,lag2_sold_count:=shift(sold_count,2)]
#Lag3_sold_count
product6_data[,lag3_sold_count:=shift(sold_count,3)]
#Lag4_sold_count
product6_data[,lag4_sold_count:=shift(sold_count,4)]
#Lag5_sold_count
product6_data[,lag5_sold_count:=shift(sold_count,5)]
#lag2_price
product6_data[,lag2_price:=shift(price,2)]

#ADD GOOGLE TRENDS
#"bluetooth kulaklýk" search
google_search_product6 <- read.csv("/Users/enesozeren/Downloads/google_search_product6.csv")
data.table(google_search_product6)
google_search_product6$event_date <- as.Date(google_search_product6$event_date)
product6_data <- merge(x = product6_data ,y = google_search_product6, by="event_date", all=TRUE)


cor(product6_data[event_date <= "2020-05-15", list(sold_count)], product6_data[event_date <= "2020-05-15", list(google_search)])

#lag4_google_search
product6_data[,lag4_google_search:=shift(google_search,4)]

#LINEAR REGRESSION MODEL
lr_model <- lm(sold_count~ lag2_sold_count + lag4_google_search, product6_data)
summary(lr_model)


#DECOMPOSING
#Model the trend
filtered_product6 <- product6_data[,list(sold_count, event_date)]
filtered_product6[,time_index:=1:.N] 
trend_lr_6 <- lm(sold_count~time_index, filtered_product6)
summary(trend_lr_6)

filtered_product6[,lr_trend_6:=trend_lr_6$fitted]

### CODE FOR FIGURE 38
matplot(filtered_product6[,list(sold_count,lr_trend_6)], type="l", xlab = "Time", ylab = "Sold Count", main = "Sales Trend")


filtered_product6[,detrend_lr_sold_count:=sold_count - lr_trend_6]

plot_ly(x=filtered_product6$event_date, y=filtered_product6$detrend_lr_sold_count, mode="lines", color = "red")

#Model the Seasonlity
filtered_product6[,day_of_the_month:=mday(event_date)]
filtered_product6[,day_of_the_month_effect_lr:=mean(detrend_lr_sold_count, na.rm = T), by=list(day_of_the_month)]
### CODE FOR FIGURE 39
matplot(filtered_product6$day_of_the_month, filtered_product6$day_of_the_month_effect_lr, col = "brown" ,type = "h" ,xlab = "Day of the Month", ylab = "Day of the Month Effect", main = "Day of the Month Effect")
?matplot

filtered_product6[,without_day_of_the_month_effect:=detrend_lr_sold_count - day_of_the_month_effect_lr]
plot_ly(x=filtered_product6$event_date, y=filtered_product6$without_day_of_the_month_effect, mode="lines", color = "red")


filtered_product6[,day_of_the_week:=weekdays(event_date)]
filtered_product6[,day_of_the_week_effect_lr:=mean(without_day_of_the_month_effect, na.rm = T), by=list(day_of_the_week)]
matplot(filtered_product6$day_of_the_week_effect_lr, type = "l", xlab = "Time", ylab = "Day of the Week Effect")



filtered_product6[,residual:=detrend_lr_sold_count - day_of_the_month_effect_lr - day_of_the_week_effect_lr]
matplot(x=filtered_product6$event_date, y=filtered_product6$residual, type="l",  xlab = "Time", ylab = "Residuals")



#EVALUATION OF MODELS
test_start_date <- as.Date("2020-03-01")
test_end_date <- max(product6_data$event_date)

number_of_test_days=as.numeric(test_end_date - test_start_date)+1
predictions=vector('list', number_of_test_days)
predictions2=vector('list', number_of_test_days)
predictions3=vector('list', number_of_test_days)



#DENEME
for(i in 1:number_of_test_days){
  
  #TS MODEL
  #Model the trend
  forecast_day = test_start_date + i - 1
  filtered_product6 <- product6_data[event_date <= (forecast_day - 2) ,list(sold_count, event_date)]
  filtered_product6[,time_index:=1:.N] 
  trend_lr <- lm(sold_count~time_index, filtered_product6)
  filtered_product6[,lr_trend:=trend_lr$fitted]
  
  filtered_product6[,detrend_lr_sold_count:=sold_count - lr_trend]
  
  #Model the Seasonlity
  filtered_product6[,day_of_the_week:=weekdays(event_date)]
  filtered_product6[,day_of_the_week_effect_lr:=mean(detrend_lr_sold_count, na.rm = T), by=list(day_of_the_week)]
  
  #Calculate the Residuals
  filtered_product6[,residual:=detrend_lr_sold_count - day_of_the_week_effect_lr]
  
  my_prediction <- 
    as.numeric(trend_lr$coefficients[1] + trend_lr$coefficients[2]*(max(filtered_product6$time_index)+2)) +
    as.numeric(filtered_product6[event_date==forecast_day %m+% weeks(-1), list(day_of_the_week_effect_lr)])
  
  
  datam = product6_data[event_date==forecast_day, list(event_date)]
  datam[,ts_model_prediction:=my_prediction]
  #store in the list
  predictions[[i]] = datam
  
  
  ##LINEAR MODEL
  forecast_day = test_start_date + i - 1
  # filtering the forecast date's predictor values
  forecast_day_data = 
    product6_data[event_date==forecast_day,list(event_date, lag2_sold_count, lag3_sold_count, lag2_price, lag4_sold_count, lag4_google_search)]
  
  # use available data which is data starting from yesterday (i.e. forecast_day - 2)
  lr_model_1 = lm(sold_count~ lag2_sold_count + lag3_sold_count + lag2_price + lag4_google_search, product6_data[event_date<=(forecast_day-2)])
  # predict using learned linear regression model
  lr_model_1_forecast = predict(lr_model_1, forecast_day_data)
  
  # add it to result data.table as a column
  forecast_day_data[,lr_model_1_forecast:=lr_model_1_forecast]
  
  #store in the list
  predictions2[[i]] = forecast_day_data
  
  
  #MEAN OF THE LR_MODEL AND TS_MODEL
  
  my_prediction2 <- 
    0.4*product6_data[event_date==forecast_day,list(lag2_sold_count)] +  0.4*product6_data[event_date==forecast_day,list(lag3_sold_count)] + 0.2*lr_model_1_forecast
  datam2 = product6_data[event_date==forecast_day, list(event_date)]
  datam2[,ensembled_model:=my_prediction2]
  
  predictions3[[i]] = datam2
}



head(predictions)


# the list will be transformed to table and actual quantities are added
pred_datatable = rbindlist(predictions)
pred_datatable2 = rbindlist(predictions2)
pred_datatable3 = rbindlist(predictions3)
merged_pred_datatable = merge(product6_data[,list(event_date,sold_count)], pred_datatable[,list(event_date, ts_model_prediction)], by='event_date')
merged_pred_datatable = merge(merged_pred_datatable[,list(event_date, sold_count, ts_model_prediction)], pred_datatable2[, list(event_date, lr_model_1_forecast, lag2_sold_count, lag3_sold_count)], by="event_date")
merged_pred_datatable = merge(merged_pred_datatable[,list(event_date, sold_count, ts_model_prediction, lr_model_1_forecast, lag2_sold_count, lag3_sold_count)], pred_datatable3[,list(event_date, ensembled_model)], by="event_date")
head(merged_pred_datatable,5)

melted = melt(merged_pred_datatable, id.vars=c(1,2))
head(melted[event_date=="2020-04-07"])

#CALCULATING THE ERRORS
# first calculate daily errors for each model (i.e. variable) and date (i.e. event_data)
# you can check group by in the script below
summary_result = melted[,list(se=(value-sold_count)^2,
                              ad=abs(value-sold_count),
                              ape=abs(value-sold_count)/sold_count,sold_count),by=list(event_date,variable)]

# then summarize it by model level (i.e. variable)
### CODE FOR TABLE 7
summary_result[,list(mse=mean(se),mad=mean(ad),mape=mean(ape),quantile90=quantile(ape,0.9)),by=list(variable)]




plot(merged_pred_datatable$event_date, merged_pred_datatable$sold_count, type = "l", col = "black")
lines(merged_pred_datatable$event_date, merged_pred_datatable$ensembled_model, type = "l", col = "red")




# install the required packages first
install.packages("jsonlite")
install.packages("httr")
require(jsonlite)
require(httr)
require(data.table)
library(xts)
library(Hmisc)
library(caTools)
library(ModelMetrics)
install.packages("MLmetrics")
library(MLmetrics)
library(forecast)

vacuumindex=data$product_content_id=="7061886"
vacuum=data[vacuumindex]

test.vacuum=data.table(vacuum$event_date,vacuum$sold_count,vacuum$price,names=c("sold","price"))

test.vacuum
vacuum.ts=as.ts(test.vacuum$vacuum.sold_count,frequency=7)
decompose(vacuum.ts)
xts.vacuum=as.xts.data.table(test.vacuum)
vacuum2=vacuum$event_date>"2019-07-26"
vacuum3=vacuum[vacuum2]
vacuum.sold=vacuum3$sold_count
ts.vacuum=ts(vacuum.sold,frequency=7)
ts.vacuum[106]=80


logvac=log(diff(ts.vacuum))

dc.vacuum=decompose(ts.vacuum,"additive")
dclog.vacuum=decompose(logvac,)
plot(dc.vacuum)
plot(dclog.vacuum)

vacuumnet=vacuum$event_date>"2019-07-26"
vacuumnet=vacuum[vacuumnet]
vacuumnet[event_date=="2019-09-14"]$price <- 185
vacuumnet[event_date=="2019-09-15"]$price <- 188
vacuumnet[event_date=="2019-09-16"]$price <- 191

vacuumnet[event_date=="2019-09-14"]$sold_count <- 1
vacuumnet[event_date=="2019-09-15"]$sold_count <- 1
vacuumnet[event_date=="2019-09-16"]$sold_count <- 2

vacuumnet[128:129]$price=c(243,247)
vacuumnet[128:129]$sold_count=c(2,3)

vacuumnet[131:132]$price=c(235,219)
vacuumnet[131:132]$sold_count=c(5,6)

mean(vacuumnet$price)

vacuumbackup=vacuumnet


vacuumnet$forecast=Lag(vacuumnet$sold_count,shift=-2)
vacuumnet$pricel1=Lag(vacuumnet$price,shift = -1)

set.seed(50)


split=sample.split(vacuumnet$sold_count,SplitRatio = 0.8)
vacuumtrain=subset(vacuumnet,split==TRUE)
vacuumtest=subset(vacuumnet,split==FALSE)
vacuum_org=lm(forecast~price+sold_count+visit_count,data=vacuumtrain)
vacuum_org
vacuum_org1=lm(forecast~price+sold_count+visit_count+favored_count+basket_count+category_sold+category_brand_sold+pricel1,data=vacuumtrain)
vacuum_org1


vacuummod1=vacuumnet
vacuummod1[106]$sold_count=120

vacuummod1$forecast=Lag(vacuummod1$sold_count,shift=-2)
vacuummod1$pricel1=Lag(vacuummod1$price,shift = -1)
split1=sample.split(vacuummod1$sold_count,SplitRatio = 0.8)
vacuumtrainmod=subset(vacuummod1,split==TRUE)
vacuumtestmod=subset(vacuummod1,split==FALSE)
vacuum_lmod=lm(forecast~price+sold_count+visit_count+pricel1,data=vacuumtrainmod)
vacuum_lmod

ts.plot(as.ts(vacuummod1$sold_count))

values1=predict(vacuum_lmod,newdata = vacuumtest)
values1
maevac1=mean(sqrt((values1-vacuumtest$forecast)^2))
maevac1
RMSE(values1,vacuumtest$forecast)

#smoothing sequence with bad results unused in report
#vacuummehmet=vacuumnet
#vacuummehmet[106:107]$sold_count=c(80,40)
#vacuummehmet[46:48]$sold_count=c(10,15,10)
#vacuummehmet[147]$sold_count=60
#vacuummehmet[193:195]$sold_count=c(45,55,54)
#vacuummehmet[281:286]$sold_count=c(90,92,100,122,120,95)
#vacuummehmet[228:230]$sold_count=c(60,40,45)
#vacuummehmet[262]$sold_count=130
#vacuummehmet[326:328]$sold_count=c(91,90,110)

#ts.plot(as.ts(vacuummehmet$sold_count))
#set.seed(135)
#splitme=sample.split(vacuummehmet$sold_count,SplitRatio = 0.8)
#vacuumtrainme=subset(vacuummehmet,splitme==TRUE)
#vacuumtestme=subset(vacuummehmet,splitme==FALSE)
#vacuum_lme=lm(forecast~price+sold_count+visit_count,data=vacuumtrainme)
#vacuum_lme
#valuesme=predict(vacuum_lme,newdata = vacuumtestme)
#valuesme
#maevacme=mean(sqrt((valuesme-vacuumtestme$forecast)^2),na.rm = T)
#maevacme

#smoothing sequence with bad results unused in report


vacuum.arima=data.table()
vacuum.arima$date=vacuumnet$event_date
vacuum.arima$sold=vacuumnet$sold_count
vacuum.arima=as.xts(vacuum.arima)
plot.xts(vacuum.arima$sold,main = "Number of Sold Vacuum Cleaners")

vacuummehmet
arimavactrain=vacuumnet[1:337]
arimavactest=vacuumnet[337:343]
arimavactrain[106]$sold_count=120

lmtrain=vacuumnet[1:335]
lmtest=vacuumnet[335:343]

lmfinal=lm(forecast~price+sold_count+visit_count+pricel1,data=lmtrain)
lmfinal
lmpredfinal=predict(lmfinal,newdata=lmtest)
lmpredfinal=lmpredfinal[1:7]
lmpredfinal
RMSE(lmpredfinal,arimavactest$sold_count)

dcvac0=decompose(ts(vacuumnet$sold_count,frequency = 7))
plot(dcvac0)
dcvac0

dcvac=decompose(ts(arimavactrain$sold_count,frequency = 7))
plot(dcvac)
dcvac
dcvac$trend[1:3]=dcvac$trend[4]
dcvac$trend[335:337]=dcvac$trend[334]
randomvac=arimavactrain$sold_count-dcvac$trend-dcvac$seasonal
arimavac=arima(randomvac,order = c(5,0,2))
arimavac
vacforarima=forecast(arimavac,h=7)
vacforarima
valuesarimavac=vacforarima$mean+dcvac$seasonal[2:8]+42.571429
valuesarimavac
RMSE(valuesarimavac,arimavactest$sold_count)
RMSE(vacuumnet[336:342]$sold_count,arimavactest$sold_count)
MAPE(valuesme,vacuumtestme$forecast)

RMSE((valuesarimavac+lmpredfinal+vacuumnet[336:342]$sold_count)/3,arimavactest$sold_count)

real_values_vacuum=arimavactest$sold_count
arima_values_vacuum=valuesarimavac
linear_values_vacuum=lmpredfinal
naive_vacuum=vacuumnet[336:342]$sold_count
ensembled_model=(valuesarimavac+lmpredfinal+vacuumnet[336:342]$sold_count)/3

RMSE(naive_vacuum,real_values_vacuum)
RMSE(linear_values_vacuum,real_values_vacuum)
RMSE(arima_values_vacuum,real_values_vacuum)
RMSE(ensembled_model,real_values_vacuum)

MAPE(naive_vacuum,real_values_vacuum)
MAPE(linear_values_vacuum,real_values_vacuum)
MAPE(arima_values_vacuum,real_values_vacuum)
MAPE(ensembled_model,real_values_vacuum)



naivevacuum=vacuumnet
maevacnaive=mean(sqrt((vacuumtest$sold_count-vacuumtest$forecast)^2),na.rm = T)
maevacnaive
RMSE(vacuumtest$sold_count,vacuumtest$forecast)



#bikini

bikiniindex=data$product_content_id=="5926527"
bikini=data[bikiniindex]
bikini=bikini[bikini$event_date>"2019-05-11"]
plot.ts(as.ts(bikini$sold_count))


bikinixts=data.table()
bikinixts$date=bikini$event_date
bikinixts$sold=bikini$sold_count
plot.xts(as.xts(bikinixts),main = "Number of Sold Bikini Tops")

bikiniany=bikini[bikini$price!=(-1)]
bikini$forecast=Lag(bikini$sold_count,shift=(-2))
bikini$lag1pr=Lag(bikini$price,shift=(-1))

splitbikini=sample.split(bikini$sold_count,SplitRatio = 0.8)
bikinitrain=subset(bikini,splitbikini==TRUE)
bikinitest=subset(bikini,splitbikini==FALSE)
bikinilm=lm(forecast~sold_count,data = bikinitrain)
bikinilm

bikinival=predict(bikinilm,newdata = bikinitest)
bikinival
mse(bikinival,bikinitest$forecast)

todcbikini=ts(bikini$sold_count,frequency = 7)
dcbikini=decompose(todcbikini)
plot(dcbikini)
dcbikini[417:419]$trend=dcbikini[416]$trend

bikiniarima_train=bikini[1:410]
bikiniarima_test=bikini[411:419]
tsmsbikini=ts(bikiniarima_train$sold_count,frequency = 7)
andcbikini=decompose(tsmsbikini)
plot(andcbikini)
andcbikini$trend[408:410]=andcbikini$trend[407]
randombikini=tsmsbikini-andcbikini$seasonal-andcbikini$trend

arimabikini=arima(randombikini,order = c(1,0,1))
values=forecast(arimabikini,h=7)
testbikini=values$mean+andcbikini$seasonal[5:11]+0.7142857
testbikini

hasbikinilm=lm(forecast~sold_count+visit_count+favored_count,data = bikiniarima_train)
hasbikinilm
haspredictions=predict(hasbikinilm,newdata = bikiniarima_test)
haspredictions
realvalues=bikiniarima_test[3:9]$sold_count
haspredictions=haspredictions[1:7]

lmbikini_pred=haspredictions
arimabikini_pred=testbikini
naivebikini_pred=bikiniarima_test[1:7]$sold_count
ensembledbikini=(lmbikini_pred+arimabikini_pred)/2

rmse(lmbikini_pred,realvalues)
rmse(arimabikini_pred,realvalues)
rmse(naivebikini_pred,realvalues)
rmse(ensembledbikini,realvalues)


