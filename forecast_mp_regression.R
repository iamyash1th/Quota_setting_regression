
forecast_input_2012_13_14<-read.csv("C:/Target setting/forecast_mp_model/Forecast_units_model/forecast_input_2012-13-14.csv")

str(forecast_input_2012_13_14)

forecast_input_2012_13_14_t1<-forecast_input_2012_13_14[forecast_input_2012_13_14$Terr_masked==1,]
forecast_input_2012_13_14_t2<-forecast_input_2012_13_14[forecast_input_2012_13_14$Terr_masked==2,]
forecast_input_2012_13_14_t3<-forecast_input_2012_13_14[forecast_input_2012_13_14$Terr_masked==3,]
forecast_input_2012_13_14_t4<-forecast_input_2012_13_14[forecast_input_2012_13_14$Terr_masked==4,]
forecast_input_2012_13_14_t5<-forecast_input_2012_13_14[forecast_input_2012_13_14$Terr_masked==5,]
forecast_input_2012_13_14_t6<-forecast_input_2012_13_14[forecast_input_2012_13_14$Terr_masked==6,]
forecast_input_2012_13_14_t7<-forecast_input_2012_13_14[forecast_input_2012_13_14$Terr_masked==7,]
forecast_input_2012_13_14_t8<-forecast_input_2012_13_14[forecast_input_2012_13_14$Terr_masked==8,]

forecast_input_2012_13_14_t1<-forecast_input_2012_13_14_t1[,3]
forecast_input_2012_13_14_t2<-forecast_input_2012_13_14_t2[,3]
forecast_input_2012_13_14_t3<-forecast_input_2012_13_14_t3[,3]
forecast_input_2012_13_14_t4<-forecast_input_2012_13_14_t4[,3]
forecast_input_2012_13_14_t5<-forecast_input_2012_13_14_t5[,3]
forecast_input_2012_13_14_t6<-forecast_input_2012_13_14_t6[,3]
forecast_input_2012_13_14_t7<-forecast_input_2012_13_14_t7[,3]
forecast_input_2012_13_14_t8<-forecast_input_2012_13_14_t8[,3]


#Convert data to time-series object
forecast_input_2012_13_14_t1_ts<-ts(forecast_input_2012_13_14_t1,frequency=12, start=c(2012,1))
forecast_input_2012_13_14_t2_ts<-ts(forecast_input_2012_13_14_t2,frequency=12, start=c(2012,1))
forecast_input_2012_13_14_t3_ts<-ts(forecast_input_2012_13_14_t3,frequency=12, start=c(2012,1))
forecast_input_2012_13_14_t4_ts<-ts(forecast_input_2012_13_14_t4,frequency=12, start=c(2012,1))
forecast_input_2012_13_14_t5_ts<-ts(forecast_input_2012_13_14_t5,frequency=12, start=c(2012,1))
forecast_input_2012_13_14_t6_ts<-ts(forecast_input_2012_13_14_t6,frequency=12, start=c(2012,1))
forecast_input_2012_13_14_t7_ts<-ts(forecast_input_2012_13_14_t7,frequency=12, start=c(2012,1))
forecast_input_2012_13_14_t8_ts<-ts(forecast_input_2012_13_14_t8,frequency=12, start=c(2012,1))



#Creating a Holt-Winters exponential smoothing model to find parameters alpha(level), beta(trend), gamma(seasonal)
forecast_model_2012_13_14_t1_ts<-HoltWinters(forecast_input_2012_13_14_t1_ts)
forecast_model_2012_13_14_t2_ts<-HoltWinters(forecast_input_2012_13_14_t2_ts)
forecast_model_2012_13_14_t3_ts<-HoltWinters(forecast_input_2012_13_14_t3_ts)
library(forecast)

forecast_model_2012_13_14_t4_ts<-hw(forecast_input_2012_13_14_t4_ts)


forecast_model_2012_13_14_t5_ts<-HoltWinters(forecast_input_2012_13_14_t5_ts)
forecast_model_2012_13_14_t6_ts<-HoltWinters(forecast_input_2012_13_14_t6_ts)
forecast_model_2012_13_14_t7_ts<-HoltWinters(forecast_input_2012_13_14_t7_ts)
forecast_model_2012_13_14_t8_ts<-HoltWinters(forecast_input_2012_13_14_t8_ts)



#Model created only smooths values for existing time period. To predict future sales-

predict_sales_2014_h2_t1_ts <- predict(forecast_model_2012_13_14_t1_ts, 6)
predict_sales_2014_h2_t2_ts <- predict(forecast_model_2012_13_14_t2_ts, 6)
predict_sales_2014_h2_t3_ts <- predict(forecast_model_2012_13_14_t3_ts, 6)
predict_sales_2014_h2_t4_ts <- predict(forecast_model_2012_13_14_t4_ts, 6)
predict_sales_2014_h2_t4_ts<-ts(predict_sales_2014_h2_t4_ts$mean[1:6],frequency=12,start=c(2014,7))
predict_sales_2014_h2_t5_ts <- predict(forecast_model_2012_13_14_t5_ts, 6)
predict_sales_2014_h2_t6_ts <- predict(forecast_model_2012_13_14_t6_ts, 6)
predict_sales_2014_h2_t7_ts <- predict(forecast_model_2012_13_14_t7_ts, 6)
predict_sales_2014_h2_t8_ts <- predict(forecast_model_2012_13_14_t8_ts, 6)

predict_sales_2014_h2<-data.frame(predict_sales_2014_h2_t1_ts,predict_sales_2014_h2_t2_ts,predict_sales_2014_h2_t3_ts,predict_sales_2014_h2_t4_ts,predict_sales_2014_h2_t5_ts,predict_sales_2014_h2_t6_ts,predict_sales_2014_h2_t7_ts,predict_sales_2014_h2_t8_ts)

colnames(predict_sales_2014_h2)<-c("t1","t2","t3","t4","t5","t6","t7","t8")

#Repeating the exercise with different years of data

forecast_input_2013_14_15<-read.csv("C:/Target setting/forecast_mp_model//Forecast_units_model/forecast_input_2013_14_15.csv")

str(forecast_input_2013_14_15)

forecast_input_2013_14_15_t1<-forecast_input_2013_14_15[forecast_input_2013_14_15$Terr_masked==1,]
forecast_input_2013_14_15_t2<-forecast_input_2013_14_15[forecast_input_2013_14_15$Terr_masked==2,]
forecast_input_2013_14_15_t3<-forecast_input_2013_14_15[forecast_input_2013_14_15$Terr_masked==3,]
forecast_input_2013_14_15_t4<-forecast_input_2013_14_15[forecast_input_2013_14_15$Terr_masked==4,]
forecast_input_2013_14_15_t5<-forecast_input_2013_14_15[forecast_input_2013_14_15$Terr_masked==5,]
forecast_input_2013_14_15_t6<-forecast_input_2013_14_15[forecast_input_2013_14_15$Terr_masked==6,]
forecast_input_2013_14_15_t7<-forecast_input_2013_14_15[forecast_input_2013_14_15$Terr_masked==7,]
forecast_input_2013_14_15_t8<-forecast_input_2013_14_15[forecast_input_2013_14_15$Terr_masked==8,]

forecast_input_2013_14_15_t1<-forecast_input_2013_14_15_t1[,3]
forecast_input_2013_14_15_t2<-forecast_input_2013_14_15_t2[,3]
forecast_input_2013_14_15_t3<-forecast_input_2013_14_15_t3[,3]
forecast_input_2013_14_15_t4<-forecast_input_2013_14_15_t4[,3]
forecast_input_2013_14_15_t5<-forecast_input_2013_14_15_t5[,3]
forecast_input_2013_14_15_t6<-forecast_input_2013_14_15_t6[,3]
forecast_input_2013_14_15_t7<-forecast_input_2013_14_15_t7[,3]
forecast_input_2013_14_15_t8<-forecast_input_2013_14_15_t8[,3]


#Convert data to time-series object
forecast_input_2013_14_15_t1_ts<-ts(forecast_input_2013_14_15_t1,frequency=12, start=c(2013,1))
forecast_input_2013_14_15_t2_ts<-ts(forecast_input_2013_14_15_t2,frequency=12, start=c(2013,1))
forecast_input_2013_14_15_t3_ts<-ts(forecast_input_2013_14_15_t3,frequency=12, start=c(2013,1))
forecast_input_2013_14_15_t4_ts<-ts(forecast_input_2013_14_15_t4,frequency=12, start=c(2013,1))
forecast_input_2013_14_15_t5_ts<-ts(forecast_input_2013_14_15_t5,frequency=12, start=c(2013,1))
forecast_input_2013_14_15_t6_ts<-ts(forecast_input_2013_14_15_t6,frequency=12, start=c(2013,1))
forecast_input_2013_14_15_t7_ts<-ts(forecast_input_2013_14_15_t7,frequency=12, start=c(2013,1))
forecast_input_2013_14_15_t8_ts<-ts(forecast_input_2013_14_15_t8,frequency=12, start=c(2013,1))



#Creating a Holt-Winters exponential smoothing model to find parameters alpha(level), beta(trend), gamma(seasonal)
forecast_model_2013_14_15_t1_ts<-HoltWinters(forecast_input_2013_14_15_t1_ts)
forecast_model_2013_14_15_t2_ts<-HoltWinters(forecast_input_2013_14_15_t2_ts)
forecast_model_2013_14_15_t3_ts<-HoltWinters(forecast_input_2013_14_15_t3_ts)
forecast_model_2013_14_15_t4_ts<-hw(forecast_input_2013_14_15_t4_ts)
forecast_model_2013_14_15_t5_ts<-HoltWinters(forecast_input_2013_14_15_t5_ts)
forecast_model_2013_14_15_t6_ts<-HoltWinters(forecast_input_2013_14_15_t6_ts)
forecast_model_2013_14_15_t7_ts<-HoltWinters(forecast_input_2013_14_15_t7_ts)
forecast_model_2013_14_15_t8_ts<-HoltWinters(forecast_input_2013_14_15_t8_ts)



#Model created only smooths values for existing time period. To predict future sales-

predict_sales_2015_h2_t1_ts <- predict(forecast_model_2013_14_15_t1_ts, 6)
predict_sales_2015_h2_t2_ts <- predict(forecast_model_2013_14_15_t2_ts, 6)
predict_sales_2015_h2_t3_ts <- predict(forecast_model_2013_14_15_t3_ts, 6)
predict_sales_2015_h2_t4_ts <- predict(forecast_model_2013_14_15_t4_ts, 6)
predict_sales_2015_h2_t4_ts<-ts(predict_sales_2015_h2_t4_ts$mean[1:6],frequency=12,start=c(2015,7))
predict_sales_2015_h2_t5_ts <- predict(forecast_model_2013_14_15_t5_ts, 6)
predict_sales_2015_h2_t6_ts <- predict(forecast_model_2013_14_15_t6_ts, 6)
predict_sales_2015_h2_t7_ts <- predict(forecast_model_2013_14_15_t7_ts, 6)
predict_sales_2015_h2_t8_ts <- predict(forecast_model_2013_14_15_t8_ts, 6)



predict_sales_2015_h2<-data.frame(predict_sales_2015_h2_t1_ts,predict_sales_2015_h2_t2_ts,predict_sales_2015_h2_t3_ts,predict_sales_2015_h2_t4_ts,predict_sales_2015_h2_t5_ts,predict_sales_2015_h2_t6_ts,predict_sales_2015_h2_t7_ts,predict_sales_2015_h2_t8_ts)

colnames(predict_sales_2015_h2)<-c("t1","t2","t3","t4","t5","t6","t7","t8")

#After collating predict_sales_2015_h2 and predict_sales_2014_h2, imported the file back to R. 
#Columns are Terr_masked	yearmonth	HoltWinters	market_potential	Actuals

forecast6_mp_input<-read.csv("C:/Target setting/forecast_mp_model/model_input_units_forecast6_mp_sales.csv")


#Making territory and quarter as categorical variables
forecast6_mp_input$territory<-factor(forecast6_mp_input$Terr_masked)
str(forecast6_mp_input)
forecast6_mp_input<-forecast6_mp_input[,-1]

#Make train and test datasets 
train_sample<- sample(nrow(forecast6_mp_input), floor(nrow(forecast6_mp_input) * 0.9))

forecast6_mp_input_train<-forecast6_mp_input[train_sample,]

forecast6_mp_input_test<-forecast6_mp_input[-train_sample,]

#************Building linear regression models with different combinations of variables***************

#Predictors: Current month forecast and market potential with intercept
forecast6_mp_input_train_v2<-forecast6_mp_input_train[,c(2,3,4)]

forecast6_mp_input_train_v2
plot(forecast6_mp_input_train_v2)

model_forecast6_mp<-lm(Actuals~HoltWinters+market_potential,forecast6_mp_input_train_v2)
summary(model_forecast6_mp)

#Predictors: Current month forecast and market potential without intercept

model_forecast6_mp_no_int<-lm(Actuals~HoltWinters+market_potential-1,forecast6_mp_input_train_v2)
summary(model_forecast6_mp_no_int)


plot(model_forecast6_mp_no_int)


predict_model_forecast6_mp_no_int<-predict(model_forecast6_mp_no_int,forecast6_mp_input_test)

results_model_forecast6_mp_no_int<-data.frame(predict_model_forecast6_mp_no_int,forecast6_mp_input_test[,4])

write.csv(results_model_forecast6_mp_no_int,"C:/Target setting/forecast_mp_model/results_model_forecast6_units_mp_no_int.csv")

#Predictors: Current month forecast, market potential, territory without intercept
forecast6_mp_input_train_v3<-forecast6_mp_input_train[,c(2,3,4,5)]

model_forecast6_mp_terr_no_int<-lm(Actuals~HoltWinters+market_potential+territory-1,forecast6_mp_input_train_v3)
summary(model_forecast6_mp_terr_no_int)

#Predictors: Current month forecast, market potential, month of year without intercept
forecast6_mp_input_train_v4<-forecast6_mp_input_train[,c(1,2,3,4)]

forecast6_mp_input_train_v4$monthofyear<-factor(months.Date(as.Date(as.character(forecast6_mp_input_train_v4$yearmonth),"%m/%d/%Y")))

str(forecast6_mp_input_train_v4)

model_forecast6_mp_month_no_int<-lm(Actuals~HoltWinters+market_potential+monthofyear-1,forecast6_mp_input_train_v4)

summary(model_forecast6_mp_month_no_int)






