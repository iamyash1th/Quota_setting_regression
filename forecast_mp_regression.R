

#After doing some manipulation on excel, imported the file back to R. 
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






