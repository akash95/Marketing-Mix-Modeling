rm(list=ls())
# install.packages("pracma")
library(readxl)
library(Hmisc) 
library(pracma) 
###load data
data = read_excel("HW2_MultimediaHW.xlsx")

###obtain summary stats for the data
colnames(data)[2] = 'Sales'
summary(data)
colnames(data)


### Check if data is missing
is.na(data)
colSums(is.na(data))

cor(data)

### Delete the columns with many 0's
data_new  = subset(data, select = -c(SocialMedia, Banner, Retargeting, Months))

cor(data_new)

# Not considering Banner, social media and retargeting for future calculations as they have many 0 values


####Generate the Variables of interest
Sales = data$Sales
Lag_Sales = Lag(Sales,shift=1) #creating the lag of sales value
Mailings = data$Mailings
ADV_Total = data$ADV_Total
ADV_Offline = data$ADV_Offline
Catalogs_ExistCust = data$Catalogs_ExistCust
Catalogs_Winback = data$Catalogs_Winback
Catalogs_NewCust = data$Catalogs_NewCust
ADV_Online = data$ADV_online
#Banner = data$Banner
Search = data$Search
#SocialMedia = data$SocialMedia
Newsletter = data$Newsletter
#Retargeting = data$Retargeting
Portals = data$Portals

###Diminishing Returns
Sqrt_Mailings <- sqrt(Mailings)
Sqrt_ADV_Total <- sqrt(ADV_Total)
Sqrt_ADV_Offline <- sqrt(ADV_Offline)
Sqrt_Catalogs_ExistCust <- sqrt(Catalogs_ExistCust)
Sqrt_Catalogs_Winback <- sqrt(Catalogs_Winback)
Sqrt_Catalogs_NewCust <- sqrt(Catalogs_NewCust)
Sqrt_ADV_Online <- sqrt(ADV_Online)
#Sqrt_Banner <- sqrt(Banner)
Sqrt_Search <- sqrt(Search)
#Sqrt_SocialMedia <- sqrt(SocialMedia)
Sqrt_Newsletter <- sqrt(Newsletter)
#Sqrt_Retargeting <- sqrt(Retargeting)
Sqrt_Portals <- sqrt(Portals)

Cuberoot_Mailings <- nthroot(Mailings, 3)
Cuberoot_ADV_Total <- nthroot(ADV_Total, 3)
Cuberoot_ADV_Offline <- nthroot(ADV_Offline, 3)
Cuberoot_Catalogs_ExistCust <- nthroot(Catalogs_ExistCust, 3)
Cuberoot_Catalogs_Winback <- nthroot(Catalogs_Winback, 3)
Cuberoot_Catalogs_NewCust <- nthroot(Catalogs_NewCust, 3)
Cuberoot_ADV_Online <- nthroot(ADV_Online, 3)
#Cuberoot_Banner <- nthroot(Banner, 3)
Cuberoot_Search <- nthroot(Search, 3)
#Cuberoot_SocialMedia <- nthroot(SocialMedia, 3)
Cuberoot_Newsletter <- nthroot(Newsletter, 3)
#Cuberoot_Retargeting <- nthroot(Retargeting, 3)
Cuberoot_Portals <- nthroot(Portals, 3)

Log_Mailings <- log(1+Mailings)
Log_ADV_Total <- log(1+ADV_Total)
Log_ADV_Offline <- log(1+ADV_Offline)
Log_Catalogs_ExistCust <- log(1+Catalogs_ExistCust)
Log_Catalogs_Winback <- log(1+Catalogs_Winback)
Log_Catalogs_NewCust <- log(1+Catalogs_NewCust)
Log_ADV_Online <- log(1+ADV_Online)
#Log_Banner <- log(1+Banner)
Log_Search <- log(1+Search)
#Log_SocialMedia <- log(1+SocialMedia)
Log_Newsletter <- log(1+Newsletter)
#Log_Retargeting <- log(1+Retargeting)
Log_Portals <- log(1+Portals)

Inv_Mailings <- 1/(1+Mailings)
Inv_ADV_Total <- 1/(1+ADV_Total)
Inv_ADV_Offline <- 1/(1+ADV_Offline)
Inv_Catalogs_ExistCust <- 1/(1+Catalogs_ExistCust)
Inv_Catalogs_Winback <- 1/(1+Catalogs_Winback)
Inv_Catalogs_NewCust <- 1/(1+Catalogs_NewCust)
Inv_ADV_Online <- 1/(1+ADV_Online)
#Inv_Banner <- 1/(1+Banner)
Inv_Search <- 1/(1+Search)
#Inv_SocialMedia <- 1/(1+SocialMedia)
Inv_Newsletter <- 1/(1+Newsletter)
#Inv_Retargeting <- 1/(1+Retargeting)
Inv_Portals <- 1/(1+Portals)


df_log = data.frame(Sales,Log_Mailings, Log_ADV_Total, Log_ADV_Offline, Log_Catalogs_ExistCust,Log_Catalogs_Winback, Log_Catalogs_NewCust, Log_ADV_Online, Log_Search, Log_Newsletter,  Log_Portals)

df_sqroot = data.frame(Sales,Sqrt_Mailings, Sqrt_ADV_Total, Sqrt_ADV_Offline, Sqrt_Catalogs_ExistCust,Sqrt_Catalogs_Winback, Sqrt_Catalogs_NewCust, Sqrt_ADV_Online, Sqrt_Search,  Sqrt_Newsletter,  Sqrt_Portals)

df_cuberoot = data.frame(Sales,Cuberoot_Mailings, Cuberoot_ADV_Total, Cuberoot_ADV_Offline, Cuberoot_Catalogs_ExistCust,Cuberoot_Catalogs_Winback, Cuberoot_Catalogs_NewCust, Cuberoot_ADV_Online,  Cuberoot_Search,  Cuberoot_Newsletter,  Cuberoot_Portals)

df_inv = data.frame(Sales,Inv_Mailings, Inv_ADV_Total, Inv_ADV_Offline, Inv_Catalogs_ExistCust,Inv_Catalogs_Winback, Inv_Catalogs_NewCust, Inv_ADV_Online,  Inv_Search, Inv_Newsletter,  Inv_Portals)

#Original Correlation
df_f = sort(data.frame(abs(cor(Sales,data_new))), decreasing = TRUE)

df_f

#Log Correlation

df_logs_f = sort(abs(data.frame(cor(Sales,df_log))), decreasing = TRUE)

df_logs_f

#Sqroot Correlation

df_sqroot_f = sort(abs(data.frame(cor(Sales,df_sqroot))), decreasing = TRUE)

df_sqroot_f

#Cuberoot Correlation

df_cuberoot_f = sort(abs(data.frame(cor(Sales,df_cuberoot))), decreasing = TRUE)

df_cuberoot_f

#Inv Correlation

df_inv_f = sort(abs(data.frame(cor(Sales,df_inv))), decreasing = TRUE)

df_inv_f

#Square models

#Running model with intercept
regmod1<-lm(Sales~Lag_Sales+Sqrt_Portals+Sqrt_Search+Sqrt_ADV_Online)
summary(regmod1)
AIC(regmod1)
BIC(regmod1)

#Running model without intercept
regmod2<-lm(Sales~Lag_Sales+Sqrt_Portals+Sqrt_Search+Sqrt_ADV_Online-1)
summary(regmod2)
AIC(regmod2)
BIC(regmod2)

#Running model with intercept and with synergy
regmod3<-lm(Sales~Lag_Sales+Sqrt_Portals+Sqrt_Search+Sqrt_ADV_Online+Sqrt_Portals*Sqrt_Search+Sqrt_Portals*Sqrt_ADV_Online)
summary(regmod3)
AIC(regmod3)
BIC(regmod3)

#Running model without intercept and with synergy
regmod4<-lm(Sales~Lag_Sales+Sqrt_Portals+Sqrt_Search+Sqrt_ADV_Online+Sqrt_Portals*Sqrt_Search+Sqrt_Portals*Sqrt_ADV_Online-1)
summary(regmod4)
AIC(regmod4)
BIC(regmod4)


#log models

#Running model with intercept
regmod5<-lm(Sales~Lag_Sales+Log_Portals+Log_Search+Log_ADV_Offline+Log_Catalogs_ExistCust+Log_ADV_Online)
summary(regmod5)
AIC(regmod5)
BIC(regmod5)

#Running model without intercept
regmod6<-lm(Sales~Lag_Sales+Log_Portals+Log_Search++Log_Portals+Log_Search+Log_ADV_Offline+Log_Catalogs_ExistCust+Log_ADV_Online-1)
summary(regmod6)
AIC(regmod6)
BIC(regmod6)

#Running model with intercept and with synergy
regmod7<-lm(Sales~Lag_Sales+Log_Portals+Log_Search+Log_ADV_Offline+Log_Catalogs_ExistCust+Log_ADV_Online+Log_Portals*Log_Search+Log_Portals*Log_ADV_Offline++Log_Portals*Log_Catalogs_ExistCust+Log_Portals*Log_ADV_Online)
summary(regmod7)
AIC(regmod7)
BIC(regmod7)

#Running model without intercept and with synergy
regmod8<-lm(Sales~Lag_Sales+Log_Portals+Log_Search+Log_ADV_Offline+Log_Catalogs_ExistCust+Log_ADV_Online+Log_Portals*Log_Search+Log_Portals*Log_ADV_Offline++Log_Portals*Log_Catalogs_ExistCust+Log_Portals*Log_ADV_Online-1)
summary(regmod8)
AIC(regmod8)
BIC(regmod8)

#Log-log models

#Running model with intercept
regmod5<-lm(log(Sales)~log(Lag_Sales)+Log_Portals+Log_Search+Log_ADV_Offline+Log_Catalogs_ExistCust+Log_ADV_Online)
summary(regmod5)
AIC(regmod5)
BIC(regmod5)

#Running model without intercept
regmod6<-lm(log(Sales)~log(Lag_Sales)+Log_Portals+Log_Search++Log_Portals+Log_Search+Log_ADV_Offline+Log_Catalogs_ExistCust+Log_ADV_Online-1)
summary(regmod6)
AIC(regmod6)
BIC(regmod6)

#Running model with intercept and with synergy
regmod7<-lm(log(Sales)~log(Lag_Sales)+Log_Portals+Log_Search+Log_ADV_Offline+Log_Catalogs_ExistCust+Log_ADV_Online+Log_Portals*Log_Search+Log_Portals*Log_ADV_Offline++Log_Portals*Log_Catalogs_ExistCust+Log_Portals*Log_ADV_Online)
summary(regmod7)
AIC(regmod7)
BIC(regmod7)

#Running model without intercept and with synergy
regmod8<-lm(log(Sales)~log(Lag_Sales)+Log_Portals+Log_Search+Log_ADV_Offline+Log_Catalogs_ExistCust+Log_ADV_Online+Log_Portals*Log_Search+Log_Portals*Log_ADV_Offline++Log_Portals*Log_Catalogs_ExistCust+Log_Portals*Log_ADV_Online-1)
summary(regmod8)
AIC(regmod8)
BIC(regmod8)




# Original variables (Without transformation)
#Running model with intercept
regmod9<-lm(Sales~Lag_Sales+Portals+Search+ADV_Online)
summary(regmod9)
AIC(regmod9)
BIC(regmod9)

#Running model without intercept
regmod10<-lm(Sales~Lag_Sales+Portals+Search+ADV_Online-1)
summary(regmod10)
AIC(regmod10)
BIC(regmod10)

#Running model with intercept and with synergy
regmod11<-lm(Sales~Lag_Sales+Portals+Search+ADV_Online+Portals*Search+Portals*ADV_Online)
summary(regmod11)
AIC(regmod11)
BIC(regmod11)

#Running model without intercept and with synergy
regmod12<-lm(Sales~Lag_Sales+Portals+Search+ADV_Online+Portals*Search+Portals*ADV_Online-1)
summary(regmod12)
AIC(regmod12)
BIC(regmod12)

#New Log Model
regmod13<-lm(log(Sales)~log(Lag_Sales)+Log_Catalogs_ExistCust+Log_ADV_Online+Log_Search+Log_Portals+Log_Search*Log_Portals-1)
summary(regmod13)
AIC(regmod13)
BIC(regmod13)


#New model for Sqroot

regmod14<-lm(Sales~Lag_Sales+Sqrt_Portals+Sqrt_Search+Sqrt_Catalogs_ExistCust+Sqrt_ADV_Online+Sqrt_Portals*Sqrt_Search+Sqrt_Portals*Sqrt_Catalogs_ExistCust+Sqrt_Portals*Sqrt_ADV_Online-1)
summary(regmod14)
AIC(regmod14)
BIC(regmod14)

regmod15<-lm(Sales~Lag_Sales+Sqrt_Portals+Sqrt_Search+Sqrt_Catalogs_ExistCust+Sqrt_ADV_Online+Sqrt_Portals*Sqrt_Search+Sqrt_Portals*Sqrt_Catalogs_ExistCust-1)
summary(regmod15)
AIC(regmod15)
BIC(regmod15)

regmod16<-lm(Sales~Lag_Sales+Sqrt_Portals+Sqrt_Search+Sqrt_Catalogs_ExistCust+Sqrt_Catalogs_NewCust+Sqrt_ADV_Online+Sqrt_Portals*Sqrt_Search+Sqrt_Portals*Sqrt_Catalogs_ExistCust+Sqrt_Catalogs_NewCust*Sqrt_ADV_Online-1)
summary(regmod16)
AIC(regmod16)
BIC(regmod16)

regmod16b<-lm(Sales~Lag_Sales+Sqrt_Portals+Sqrt_Search+Sqrt_Catalogs_ExistCust+Sqrt_Catalogs_NewCust+Sqrt_Search*Sqrt_Catalogs_NewCust+Sqrt_Search*Sqrt_Catalogs_ExistCust+Sqrt_Portals*Sqrt_Catalogs_NewCust+Sqrt_Portals*Sqrt_Catalogs_ExistCust-1)
summary(regmod16b)
AIC(regmod16b)
BIC(regmod16b)
