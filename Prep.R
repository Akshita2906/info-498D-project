library(Hmisc)
library(foreign)
library(survey)
library(dplyr)

#Loading the data
#BRFSS_all_data <- sasxport.get("data/LLCP2016XPT/LLCP2016.xpt")
BRFSS_all_data <- sasxport.get("./data/LLCP2016.XPT")
View(BRFSS_all_data)

#--------------------------------------Health Care Access -------------------------------------------------------

#Picking up columns of choice
vars <- c("x.rfhlth", "hlthpln1", "persdoc2", "delaymed","medcost", "x.state", "x.llcpwt")
HCA_data <- BRFSS_all_data[vars]
#unique(HCA_data$x.rfhlth)

#Converting the values into yes and no. Setting defaults for x.rfhlth column
HCA_data$x.rfhlth <- replace(HCA_data$x.rfhlth, HCA_data$x.rfhlth==2,0)
HCA_data$x.rfhlth <- replace(HCA_data$x.rfhlth, HCA_data$x.rfhlth==9,NA)

#Survey weights for x.rfhlth column 
svey <- svydesign(ids=~1 ,strata=HCA_data$x.rfhlth, weights=HCA_data$x.llcpwt, nest=T, data=HCA_data)
sveymean <- svymean(~x.rfhlth, svey, na.rm=TRUE)
df_results<-data.frame("Adults with good or better health",sveymean[1]*100, stringsAsFactors = FALSE)
names(df_results)<-c("Metric","Value")
Z <- svyby(~x.rfhlth, ~x.state, svey, svymean, na.rm=TRUE)

#Storing the results into a dataframe for further use
df_states<-data.frame("Adults with good health",Z[1], Z[2]*100, stringsAsFactors = FALSE)
names(df_states)<-c("Metric","State", "Value")


#Converting the values. Setting defaults for hlthpln1 column
HCA_data$hlthpln1 <- replace(HCA_data$hlthpln1, HCA_data$hlthpln1 == 2,0)
HCA_data$hlthpln1 <- replace(HCA_data$hlthpln1, HCA_data$hlthpln1 == 9,NA)
HCA_data$hlthpln1 <- replace(HCA_data$hlthpln1, HCA_data$hlthpln1 == 7,NA)


#Survey weights for hlthpln1 column
svey <- svydesign(ids=~1 ,strata=HCA_data$hlthpln1, weights=HCA_data$x.llcpwt, nest=T, data=HCA_data)
sveymean <- svymean(~hlthpln1, svey, na.rm=TRUE)


#Storing the results into a dataframe for further use
df_results[nrow(df_results) + 1,] = c("Adults with health care coverage", sveymean[1]*100)
Z <- svyby(~hlthpln1, ~x.state, svey, svymean, na.rm=TRUE)
df_states<-data.frame("Adults with health care coverage",Z[1], Z[2]*100, stringsAsFactors = FALSE)
names(df_states)<-c("Metric","State", "Value")


#Converting the values. Setting defaults for persdoc2 column
HCA_data$persdoc2 <- replace(HCA_data$persdoc2, HCA_data$persdoc2==2,1)
HCA_data$persdoc2 <- replace(HCA_data$persdoc2, HCA_data$persdoc2==3,0)
HCA_data$persdoc2 <- replace(HCA_data$persdoc2, HCA_data$persdoc2==7,NA)
HCA_data$persdoc2 <- replace(HCA_data$persdoc2, HCA_data$persdoc2==9,NA)

#Survey weights for persdoc2 column
svey <- svydesign(ids=~1 ,strata=HCA_data$persdoc2, weights=HCA_data$x.llcpwt, nest=T, data=HCA_data)
sveymean <- svymean(~persdoc2, svey, na.rm=TRUE)


#Storing the results into a dataframe for further use
df_results[nrow(df_results) + 1,] = c("Adults with personal doctor or health care provider", sveymean[1]*100)
Z <- svyby(~persdoc2, ~x.state, svey, svymean, na.rm=TRUE)
df_states<-data.frame("Adults with health care service",Z[1], Z[2]*100, stringsAsFactors = FALSE)
names(df_states)<-c("Metric","State", "Value")


#Converting the values. Setting defaults for medcost column
HCA_data$medcost <- replace(HCA_data$medcost, HCA_data$medcost==2,0)
HCA_data$medcost <- replace(HCA_data$medcost, HCA_data$medcost==7,NA)
HCA_data$medcost <- replace(HCA_data$medcost, HCA_data$medcost==9,NA)

#Survey weights for medcost column
svey <- svydesign(ids=~1 ,strata=HCA_data$medcost, weights=HCA_data$x.llcpwt, nest=T, data=HCA_data)
svey_mean <- svymean(~medcost, svey, na.rm=TRUE)

#Storing the results into a dataframe for further use
df_results[nrow(df_results) + 1,] = c("Adults with no cost constraints", 100 - svey_mean[1]*100)
Z <- svyby(~medcost, ~x.state, svey, svymean, na.rm=TRUE)
df_states<-data.frame("Adults with delay in healthcare due to cost",Z[1], Z[2]*100, stringsAsFactors = FALSE)
names(df_states)<-c("Metric","State", "Value")

#--------------------------------Heart Disease------------------------------------------------------------------------

#Picking up columns of choice
vars <- c("x.michd","cvdinfr4","cvdcrhd4","cvdstrk3","x.state","x.llcpwt")
HD_data <- BRFSS_all_data[vars]

#Converting the values into yes and no. Setting defaults for all columns following above convention
HD_data %>% mutate(x.michd = case_when((x.michd== 1)~1, (x.michd == 2)~0, (x.michd>2)~-1 )) -> HD_data
HD_data %>% mutate(cvdinfr4 = case_when((cvdinfr4== 1)~1, (cvdinfr4 == 2)~0, (cvdinfr4>2)~-1 )) -> HD_data
HD_data %>% mutate(cvdcrhd4 = case_when((cvdcrhd4== 1)~1, (cvdcrhd4 == 2)~0, (cvdcrhd4>2)~-1 )) -> HD_data
HD_data %>% mutate(cvdstrk3 = case_when((cvdstrk3== 1)~1, (cvdstrk3 == 2)~0, (cvdstrk3>2)~-1 )) -> HD_data


#Applying weights
srvey_heart <- svydesign(ids=~1 ,strata=HD_data$cvdinfr4, weights=HD_data$x.llcpwt, nest=T, data=HD_data)
srvey_mean_heart <- svymean(~cvdinfr4, srvey_heart, na.rm=TRUE)
srvey_coro <- svydesign(ids=~1 ,strata=HD_data$cvdcrhd4, weights=HD_data$x.llcpwt, nest=T, data=HD_data)
srvey_mean_coro <- svymean(~cvdcrhd4, srvey_coro, na.rm=TRUE)
srvey_stroke <- svydesign(ids=~1 ,strata=HD_data$cvdstrk3, weights=HD_data$x.llcpwt, nest=T, data=HD_data)
srvey_mean_stroke <- svymean(~cvdstrk3, srvey_stroke, na.rm=TRUE)
srvey_net<- svydesign(ids=~1 ,strata=HD_data$x.michd, weights=HD_data$x.llcpwt, nest=T, data=HD_data)
srvey_mean_net <- svymean(~x.michd, srvey_net, na.rm=TRUE)

weighted_stats <- c(srvey_mean_heart*100,srvey_mean_coro*100,srvey_mean_stroke*100, srvey_mean_net*100)
net_means_heart <- data.frame(Cardiovascular = c("Heart Attack", "Coronary or Angial", "Stroke","Net Heart Disease"), Weighted = weighted_stats)


#Wrangling into state-wise weights for x.michd 
H <- svyby(~x.michd, ~x.state, srvey_net, svymean, na.rm=TRUE)
df_states_heart<-data.frame("Adults with Heart Disease By State",H[1], H[2]*100, stringsAsFactors = FALSE)
names(df_states_heart)<-c("Metric","State", "Value")

#Wrangling into state-wise weights for cvdinfr4 - Myocardial Infarction
H_MI <- svyby(~cvdinfr4, ~x.state, srvey_heart, svymean, na.rm=TRUE)
df_states_heart_MI<-data.frame("Adults with Diagnosed Myocardial Infarction By State",H[1], H[2]*100, stringsAsFactors = FALSE)
names(df_states_heart_MI)<-c("Metric","State", "Value")

#Wrangling into state-wise weights for cvdinfr4 - Myocardial Infarction
H_MI <- svyby(~cvdinfr4, ~x.state, srvey_heart, svymean, na.rm=TRUE)
df_states_heart_MI<-data.frame("Adults with Diagnosed Myocardial Infarction By State",H_MI[1], H_MI[2]*100, stringsAsFactors = FALSE)
names(df_states_heart_MI)<-c("Metric","State", "Value")

#Wrangling into state-wise weights for cvdstrk3 - Diagnosed for Stroke
H_S <- svyby(~cvdstrk3, ~x.state, srvey_stroke, svymean, na.rm=TRUE)
df_states_heart_S<-data.frame("Adults with Diagnosed Stroke By State",H_S[1], H_S[2]*100, stringsAsFactors = FALSE)
names(df_states_heart_S)<-c("Metric","State", "Value")

#Wrangling into state-wise weights for cvdcrhd4 - Diagnosed for Angial/Coronary Issues
H_AC <- svyby(~cvdcrhd4, ~x.state, srvey_coro, svymean, na.rm=TRUE)
df_states_heart_AC<-data.frame("Adults with Diagnosed Angial/Coronary Issues By State",H_AC[1], H_AC[2]*100, stringsAsFactors = FALSE)
names(df_states_heart_AC)<-c("Metric","State", "Value")

#---------------------------------------------Obesity--------------------------------------------------------------------
#Picking up columns of choice
vars <- c("x.rfbmi5", "x.state", "x.llcpwt")
obese_data <- BRFSS_all_data[vars]
#unique(HCA_data$x.rfhlth)

#Converting the values into yes and no. Setting defaults for x.rfbmi5 column
obese_data$x.rfbmi5 <- replace(obese_data$x.rfbmi5, obese_data$x.rfbmi5==1,0)
obese_data$x.rfbmi5 <- replace(obese_data$x.rfbmi5, obese_data$x.rfbmi5==2,1)
obese_data$x.rfbmi5 <- replace(obese_data$x.rfbmi5, obese_data$x.rfbmi5==9,NA)


#Survey weights for x.rfbmi5 column
svey <- svydesign(ids=~1 ,strata=obese_data$x.rfbmi5, weights=obese_data$x.llcpwt, nest=T, data=obese_data)
svey_mean <- svymean(~x.rfbmi5, svey, na.rm=TRUE)

#Storing the results into a dataframe for further use
df_results[nrow(df_results) + 1,] = c("Adults with obesity", svey_mean[1]*100)
Z <- svyby(~x.rfbmi5, ~x.state, svey, svymean, na.rm=TRUE)
df_states<-data.frame("Adults with Obesity",Z[1], Z[2]*100, stringsAsFactors = FALSE)
names(df_states)<-c("Metric","State", "Value")
