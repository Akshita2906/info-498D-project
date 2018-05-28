library(Hmisc)
library(foreign)
library(survey)
library(dplyr)
library(tidyr)


#Loading the data
#BRFSS_all_data <- sasxport.get("data/LLCP2016XPT/LLCP2016.xpt")
BRFSS_all_data <- sasxport.get("./data/LLCP2016.XPT")
View(BRFSS_all_data)

#--------------------------------------Health Care Access/knowledge -------------------------------------------------------

#unique(BRFSS_all_data$x.race.g1)
#Picking up columns of choice
vars <- c("x.rfhlth", "hlthpln1", "persdoc2", "delaymed","medcost", "x.state", "x.llcpwt", "undrstnd", "sex", "x.race.g1")
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
Z <- svyby(~x.rfhlth, ~sex+x.state+x.race.g1 , svey, svymean, na.rm=TRUE)
Z_state <- svyby(~x.rfhlth, ~x.state , svey, svymean, na.rm=TRUE)

#Storing the results into a dataframe for further use
df_states<-data.frame("Adults with good health",Z[1], Z[4]*100, stringsAsFactors = FALSE)
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
Z <- svyby(~hlthpln1, ~sex+x.state+x.race.g1, svey, svymean, na.rm=TRUE)
Z_states_2<- svyby(~hlthpln1, ~x.state, svey, svymean, na.rm=TRUE)
df_states <- data.frame("Adults with health care coverage",Z[1], Z[4]*100, stringsAsFactors = FALSE)
#names(df_states)<-c("Metric","State", "Value")


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
Z <- svyby(~persdoc2, ~sex+x.state+x.race.g1, svey, svymean, na.rm=TRUE)
Z_states_3 <- svyby(~persdoc2, ~x.state, svey, svymean, na.rm=TRUE)
df_states<-data.frame("Adults with health care service",Z[1], Z[4]*100, stringsAsFactors = FALSE)
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
#Z <- svyby(~medcost, ~(sex+x.state+x.race.g1), svey, svymean, na.rm=TRUE)
Z_states_4 <- svyby(~medcost, ~x.state, svey, svymean, na.rm=TRUE)
#df_states<-data.frame("Adults with delay in healthcare due to cost",Z[1], Z[4]*100, stringsAsFactors = FALSE)
#names(df_states)<-c("Metric","State", "Value")



#Converting the values into yes and no. Setting defaults for undrstnd column
HCA_data<- HCA_data[na.omit(HCA_data$undrstnd), ]
HCA_data$undrstnd <- replace(HCA_data$undrstnd, HCA_data$undrstnd==3,0)
HCA_data$undrstnd <- replace(HCA_data$undrstnd, HCA_data$undrstnd==4,0)
HCA_data$undrstnd <- replace(HCA_data$undrstnd, HCA_data$undrstnd==2,1)
HCA_data$undrstnd <- replace(HCA_data$undrstnd, HCA_data$undrstnd==7,NA)
HCA_data$undrstnd <- replace(HCA_data$undrstnd, HCA_data$undrstnd==9,NA)

#Survey weights for undrstnd column 
svey <- svydesign(ids=~1 ,strata=HCA_data$undrstnd, weights=HCA_data$undrstnd, nest=T, data=HCA_data)
sveymean <- svymean(~undrstnd, svey, na.rm=TRUE)
df_results<-data.frame("Adults who understand the language of medical professionals ",sveymean[1]*100, stringsAsFactors = FALSE)
names(df_results)<-c("Metric","Value")
Z <- svyby(~x.rfhlth, ~sex+x.state+x.race.g1, svey, svymean, na.rm=TRUE)

#Storing the results into a dataframe for further use
df_states<-data.frame("Adults with good health",Z[1], Z[2]*100, stringsAsFactors = FALSE)
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

#---------------------------------------------Diabetes--------------------------------------------------------------------

#Picking up columns of choice
vars<- c("diabete3", "diabage2", "pdiabtst", "prediab1", "x.state", "x.llcpwt")
diab_data <- BRFSS_all_data[vars]

#Converting the values into yes and no. Setting the defaults

diab_data$diabete3[diab_data$diabete3==3] <- 0
diab_data$diabete3[diab_data$diabete3==4] <- 0
diab_data$diabete3[diab_data$diabete3==7] <- 0
diab_data$diabete3[diab_data$diabete3==9] <- 0

diab_data$diabage2[diab_data$diabage2==98] <- 0
diab_data$diabage2[diab_data$diabage2==99] <- 0

diab_data$prediab1[diab_data$prediab1==3] <- 0
diab_data$prediab1[diab_data$prediab1==7] <- 0
diab_data$prediab1[diab_data$prediab1==9] <- 0

#Survey weights
svey_daibete3 <- svydesign(ids=~1 ,strata=diab_data$diabete3, weights=diab_data$x.llcpwt, nest=T, data=diab_data)
svey_mean_daibete3 <- svymean(~diabete3, svey_daibete3, na.rm=TRUE)

svey_diabage2 <- svydesign(ids=~1 ,strata=diab_data$diabage2, weights=diab_data$x.llcpwt, nest=T, data=diab_data)
svey_mean_diabage2 <- svymean(~diabage2, svey_diabage2, na.rm=TRUE)

svey_prediab <- svydesign(ids=~1 ,strata=diab_data$prediab1, weights=diab_data$x.llcpwt, nest=T, data=diab_data)
svey_mean_prediab <- svymean(~prediab1, svey_prediab, na.rm=TRUE)

weighted_stats_diab <- c(svey_mean_daibete3*100,svey_mean_prediab*100)
net_means_diabetes <- data.frame(Diabetes = c("Diabetes","Pre-diabetes"), Weighted = weighted_stats_diab)

#Wrangling into state-wise weights for diabete3 

S1 <- svyby(~diabete3, ~x.state, svey_daibete3, svymean, na.rm=TRUE)
df_states_diabetes<-data.frame("Adults with Diabetes by state",S1[1], S1[2]*100, stringsAsFactors = FALSE)
names(df_states_diabetes)<-c("Metric","State", "Value")

State_name <- c("Alabama",
            "Alaska", 
            "Arizona", 
            "Arkansas",
            "California", 
            "Colorado", 
            "Connecticut",
            "Delaware", 
            "District of Columbia",
            "Florida",
            "Georgia", 
            "Hawaii", 
            "Idaho", 
            "Illinois",
            "Indiana",
            "Iowa", 
            "Kansas", 
            "Kentucky", 
            "Louisiana",
            "Maine", 
            "Maryland", 
            "Massachusetts", 
            "Michigan", 
            "Minnesota", 
            "Mississippi", 
            "Missouri", 
            "Montana",
            "Nebraska", 
            "Nevada", 
            "New Hampshire", 
            "New Jersey", 
            "New Mexico", 
            "New York", 
            "North Carolina", 
            "North Dakota", 
            "Ohio",
            "Oklahoma",
            "Oregon", 
            "Pennsylvania",
            "Rhode Island",
            "South Carolina", 
            "South Dakota", 
            "Tennessee", 
            "Texas", 
            "Utah",
            "Vermont", 
            "Virginia", 
            "Washington", 
            "West Virginia", 
            "Wisconsin", 
            "Wyoming",
            "Guam",
            "Puerto Rico",
            "Virgin Islands")

df_states_diabetes <- cbind(State_name, df_states_diabetes)

df_states_diabetes<-data.frame("Adults with Diabetes by state",S1[1], S1[2]*100, stringsAsFactors = FALSE)
names(df_states_diabetes)<-c("Metric","State", "Value")

S2 <- svyby(~prediab1, ~x.state, svey_prediab, svymean, na.rm=TRUE)
