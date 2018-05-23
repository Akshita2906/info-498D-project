library(Hmisc)
library(foreign)
library(survey)
library(dplyr)

#Loading the data
BRFSS_all_data <- sasxport.get("data/LLCP2016XPT/LLCP2016.xpt")
View(BRFSS_all_data)

#--------------------------------------Health Care Access -------------------------------------------------------

#Picking up columns of choice
vars <- c("x.rfhlth", "hlthpln1", "persdoc2", "delaymed","medcost", "x.state")
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


