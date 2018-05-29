#Downloading all relevant libraries
library(Hmisc)
library(foreign)
library(survey)
library(dplyr)
library(tidyr)

#Importing net BRFSS Data 
#BRFSS_all_data <- sasxport.get("data/LLCP2016XPT/LLCP2016.xpt")
BRFSS_all_data <- sasxport.get("LLCP2016.XPT")
View(BRFSS_all_data)
#BREAKING DOWN INTO SECTIONS
#PARAMETER 1 - HEALTH CARE ACCESS AND HEALTH LITERACY

#--------------------------------------Health Care Access/knowledge -------------------------------------------------------

#unique(BRFSS_all_data$x.race.g1)
#unique(HCA_data$x.rfhlth)

#Picking up variables linked to healthcare access
vars <- c("x.rfhlth", "hlthpln1", "persdoc2", "delaymed","medcost", "x.state", "x.llcpwt", "undrstnd", "sex", "x.race.g1")
HCA_data <- BRFSS_all_data[vars]

#Converting the values into yes and no. Setting defaults for x.rfhlth column
HCA_data$x.rfhlth <- replace(HCA_data$x.rfhlth, HCA_data$x.rfhlth==2,0)
HCA_data$x.rfhlth <- replace(HCA_data$x.rfhlth, HCA_data$x.rfhlth==9,NA)

#Adding Survey weights for x.rfhlth column 
svey <- svydesign(ids=~1 ,strata=HCA_data$x.rfhlth, weights=HCA_data$x.llcpwt, nest=T, data=HCA_data)
sveymean <- svymean(~x.rfhlth, svey, na.rm=TRUE)
df_results<-data.frame("Adults with good or better health",sveymean[1]*100, stringsAsFactors = FALSE)
names(df_results)<-c("Metric","Value")

#Weighing variables as per state and then by state,race and sex
Z <- svyby(~x.rfhlth, ~sex+x.state+x.race.g1 , svey, svymean, na.rm=TRUE)
Z_state <- svyby(~x.rfhlth, ~x.state , svey, svymean, na.rm=TRUE)
Z_betterhealth_state <- Z_state
Z_betterhealth_race <- Z

#Storing the results into a dataframe for further use
df_states<-data.frame("Adults with good health",Z[2], Z[4]*100, stringsAsFactors = FALSE)
names(df_states)<-c("Metric","State", "Value")

#Variable 2
#Converting the values and Setting defaults for hlthpln1 column
HCA_data$hlthpln1 <- replace(HCA_data$hlthpln1, HCA_data$hlthpln1 == 2,0)
HCA_data$hlthpln1 <- replace(HCA_data$hlthpln1, HCA_data$hlthpln1 == 9,NA)
HCA_data$hlthpln1 <- replace(HCA_data$hlthpln1, HCA_data$hlthpln1 == 7,NA)

#Adding Survey weights for hlthpln1 column
svey <- svydesign(ids=~1 ,strata=HCA_data$hlthpln1, weights=HCA_data$x.llcpwt, nest=T, data=HCA_data)
sveymean <- svymean(~hlthpln1, svey, na.rm=TRUE)

#Storing the results into a dataframe for further use
df_results[nrow(df_results) + 1,] = c("Adults with health care coverage", sveymean[1]*100)

#Weighing variables as per state and then by state,race and sex
Z <- svyby(~hlthpln1, ~sex+x.state+x.race.g1, svey, svymean, na.rm=TRUE)
Z_state<- svyby(~hlthpln1, ~x.state, svey, svymean, na.rm=TRUE)
Z_healthcoverage_state <-Z_state
Z_healthcoverage_race<- Z

df_states <- data.frame("Adults with health care coverage",Z[2], Z[4]*100, stringsAsFactors = FALSE)
names(df_states)<-c("Metric","State", "Value")

#variable 3
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

#Weighing variables as per state and then by state,race and sex
Z <- svyby(~persdoc2, ~sex+x.state+x.race.g1, svey, svymean, na.rm=TRUE)
Z_state <- svyby(~persdoc2, ~x.state, svey, svymean, na.rm=TRUE)
Z_provider_state <- Z_state
Z_provider_race <- Z
df_states<-data.frame("Adults with health care service",Z[2], Z[4]*100, stringsAsFactors = FALSE)
names(df_states)<-c("Metric","State", "Value")


#PARAMETER 2
#--------------------------------------Health Care Knowledge -------------------------------------------------------

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
df_results[nrow(df_results) + 1,] = c("Adults who understand the language of medical professionals", sveymean[1]*100)
names(df_results)<-c("Metric","Value")

#Weighing variables as per state and then by state,race and sex
Z <- svyby(~undrstnd, ~sex+x.state+x.race.g1, svey, svymean, na.rm=TRUE)
Z_state <- svyby(~undrstnd, ~x.state, svey, svymean, na.rm=TRUE)
Z_literacy_state<- Z_state
Z_literacy_race <- Z

#Storing the results into a dataframe for further use
df_states<-data.frame("Adults with good health",Z[2], Z[4]*100, stringsAsFactors = FALSE)
names(df_states)<-c("Metric","State", "Value")

#Writing all variables into separate CSVs for further use
Z_provider_state <- Z_provider_state %>% select(-x.state)
Z_betterhealth_state <- Z_betterhealth_state %>% select(-x.state)
Z_net_state <-(cbind(Z_healthcoverage_state,Z_provider_state,Z_betterhealth_state))
Z_net_race <- cbind(Z_healthcoverage_race, Z_provider_race, Z_betterhealth_race)

#Final CSVs for vizualizations
write.csv(Z_net_race,"./data/healthcareaccess_race.csv")
write.csv(Z_net_state,"./data/healthcareaccess_state.csv")


#PARAMETER 3

#--------------------------------Heart Disease------------------------------------------------------------------------

#Picking up columns of choice
vars <- c("x.michd","cvdinfr4","cvdcrhd4","cvdstrk3","x.state","x.llcpwt","sex", "x.race.g1")
HD_data <- BRFSS_all_data[vars]

#Converting the values into yes and no. Setting defaults for all columns following above convention
HD_data %>% mutate(x.michd = case_when((x.michd== 1)~1, (x.michd == 2)~0, (x.michd>2)~9 )) -> HD_data
HD_data %>% mutate(cvdinfr4 = case_when((cvdinfr4== 1)~1, (cvdinfr4 == 2)~0, (cvdinfr4>2)~9 )) -> HD_data
HD_data %>% mutate(cvdcrhd4 = case_when((cvdcrhd4== 1)~1, (cvdcrhd4 == 2)~0, (cvdcrhd4>2)~9 )) -> HD_data
HD_data %>% mutate(cvdstrk3 = case_when((cvdstrk3== 1)~1, (cvdstrk3 == 2)~0, (cvdstrk3>2)~9 )) -> HD_data


#Applying weights
srvey_heart <- svydesign(ids=~1 ,strata=HD_data$cvdinfr4, weights=HD_data$x.llcpwt, nest=T, data=HD_data)
srvey_mean_heart <- svymean(~cvdinfr4, srvey_heart, na.rm=TRUE)
df_results[nrow(df_results) + 1,] = c("Adults diagnosed with Heart Attack", srvey_mean_heart[1]*100)
names(df_results)<-c("Metric","Value")


srvey_coro <- svydesign(ids=~1 ,strata=HD_data$cvdcrhd4, weights=HD_data$x.llcpwt, nest=T, data=HD_data)
srvey_mean_coro <- svymean(~cvdcrhd4, srvey_coro, na.rm=TRUE)
df_results[nrow(df_results) + 1,] = c("Adults diagnosed with Coronary Heart Disease", srvey_mean_coro[1]*100)
names(df_results)<-c("Metric","Value")


srvey_stroke <- svydesign(ids=~1 ,strata=HD_data$cvdstrk3, weights=HD_data$x.llcpwt, nest=T, data=HD_data)
srvey_mean_stroke <- svymean(~cvdstrk3, srvey_stroke, na.rm=TRUE)
df_results[nrow(df_results) + 1,] = c("Adults diagnosed with a Stroke", srvey_mean_stroke[1]*100)
names(df_results)<-c("Metric","Value")


srvey_net<- svydesign(ids=~1 ,strata=HD_data$x.michd, weights=HD_data$x.llcpwt, nest=T, data=HD_data)
srvey_mean_net <- svymean(~x.michd, srvey_net, na.rm=TRUE)
df_results[nrow(df_results) + 1,] = c("Adults diagnosed with CHD or MI", srvey_mean_net[1]*100)
names(df_results)<-c("Metric","Value")

#Saving net national means into a dataframe
weighted_stats <- c(srvey_mean_heart*100,srvey_mean_coro*100,srvey_mean_stroke*100, srvey_mean_net*100)
net_means_heart <- data.frame(Cardiovascular = c("Heart Attack", "Coronary or Angial", "Stroke","Net Heart Disease"), Weighted = weighted_stats)

HD_data <- na.omit(HD_data)

#Wrangling into state-wise weights for x.michd 
H_state <- svyby(~x.michd, ~x.state, srvey_net, svymean, na.rm=TRUE)
H <- svyby(~x.michd, ~x.race.g1+x.state , srvey_net, svymean, na.rm=TRUE)
H_heartdisease_race <- H
H_heartdisease_state <- H_state
df_states_heart<-data.frame("Adults with Heart Disease By State",H[2], H[4]*100, stringsAsFactors = FALSE)
names(df_states_heart)<-c("Metric","State", "Value")

#Wrangling into state-wise weights for cvdinfr4 - Myocardial Infarction
H_state <- svyby(~cvdinfr4, ~x.state, srvey_heart, svymean, na.rm=TRUE)
H <- svyby(~cvdinfr4, ~x.state+x.race.g1, srvey_heart, svymean, na.rm=TRUE)
H_myo_race <- H
H_myo_state <- H_state
df_states_heart_MI<-data.frame("Adults with Diagnosed Myocardial Infarction By State",H[2], H[4]*100, stringsAsFactors = FALSE)
names(df_states_heart_MI)<-c("Metric","State", "Value")


#Wrangling into state-wise weights for cvdstrk3 - Diagnosed for Stroke
H_state <- svyby(~cvdstrk3, ~x.state, srvey_stroke, svymean, na.rm=TRUE)
H <- svyby(~cvdstrk3, ~x.state+x.race.g1, srvey_stroke, svymean, na.rm=TRUE)
H_stroke_race <- H
H_stroke_state <- H_state
df_states_heart_S<-data.frame("Adults with Diagnosed Stroke By State",H[2], H[4]*100, stringsAsFactors = FALSE)
names(df_states_heart_S)<-c("Metric","State", "Value")

#Wrangling into state-wise weights for cvdcrhd4 - Diagnosed for Angial/Coronary Issues
H_state <- svyby(~cvdcrhd4, ~x.state, srvey_coro, svymean, na.rm=TRUE)
H <- svyby(~cvdcrhd4, ~x.state+x.race.g1, srvey_coro, svymean, na.rm=TRUE)
H_ang_race <- H
H_ang_state <- H_state
df_states_heart_AC<-data.frame("Adults with Diagnosed Angial/Coronary Issues By State",H[2], H[4]*100, stringsAsFactors = FALSE)
names(df_states_heart_AC)<-c("Metric","State", "Value")

H_net_state <- cbind(H_heartdisease_state,H_myo_state,H_stroke_state,H_ang_state)
H_net_race <- cbind(H_heartdisease_race,H_myo_race,H_stroke_race,H_ang_race)

#Writing into final CSVs for vizualization
write.csv(H_net_race,"./data/heartdisease_race.csv")
write.csv(H_net_state,"./data/heartdisease_state.csv")


#PARAMETER 4

#---------------------------------------------Obesity--------------------------------------------------------------------
#Picking up columns of choice
vars <- c("x.rfbmi5", "x.state", "x.llcpwt","x.race.g1","sex")
obese_data <- BRFSS_all_data[vars]
#unique(HCA_data$x.rfhlth)

#Converting the values into yes and no. Setting defaults for x.rfbmi5 column
obese_data$x.rfbmi5 <- replace(obese_data$x.rfbmi5, obese_data$x.rfbmi5==1,0)
obese_data$x.rfbmi5 <- replace(obese_data$x.rfbmi5, obese_data$x.rfbmi5==2,1)
obese_data$x.rfbmi5 <- replace(obese_data$x.rfbmi5, obese_data$x.rfbmi5==9,NA)


#Survey weights for x.rfbmi5 column
svey <- svydesign(ids=~1 ,strata=obese_data$x.rfbmi5, weights=obese_data$x.llcpwt, nest=T, data=obese_data)
svey_mean <- svymean(~x.rfbmi5, svey, na.rm=TRUE)
options(survey.lonely.psu = "adjust")

#Storing the results into a dataframe for further use
df_results[nrow(df_results) + 1,] = c("Adults with obesity", svey_mean[1]*100)

#Statewise wrangling as per state,sex and race
O_state <- svyby(~x.rfbmi5, ~x.state, svey, svymean, na.rm=TRUE)
O_race <- svyby(~x.rfbmi5, ~x.race.g1+x.state, svey, svymean, na.rm=TRUE)

df_states<-data.frame("Adults with Obesity",O_race[2], O_race[4]*100, stringsAsFactors = FALSE)
names(df_states)<-c("Metric","State", "Value")

#Writing into final CSVs for vizualization
write.csv(O_state,"./data/obesity_state.csv")
write.csv(O_race,"./data/obesity_state.csv")


#PARAMETER 5
#---------------------------------------------Diabetes--------------------------------------------------------------------

#Picking up columns of choice
vars<- c("diabete3", "diabage2", "pdiabtst", "prediab1", "x.state", "x.llcpwt","sex","x.race.g1")
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
df_results[nrow(df_results) + 1,] = c("Adults with diabetes", svey_mean_daibete3[1]*100)


svey_diabage2 <- svydesign(ids=~1 ,strata=diab_data$diabage2, weights=diab_data$x.llcpwt, nest=T, data=diab_data)
svey_mean_diabage2 <- svymean(~diabage2, svey_diabage2, na.rm=TRUE)


svey_prediab <- svydesign(ids=~1 ,strata=diab_data$prediab1, weights=diab_data$x.llcpwt, nest=T, data=diab_data)
svey_mean_prediab <- svymean(~prediab1, svey_prediab, na.rm=TRUE)
df_results[nrow(df_results) + 1,] = c("Adults with pre-diabetes", svey_mean_prediab[1]*100)

weighted_stats_diab <- c(svey_mean_daibete3*100,svey_mean_prediab*100)
net_means_diabetes <- data.frame(Diabetes = c("Diabetes","Pre-diabetes"), Weighted = weighted_stats_diab)


#Wrangling into state-wise weights for diabete3 
D_state <- svyby(~diabete3, ~x.state, svey_daibete3, svymean, na.rm=TRUE)
D_race <- svyby(~diabete3, ~x.race.g1+x.state+sex, svey_daibete3, svymean, na.rm=TRUE)
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

#Writing into final CSVs for viz
write.csv(D_state,"./data/diabetes_state.csv")
write.csv(D_race,"./data/diabetes_race.csv")


#PARAMETER 5

#----------------------------------Pre-Diab-----------------------------------------------------------------
#Loading data
X <- BRFSS_all_data

#Picking variables of choice and subsetting dataframe 
Y <- subset(X, select=c("x.state", "prediab1", "x.llcpwt", "sex", "x.race.g1"))

#Mutating variables and making them uniform
Y <- mutate(Y, prediab1 = case_when(prediab1 %in% 1:2~1, prediab1== 3~0, prediab1 %in% 7:9~-1, is.na(prediab1)==T~-1))
Y$prediab1[Y$prediab1==-1] <- NA
Y <- na.omit(Y)

#Survey weights
svey <- svydesign(ids=~1 ,strata=Y$prediab1, weights=Y$x.llcpwt, nest=T, data=Y)
national_prediabetic <- svymean(~prediab1, svey, na.rm=T)

#Statewise wrangling into sex, race
state_prediabetic <- svyby(~prediab1, ~x.state, svey, svymean, na.rm=T)
Z <- svyby(~prediab1, ~x.state+sex+x.race.g1, svey, svymean, na.rm=T)

#Writing into final CSVs for viz
write.csv(state_prediabetic,"./data/prediabetes_state.csv")
write.csv(Z,"./data/prediabetes_race.csv")

#PARAMETER 6

# ------------------------------------ Soda Pop ----------------------------------------------------
#Loading data
X <- BRFSS_all_data

#Picking variables of choice and subsetting dataframe
Y <- subset(X, select=c("x.state", "ssbsugr2", "x.llcpwt", "sex", "x.race.g1"))

#Mutating variables and making them uniform
Y$ssbsugr2[Y$ssbsugr2==777] <- NA 
Y$ssbsugr2[Y$ssbsugr2==999] <- NA
Y$ssbsugr2[Y$ssbsugr2==888] <- 0
Y <- na.omit(Y)

#Mutating daily,monthly and weekly variables to weekly
Y <- mutate(Y, ssbsugr2 = case_when(ssbsugr2 %in% 100:199~((ssbsugr2%%100)*7), ssbsugr2 %in% 200:299~ssbsugr2%%200, ssbsugr2 %in% 300:399~((ssbsugr2%%300)/4), ssbsugr2==0~0))

#survey weights
svey <- svydesign(ids=~1 ,strata=Y$ssbsugr2, weights=Y$x.llcpwt, nest=T, data=Y)
options(survey.lonely.psu = "adjust")
national_ssbpop <- svymean(~ssbsugr2, svey, na.rm=T)

#Adding results to dataframe
df_results[nrow(df_results) + 1,] = c("Adults drinking sugar sweetened soda", national_ssbpop[1])

#Statewise wrangling to state, race and sex
state_ssbpop <- svyby(~ssbsugr2, ~x.state, svey, svymean, na.rm=T)
Z <- svyby(~ssbsugr2, ~x.state+sex+x.race.g1, svey, svymean, na.rm=T)

#Writing into final CSVs for viz
write.csv(state_ssbpop,"./data/sugar_soda_state.csv")
write.csv(Z,"./data/sugar_soda_race.csv")


#PARAMETER 7

# -------------------------------------- SSB (Non Soda) -------------------------------------------
#Loading data
X <- BRFSS_all_data

#Subsetting data and adding variables of interest
Y <- subset(X, select=c("x.state", "ssbfrut2", "x.llcpwt", "sex", "x.race.g1"))

#Mutating variables to make them uniform
Y$ssbfrut2[Y$ssbfrut2==777] <- NA 
Y$ssbfrut2[Y$ssbfrut2==999] <- NA
Y$ssbfrut2[Y$ssbfrut2==888] <- 0
Y <- na.omit(Y)

#Mutating daily,monthly and weekly variables to weekly
Y <- mutate(Y, ssbfrut2 = case_when(ssbfrut2 %in% 100:199~((ssbfrut2%%100)*7), ssbfrut2 %in% 200:299~ssbfrut2%%200, ssbfrut2 %in% 300:399~((ssbfrut2%%300)/4), ssbfrut2==0~0))

#Survey weights
svey <- svydesign(ids=~1 ,strata=Y$ssbfrut2, weights=Y$x.llcpwt, nest=T, data=Y)
options(survey.lonely.psu = "adjust")
national_nonpop <- svymean(~ssbfrut2, svey, na.rm=T)

#Adding results to dataframe
df_results[nrow(df_results) + 1,] = c("Adults drinking sweetened non-soda beverages", national_nonpop[1])

#Statewise wrangling into race, sex and state
state_nonpop <- svyby(~ssbfrut2, ~x.state, svey, svymean, na.rm=T)
Z <- svyby(~ssbfrut2, ~x.state+sex+x.race.g1, svey, svymean, na.rm=T)

#Writing into final CSVs for viz
write.csv(state_nonpop,"./data/sugar_nosoda_state.csv")
write.csv(Z,"./data/sugar_nosoda_race.csv")


#-------------------------------------------------------------------------------------
#EXTRA
#df_results <- df_results[-c(12,13,14),]
rownames(df_results) <- 1:nrow(df_results)
write.csv(df_results,"./data/total_prevalence.csv")
