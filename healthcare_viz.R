library(Hmisc)
library(foreign)
library(survey)
library(dplyr)
library(tidyr)
library(ggplot2)
library(readxl)
library(reshape2)

#Added state names and race names in the CSV files 

healthcareaccess_race <- read.csv("healthcareaccess_race.csv")
healthcareaccess_state <- read.csv("healthcareaccess_state.csv")

##Health care by race

#Coding the sex variables 
healthcareaccess_race$sex[healthcareaccess_race$sex==1] <- "Male"
healthcareaccess_race$sex[healthcareaccess_race$sex==2] <- "Female"

# Removing the data with no sex value
healthcareaccess_race <- healthcareaccess_race[!(healthcareaccess_race$sex == 9),]

#Good health

# Sorting the dataset with top 10 races with best health 
healthcareaccess_race_top10_rfhlth <- head(healthcareaccess_race[order(-healthcareaccess_race$x.rfhlth),],10)

#Coverting the variable to a percentage
healthcareaccess_race_top10_rfhlth$x.rfhlth <- healthcareaccess_race_top10_rfhlth$x.rfhlth*100

#Races with good health overall
ggplot(data=healthcareaccess_race_top10_rfhlth, aes(x=race, y=x.rfhlth, fill=race))+
  geom_bar(stat = "identity", position = position_dodge())+
  labs(x = "State", y = "Adults with good health", title="Adults with good health by state")+coord_flip()

#Most males have good health as compared to females 
ggplot(data=healthcareaccess_race_top10_rfhlth, aes(x=race, y=x.rfhlth, fill= sex))+
  geom_bar(stat = "identity",position = position_dodge())+scale_fill_manual(values=c("light pink", "light blue"))+
  labs(x = "Race", y = "Adults with good health", title="Adults with good health by race")+coord_flip()

ggplot(data=healthcareaccess_race_top10_rfhlth, aes(x=sex, y=x.rfhlth, fill=sex))+
  geom_bar(stat = "identity", position = position_dodge())+scale_fill_manual(values=c("light pink", "light blue"))+
  labs(x = "Sex", y = "Adults with good health", title="Adults with good health by race and sex")

# Health care coverage 

healthcareaccess_race_top10_hlthpln1 <- head(healthcareaccess_race[order(-healthcareaccess_race$hlthpln1),],10)

healthcareaccess_race_top10_hlthpln1$hlthpln1 <- healthcareaccess_race_top10_hlthpln1$hlthpln1*100

#Races with good health coverage overall
ggplot(data=healthcareaccess_race_top10_hlthpln1, aes(x=race, y=hlthpln1, fill= race))+
  geom_bar(stat = "identity", position = position_dodge())+
  labs(x = "State", y = "Adults with health care coverage", title="Adults with good health by state")+coord_flip()

#Health coverage depends on different sex
ggplot(data=healthcareaccess_race_top10_hlthpln1, aes(x=race, y=hlthpln1, fill= sex))+
  geom_bar(stat = "identity",position = position_dodge())+scale_fill_manual(values=c("pink", "light blue"))+
  labs(x = "Race", y = "Adults with health care coverage", title="Adults with good health by race and sex")+coord_flip()

#Access to personal doctor 

healthcareaccess_race_top10_persdoc <- head(healthcareaccess_race[order(-healthcareaccess_race$persdoc2),],10)

#Coverting the variable to a percentage
healthcareaccess_race_top10_persdoc$persdoc2 <- healthcareaccess_race_top10_persdoc$persdoc2*100

ggplot(data=healthcareaccess_race_top10_persdoc, aes(x=race, y=persdoc2, fill= race))+
  geom_bar(stat = "identity", position = position_dodge())+labs(x = "Race", y = "Adults with personal doctor", title="Adults with personal doctor by race")+
  coord_flip()

ggplot(data=healthcareaccess_race_top10_persdoc, aes(x=race, y=persdoc2, fill= sex))+
  geom_bar(stat = "identity",position = position_dodge())+scale_fill_manual(values=c("pink", "light blue"))+
  labs(x = "Race", y = "Adults with personal doctor", title="Adults with good health by race and sex")+coord_flip()

##Health care by state

#Good health
healthcareaccess_state_top10_rfhlth <- head(healthcareaccess_state[order(-healthcareaccess_state$x.rfhlth),],10)

#Coverting the variable to a percentage
healthcareaccess_state_top10_rfhlth$x.rfhlth <- healthcareaccess_state_top10_rfhlth$x.rfhlth*100

#Adults with good health by state
ggplot(data=healthcareaccess_state_top10_rfhlth, aes(x=state_name, y=x.rfhlth, fill=state_name))+
  geom_bar(stat = "identity", position = position_dodge())+
  labs(x = "State", y = "Good health (%)", title="Adults with good health by state")+coord_flip()

# Health care coverage 
healthcareaccess_state_top10_hlthpln1 <- head(healthcareaccess_state[order(-healthcareaccess_state$hlthpln1),],10)

#Converting the variable to a percentage
healthcareaccess_state_top10_hlthpln1$hlthpln1 <- healthcareaccess_state_top10_hlthpln1$hlthpln1*100

ggplot(data=healthcareaccess_state_top10_hlthpln1, aes(x=state_name, y=hlthpln1, fill=state_name))+
  geom_bar(stat = "identity", position = position_dodge())+
  labs(x = "State", y = "Health care coverage (%)", title="Adults with health care coverage by state")+coord_flip()

#Access to personal doctor 
healthcareaccess_state_top10_persdoc <- head(healthcareaccess_state[order(-healthcareaccess_state$persdoc2),],10)

#Coverting the variable to a percentage
healthcareaccess_state_top10_persdoc$persdoc2 <- healthcareaccess_state_top10_persdoc$persdoc2*100

#Access to personal doctor by state
ggplot(data=healthcareaccess_state_top10_persdoc, aes(x=state_name, y=persdoc2, fill=state_name))+
  geom_bar(stat = "identity", position = position_dodge())+
  labs(x = "State", y = "Adults with personal doctor (%)", title="Adults with personal doctor by state")+coord_flip()
