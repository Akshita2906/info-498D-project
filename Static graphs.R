library(shiny)
library(ggplot2)
theme_set(theme_bw())
library(plotly)
library(dplyr)
library(gridExtra)

state_table = read.csv("./data/state_table.csv")
state_table$fips = as.numeric(state_table$value)
#---------------------------Obesity-------------------------------------------------------------
obesity_df = read.csv("./data/obesity_race.csv")
obesity_state <- read.csv("./data/obesity_state.csv")

#Picking top 10 states with highest percent of overweight population
obesity_state %>% arrange(-x.rfbmi5) %>% head(10) -> obesity_state_top10
colnames(obesity_state_top10)
names(obesity_state_top10)[names(obesity_state_top10) == "x.state"] <- "fips"
l<- obesity_state_top10$fips
obesity_df %>% filter(fips %in% l) -> check
colnames(check)

#Creating dataframes for vizualization
viz_df<- left_join(check,state_table, by = "fips")
viz_top10 <- left_join(obesity_state_top10,state_table, by = "fips")
viz_df$sex <- as.character(viz_df$sex)
viz_df$x.race.g1 <- as.character(viz_df$x.race.g1)
viz_df$x.rfbmi5 <- viz_df$x.rfbmi5*100
viz_df %>% filter(x.race.g1 == 6) -> viz_df_sex
viz_df %>% filter(sex == 3) ->viz_df_race

#Plotting only for sex - male and female
p1 <- ggplot(data = viz_df_sex, aes(x = reorder(state,x.rfbmi5), y = x.rfbmi5, fill = sex)) +
  geom_bar(stat="identity", position="dodge", width = 0.5, color = "black") +
  ylim(c(0,100)) +
  scale_fill_manual(values =c("#576ddb","#FFB6C1"),
                    name="Gender",
                    breaks=c("1", "2"),
                    labels=c("Male", "Female")) +
  labs(y = "Percent of obese people", x = "States", title = "Gender-wise breakdown of Obese/Overweight percentage of population")


#Plotting for races - both male and female
p2<- ggplot(data = viz_df_race, aes(x = reorder(state,x.rfbmi5), y = x.rfbmi5, fill = x.race.g1)) +
  geom_bar(stat="identity", position="dodge", width = 0.75, color = "black") +
  ylim(c(0,100)) +
  labs(y = "Percent of obese people", x = "States", title = "Race-wise breakdown of Obese/Overweight percentage of population") +
  scale_fill_manual(values =c("#224D17", "#099441","#60A830","#9FDA40","#D9DF1D"),
                      name="Races",
                         breaks=c("1", "2", "3", "4", "5"),
                         labels=c("White - Non-Hispanic", "Black - Non-Hispanic", "Hispanic", "Other race only, Non-Hispanic","Multiracial, Non-Hispanic"))


grid.arrange(p1,p2, nrow=2, ncol=1)

#--------------------------------Diabetes--------------------------------------------------------
dia_df = read.csv("./data/diabetes_race.csv")
dia_state <- read.csv("./data/diabetes_state.csv")

#Picking top 10 states with highest percent of overweight population
dia_state %>% arrange(-diabete3) %>% head(10) -> dia_state_top10
#colnames(dia_state_top10)
names(dia_df)[names(dia_df) == "x.state"] <- "fips"
names(dia_state_top10)[names(dia_state_top10) == "x.state"] <- "fips"
l<- dia_state_top10$fips
dia_df %>% filter(fips %in% l) -> dia_race_top10
#colnames(dia_race_top10)

#Creating dataframes for vizualization
viz_df_dia<- left_join(dia_race_top10,state_table, by = "fips")
viz_top10_dia <- left_join(dia_state_top10,state_table, by = "fips")
viz_df_dia$sex <- as.character(viz_df_dia$sex)
viz_df_dia$x.race.g1 <- as.character(viz_df_dia$x.race.g1)
viz_df_dia$diabete3 <- viz_df_dia$diabete3*100
viz_df_dia %>% filter(x.race.g1 == 6) -> viz_df_dia_sex
viz_df_dia %>% filter(sex == 3) ->viz_df_dia_race

#Plotting only for sex - male and female
p1 <- ggplot(data = viz_df_dia_sex, aes(x = reorder(state,diabete3), y = diabete3, fill = sex)) +
  geom_bar(stat="identity", position="dodge", width = 0.5, color = "black") +
  ylim(c(0,100)) +
  scale_fill_manual(values =c("#576ddb","#FFB6C1"),
                    name="Gender",
                    breaks=c("1", "2"),
                    labels=c("Male", "Female")) +
  labs(y = "Percent of diabetic people", x = "States", title = "Gender-wise breakdown of Diabetic percentage of population")


#Plotting for races - both male and female
p2<- ggplot(data = viz_df_dia_race, aes(x = reorder(state,diabete3), y = diabete3, fill = x.race.g1)) +
  geom_bar(stat="identity", position="dodge", width = 0.75, color = "black") +
  ylim(c(0,100)) +
  labs(y = "Percent of diabetic people", x = "States", title = "Race-wise breakdown of Diabetic percentage of population") +
  scale_fill_manual(values =c("#224D17", "#099441","#60A830","#9FDA40","#D9DF1D"),
                    name="Races",
                    breaks=c("1", "2", "3", "4", "5"),
                    labels=c("White - Non-Hispanic", "Black - Non-Hispanic", "Hispanic", "Other race only, Non-Hispanic","Multiracial, Non-Hispanic"))


grid.arrange(p1,p2, nrow=2, ncol=1)

#--------------------------------PreDiabetes--------------------------------------------------------

predia_df <- read.csv("./data/prediabetes_race.csv")
predia_state<- read.csv("./data/prediabetes_state.csv")

#Picking top 10 states with highest percent of overweight population
predia_state %>% arrange(-prediab1) %>% head(10) -> predia_state_top10
predia_state_top10$prediab1 <- predia_state_top10$prediab1*100

#Changing column names
names(predia_df)[names(predia_df) == "x.state"] <- "fips"
names(predia_state_top10)[names(predia_state_top10) == "x.state"] <- "fips"
viz_df_predia<- left_join(predia_state_top10,state_table, by = "fips")
#Plotting top 10 states who have highest prediabetic percentage
ggplot(data = viz_df_predia, aes(x = reorder(state,prediab1), y = prediab1, group = 1)) +
  geom_point(size = 5, color = "darkgreen") +
  ylim(0,100) +
  theme_bw() + 
  geom_line() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(color='grey60', linetype='dashed')) +
  guides(fill = FALSE) +
  labs(x = "States", y = "Percentage of people diagnosed with PreDiabetes")

#------------------------------Sugar-----------------------------------------------------------

soda_state <- read.csv("./data/sugar_soda_state.csv")
soda_race <- read.csv("./data/sugar_soda_race.csv")
nosoda_state <- read.csv("./data/sugar_nosoda_state.csv")
nosoda_race <- read.csv("./data/sugar_nosoda_race.csv")

#Finding total for state
sugar_state <- cbind(soda_state,nosoda_state)
sugar_state$total <- sugar_state$ssbsugr2 + sugar_state$ssbfrut2

#Renaming
sugar_state <-  sugar_state[,c(2,3,7,9)]
names(sugar_state)[names(sugar_state) == "x.state"] <- "fips"
viz_df_sugarstate<- left_join(sugar_state,state_table, by = "fips")

#Finding total by race 
sugar_race <- cbind(soda_race,nosoda_race)
sugar_race$total <- sugar_race$ssbsugr2 + sugar_race$ssbfrut2
sugar_race <-  sugar_race[,c(2,3,4,5,10,11)]
viz_df_sugarrace<- left_join(sugar_race,state_table, by = "fips")

#making dataframes ready for viz
viz_df_sugarrace$sex <- as.character(viz_df_sugarrace$sex)
viz_df_sugarrace$x.race.g1 <- as.character(viz_df_sugarrace$x.race.g1)
#viz_df_sugarrace$total <- viz_df_sugarrace$total*100
viz_df_sugarrace %>% filter(x.race.g1 == 6) -> viz_df_sugar_sex
viz_df_sugarrace %>% filter(sex == 3) ->viz_df_sugar_race


#Plotting only for sex - male and female
p1 <- ggplot(data = viz_df_sugar_sex, aes(x = reorder(state,total), y = total, fill = sex)) +
  geom_bar(stat="identity", position="dodge", width = 0.5, color = "black") +
  ylim(c(0,20)) +
  scale_fill_manual(values =c("#576ddb","#FFB6C1"),
                    name="Gender",
                    breaks=c("1", "2"),
                    labels=c("Male", "Female")) +
  labs(y = "Average number of SSBs consumed in a week", x = "States", title = "Gender-wise breakdown of SSBs consumed in State")


#Plotting for races - both male and female
p2<- ggplot(data = viz_df_sugar_race, aes(x = reorder(state,total), y = total, fill = x.race.g1)) +
  geom_bar(stat="identity", position="dodge", width = 0.75, color = "black") +
  ylim(c(0,20)) +
  labs(y = "Average number of SSBs consumed in a week", x = "States", title = "Race-wise breakdown of SSBs consumed in State") +
  scale_fill_manual(values =c("#224D17", "#099441","#60A830","#9FDA40","#D9DF1D"),
                    name="Races",
                    breaks=c("1", "2", "3", "4", "5"),
                    labels=c("White - Non-Hispanic", "Black - Non-Hispanic", "Hispanic", "Other race only, Non-Hispanic","Multiracial, Non-Hispanic"))


grid.arrange(p1,p2, nrow=2, ncol=1)

#------------------------------Heart Disease-----------------------------------------------------------

heart_state <- read.csv("./data/heartdisease_state.csv")
heart_race <- read.csv("./data/heartdisease_race.csv")

#Renaming and adding states
heart_state <-  heart_state[,c(2,3)]
names(heart_state)[names(heart_state) == "x.state"] <- "fips"
heart_race <-  heart_race[,c(2,3,4,5)]

#Picking top 10 states with highest percent of overweight population
heart_state %>% arrange(-x.michd) %>% head(10) -> heart_state_top10

l<- heart_state_top10$fips
heart_race %>% filter(fips %in% l) -> heart_race_top10
viz_df_heartrace <- left_join(heart_race_top10,state_table, by = "fips")

#Converting into categorical variables
viz_df_heartrace$sex <- as.character(viz_df_heartrace$sex)
viz_df_heartrace$x.race.g1 <- as.character(viz_df_heartrace$x.race.g1)
viz_df_heartrace$x.michd <- viz_df_heartrace$x.michd*100
viz_df_heartrace %>% filter(x.race.g1 == 6) -> viz_df_heartsex
viz_df_heartrace %>% filter(sex == 3) ->viz_df_heart_race


#Plotting only for sex - male and female
p1 <- ggplot(data = viz_df_heartsex, aes(x = reorder(state,x.michd), y = x.michd, fill = sex)) +
  geom_bar(stat="identity", position="dodge", width = 0.5, color = "black") +
  ylim(c(0,100)) +
  scale_fill_manual(values =c("#576ddb","#FFB6C1"),
                    name="Gender",
                    breaks=c("1", "2"),
                    labels=c("Male", "Female")) +
  labs(y = "Percentage of population", x = "States", title = "Gender-wise breakdown of Heart Disease prevalence in each state")


#Plotting for races - both male and female
p2<- ggplot(data = viz_df_heart_race, aes(x = reorder(state,x.michd), y = x.michd, fill = x.race.g1)) +
  geom_bar(stat="identity", position="dodge", width = 0.75, color = "black") +
  ylim(c(0,100)) +
  labs(y = "Percentage of population", x = "States", title = "Race-wise breakdown of Heart Disease Prevalence in each state") +
  scale_fill_manual(values =c("#224D17", "#099441","#60A830","#9FDA40","#D9DF1D"),
                    name="Races",
                    breaks=c("1", "2", "3", "4", "5"),
                    labels=c("White - Non-Hispanic", "Black - Non-Hispanic", "Hispanic", "Other race only, Non-Hispanic","Multiracial, Non-Hispanic"))


grid.arrange(p1,p2, nrow=2, ncol=1)

#------------------------------------Healthcare Access ---------------------------------------

health_state <- read.csv("./data/healthcareaccess_state.csv")
health_race <- read.csv("./data/healthcareaccess_race.csv")

#Renaming and adding states
health_state <-  health_state[,c(2,3,7)]
names(health_state)[names(health_state) == "x.state"] <- "fips"
health_race <-  health_race[,c(2,3,4,5,13)]

#Picking top 10 states with lowest percent of goodhealth population
health_state %>% arrange(x.rfhlth) %>% head(10) -> health_bottom10
health_state %>% arrange(hlthpln1) %>% head(10) -> health2_bottom10

l<- health_bottom10$fips
health_race %>% filter(fips %in% l) -> health_race_bottom10
l2<- health2_bottom10$fips
health_race %>% filter(fips %in% l2) -> health2_race_bottom10
viz_df_health <- left_join(health_race_bottom10,state_table, by = "fips")
viz_df_health2 <- left_join(health2_race_bottom10,state_table, by = "fips")

#Converting into categorical variables
viz_df_health$sex <- as.character(viz_df_health$sex)
viz_df_health$x.race.g1 <- as.character(viz_df_health$x.race.g1)
viz_df_health %>% filter(sex == 3) ->viz_df_health_race
viz_df_health_race$x.rfhlth <- viz_df_health_race$x.rfhlth*100
viz_df_health2$sex <- as.character(viz_df_health2$sex)
viz_df_health2$x.race.g1 <- as.character(viz_df_health2$x.race.g1)
viz_df_health2 %>% filter(sex == 3) ->viz_df_health2_race
viz_df_health2_race$hlthpln1 <- viz_df_health2_race$hlthpln1 *100

p1 <-ggplot(data = viz_df_health_race, aes(x = reorder(state,x.rfhlth), y = x.rfhlth, fill = x.race.g1)) +
  geom_bar(stat="identity", position="dodge", width = 0.75, color = "black") +
  ylim(c(0,100)) +
  labs(y = "Percentage of population", x = "States", title = "Race-wise breakdown of Good health in a population") +
  scale_fill_manual(values =c("#224D17", "#099441","#60A830","#9FDA40","#D9DF1D"),
                    name="Races",
                    breaks=c("1", "2", "3", "4", "5"),
                    labels=c("White - Non-Hispanic", "Black - Non-Hispanic", "Hispanic", "Other race only, Non-Hispanic","Multiracial, Non-Hispanic"))

p2<- ggplot(data = viz_df_health2_race, aes(x = reorder(state,hlthpln1), y = hlthpln1, fill = x.race.g1)) +
  geom_bar(stat="identity", position="dodge", width = 0.75, color = "black") +
  ylim(c(0,100)) +
  labs(y = "Percentage of population", x = "States", title = "Race-wise breakdown of health coverage in a population") +
  scale_fill_manual(values =c("#224D17", "#099441","#60A830","#9FDA40","#D9DF1D"),
                    name="Races",
                    breaks=c("1", "2", "3", "4", "5"),
                    labels=c("White - Non-Hispanic", "Black - Non-Hispanic", "Hispanic", "Other race only, Non-Hispanic","Multiracial, Non-Hispanic"))

grid.arrange(p1,p2, nrow=2, ncol=1)
