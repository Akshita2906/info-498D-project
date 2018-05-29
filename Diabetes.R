install.packages("wesanderson")
library(wesanderson)


predia_df = read.csv("data/prediabetes_state.csv")
state_table = read.csv("data/state_table.csv")
dia_df = read.csv("data/diabetes_state.csv")


predia_df$fips = as.numeric(predia_df$x.state)
dia_df$fips = as.numeric(predia_df$x.state)
state_table$fips = as.numeric(state_table$value)
predia_df$prediab1 = predia_df$prediab1*100
dia_df$diabete3 = dia_df$diabete3*100

usmap::plot_usmap(data = predia_df, values = "prediab1", lines = "black") + 
  scale_fill_continuous(
    low = "white", high = "#00441b", name = "Prevalence", label = scales::comma
  ) + theme(legend.position = "right") +
  labs(title = "Prevalence in the United States")

usmap::plot_usmap(data = dia_df, values = "diabete3", lines = "black") + 
  scale_fill_continuous(
    low = "white", high = "#00441b", name = "Prevalence", label = scales::comma
  ) + theme(legend.position = "right") +
  labs(title = "Prevalence in the United States")

#--------------------------------------------Bar graph across states--------------------------------------------

bar_state<- left_join(state_table, predia_df, by = "fips")
bar_state[is.na(bar_state)] <- 0


bar_state = bar_state[bar_state$prediab1 > 0,]
bar_state = bar_state[order(-bar_state$prediab1),]

#All States without NA values
ggplot(bar_state, aes(x = reorder(state,-prediab1) , prediab1, fill = prediab1)) + theme_bw() + geom_bar(stat="identity")   +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  scale_fill_gradient(low="#41ae76", high="#00441b") +
  ggtitle("Prevalence of Diabetes across States**") + 
  xlab("Metrics") + ylab("Prevalence Values (in %)") 

#bar_state_top10 = head(bar_state,10)
ggplot(bar_state_top10, aes(x = reorder(state,-prediab1) , prediab1, fill = prediab1)) + theme_bw() + geom_bar(stat="identity")   +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  scale_fill_gradient(low="#41ae76", high="#00441b") +
  ggtitle("Prevalence of Diabetes across top 10 States**") + 
  xlab("Metrics") + ylab("Prevalence Values (in %)") 

#bar_state_bot_10 = tail(bar_state,10)
ggplot(bar_state_bot_10, aes(x = reorder(state,-prediab1) , prediab1, fill = prediab1)) + theme_bw() + geom_bar(stat="identity")   +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  scale_fill_gradient(low="#41ae76", high="#00441b") +
  ggtitle("Prevalence of Diabetes across bottom 10 States**") + 
  xlab("Metrics") + ylab("Prevalence Values (in %)")  
