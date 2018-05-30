#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
Avgs <- read.csv("./Modified Dataframes/total_prevalence.csv")
state_table <- read.csv("./Modified Dataframes/state_table.csv")
state_table$fips = as.numeric(state_table$value)
all_heart <- read.csv("./Modified Dataframes/heartdisease.csv")
ob <- read.csv("./Modified Dataframes/obesity.csv")
ov <- read.csv("./Modified Dataframes/overview.csv")
Z <- read.csv("./Modified Dataframes/exp2.csv")
sug <- read.csv("./Modified Dataframes/sugar.csv")
sug <- melt(sug, id=c("fips", 'sex', "x.race.g1"))
sug <- mutate(sug, variable = case_when(variable=="ssbsugr2"~1, variable=="ssbfrut2"~2))

sug<- left_join(sug,state_table, by = "fips")


Z$diabete3 <- Z$diabete3 * 100
Z <- left_join(Z,state_table, by = "fips")



ob<- left_join(ob,state_table, by = "fips")

#ob <- mutate(ob,type = case_when( x.rfbmi5 > Avgs[9,4]~'high', x.rfbmi5 <= Avgs[9,4]~'low'))

ob$x.rfbmi5 <- ob$x.rfbmi5 * 100
ob$x.rfbmi5 <- round(ob$x.rfbmi5,2)




state_table$fips = as.numeric(state_table$value)
hd_df1<- left_join(all_heart,state_table, by = "fips")
hd_df1$value.x = hd_df1$value.x*100
hd_df1 <- mutate(hd_df1, variable = case_when(variable=="x.michd"~1, variable=="cvdinfr4"~2, variable=="cvdstrk3"~3, variable=="cvdcrhd4"~4))


ov <- mutate(ov, value = case_when(value<1~value*100, value>=1~value*1))

# Define server logic required to draw a histogram
colnames(Z)[2] <- "fips"
state_table <- read.csv("./Modified Dataframes/state_table.csv")

state_table$fips = as.numeric(state_table$value)


#viz_df <- mutate(viz_df,type = case_when( x.rfbmi5 > prev_value~1, x.rfbmi5 <= prev_value~0))

X <- read.csv("./Modified Dataframes/healthcareaccess_race.csv")
Y <- read.csv("./Modified Dataframes/healthcareaccess_state.csv")
Avgs <- read.csv("./Modified Dataframes/total_prevalence.csv")

X <- select(X, 2:5, 9,13)
Y <- select(Y, 2,3,5,7)
Y["sex"] <- 3
Y["x.race.g1"] <-6
colnames(Y)[1] <- "fips"

X <- rbind(X, Y)
X <- melt(X, id= c("fips", "sex", "x.race.g1"))
X <- mutate(X, variable = case_when(variable=="hlthpln1"~0, variable=="persdoc2"~1, variable=="x.rfhlth"~2))
#write.csv(X, "HCA.csv")

# X <- read.csv("HCA.csv")

X$variable <- as.numeric(X$variable)


state_table$fips = as.numeric(state_table$value)

X$value <- X$value * 100

X <- left_join(X,state_table, by = "fips")
X <- select(X, -6)
colnames(X)[5] <- "value"

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  output$resultOV <- renderPlot({
    
    ov = ov[ov$metric==input$overview,]
    
    if (input$overview==1){
      gtitle <- "Prevalence of Diabetes across United States"
    } else if (input$overview==2){
      gtitle <- "Percentage of population enrolled in a Health Plan"
    } else if (input$overview==3){
      gtitle <- "Percentage of population who have a Personal Doctor"
    } else if (input$overview==4){
      gtitle <- "Percentage of who believe that have good health"
    } else if (input$overview==5){
      gtitle <- "Percentage of population diagnosed with Heart Disease"
    } else if (input$overview==9){
      gtitle <- "Percentage of population who are either Overweight or Obese"
    } else if (input$overview==10){
      gtitle <- "Prevalence of Prediabetes across United States"
    } else if (input$overview==11){
      gtitle <- "Weekly Consumption of Non Soda Sugar Sweetend Beverages"
    } else {
      gtitle <- "Weekly Consumption of Soda Sugar Sweetend Beverages"
    }
    
    
    
    
    usmap::plot_usmap(data = ov, values = "value", lines = "black") + 
      scale_fill_continuous(
        low = "#e5f5f9", high = "#00441b", name = "Prevalence", label = scales::comma
      ) + theme(legend.position = "right") + labs(title = gtitle)
    
    
    #paste("You chose", input$sex, "You chose", input$x.race.g1 )
  })   
  
  
  output$hresult <- renderPlot({
    
    if (input$hmetric == 0){
      gtitle <- " Percentage of adults with Healthcare Coverage"
      threshold <- Avgs[2,4]
      
    }
    else if (input$hmetric == 1){
      gtitle <- "Percentage of adults who have a Personal Doctor"
      threshold <- Avgs[3,4]
    }
    else {
      gtitle <- "Percentage of Adults who have Good Health"
      threshold <- Avgs[1,4]
    } 
    
    
    X = X[X$sex==input$hsex,]
    X = X[X$x.race.g1 == input$hrace,]
    X = X[X$variable== input$hmetric,]
    
    
    usmap::plot_usmap(data = X, values = "value", lines = "black") + 
      scale_fill_continuous(
        low = "#e5f5f9", high = "#00441b", name = "Prevalence", label = scales::comma
      ) + theme(legend.position = "right") +
      labs(title = gtitle)
    
    
  })   
  
  
  output$hblank <- renderText({
    paste('\n')
    paste('\n')
    paste('\n')
    paste('\n')
    
    
    
  })
  
  
  
  output$hresult2 <- renderPlotly({  
    
    if (input$hmetric == 0){
      gtitle <- " Percentage of adults with Healthcare Coverage"
      threshold <- Avgs[2,4]
      
    }
    else if (input$hmetric == 1){
      gtitle <- "Percentage of adults who have a Personal Doctor"
      threshold <- Avgs[3,4]
    }
    else {
      gtitle <- "Percentage of Adults who have Good Health"
      threshold <- Avgs[1,4]
    } 
    
    X <- X[X$sex==input$hsex,]
    X = X[X$x.race.g1 == input$hrace,]
    X <- X[X$variable== input$hmetric,]
    
    
    # 
    # g <- ggplot(X, aes(x = X$state, y = X$value))  + geom_bar(stat='identity') +coord_cartesian(ylim=c((min(X$value)-3), (max(X$value)+3)))+scale_fill_gradient(low="#41ae76", high="#00441b") + ggtitle("Prevalence of adults with healthplan across States**") + xlab("Metrics") + ylab("Prevalence Values (in %)") 
    # g <- ggplotly(g)
    # g
    # 
    # scale_fill_gradient(low="#238b45", high="#00441b") +
    # geom_text(color="white", size=3, fontface = "bold") +
    # labs(title="BMI Levels Across States in the United States of America") + 
    # coord_flip()
    
    
    g <- ggplot(X, aes(x = reorder(X$state,-value), y=value, fill = value)) + theme_bw() + geom_bar(stat="identity")+ scale_fill_gradient(low="#41ae76", high="#00441b") + ggtitle(gtitle) + xlab("State") + ylab("Prevalence Values (in %)") +coord_cartesian(ylim=c((min(X$value)-3), (max(X$value)+3))) + geom_hline(yintercept=threshold, linetype="dashed", color = "orange", size=0.5) + theme(legend.position="none")
    
    
    ggplotly(g)
    
  }) 
  
  
  
  
  output$result <- renderPlot({
    
    if (input$diametric == 1){
      gtitle <- " Percentage of adults with Diabetes"
      threshold <- Avgs[10,4]
      
    }
    else {
      gtitle <- "Percentage of Adults with Prediabetes"
      threshold <- Avgs[11,4]
    } 
    
    
    Z = Z[Z$sex==input$diasex,]
    Z = Z[Z$race == input$diarace,] 
    Z = Z[Z$dia== input$diametric,]
    
    
    # X <- filter(X, X$Dia == input$diametric) %>% (X$sex== input$diasex) %>% (X$x.race.g1== input$diarace)
    # X <- X[c("fips", "diabete3"),]
    
    usmap::plot_usmap(data = Z, values = "diabete3", lines = "black") + 
      scale_fill_continuous(
        low = "#e5f5f9", high = "#00441b", name = "Prevalence", label = scales::comma
      ) + theme(legend.position = "right") +
      labs(title = gtitle)
    
    #paste("You chose", input$sex, "You chose", input$x.race.g1 )
  })   
  
  
  output$blank <- renderText({
    paste('\n')
    paste('\n')
    paste('\n')
    paste('\n')
    
    
    
  })
  
  
  
  output$dresult2 <- renderPlotly({
    
    if (input$diametric == 1){
      gtitle <- " Percentage of adults with Diabetes"
      threshold <- Avgs[10,4]
      
    }
    else {
      gtitle <- "Percentage of Adults with Prediabetes"
      threshold <- Avgs[11,4]
    } 
    
    # Plot
    
    
    Z = Z[Z$sex==input$diasex,]
    Z = Z[Z$race == input$diarace,]  
    Z = Z[Z$dia== input$diametric,]
    
    # g <- ggplot(X, aes(x = X$state, y = X$diabete3)) + 
    #   geom_bar(stat='identity') + scale_fill_gradient(low="#41ae76", high="#00441b")
    # 
    # g <- ggplotly(g)
    
    g <- ggplot(Z, aes(x = reorder(Z$state,-diabete3), y=diabete3, fill = diabete3)) + theme_bw() + geom_bar(stat="identity")+ scale_fill_gradient(low="#41ae76", high="#00441b") + ggtitle(gtitle) + xlab("State") + ylab("Prevalence Values (in %)") +coord_cartesian(ylim=c((min(Z$diabete3)-3), (max(Z$diabete3)+3))) + geom_hline(yintercept=threshold, linetype="dashed", color = "orange", size=0.5) + theme(legend.position="none") 
    
    ggplotly(g)
    
    
    
  }) 
  
  output$blankOV <- renderText({
    paste('\n')
    paste('\n')
    paste('\n')
    paste('\n')
    
  })
  
  output$result1 <- renderPlot({
    
    if (input$variable1 == 1){
      gtitle <- " Percentage of adults with Coronary Heart Disease OR Myocardial Infarction"
      threshold <- Avgs[8,4]
      
    }
    else if (input$variable1 == 2){
      gtitle <- " Percentage of adults with Myocardial Infarction"
      threshold <- Avgs[5,4]
    }
    else if (input$variable1 == 3){
      gtitle <- " Percentage of adults with Stroke"
      threshold <- Avgs[7,4]
      
    } else {
      gtitle <- " Percentage of adults with Coronary Heart Disease"
      threshold <- Avgs[6,4]
      
    }
    
    
    hd_df1 = hd_df1[hd_df1$sex == input$sex1,]
    hd_df1 = hd_df1[hd_df1$x.race.g1 == input$x.race.g11,]
    hd_df1 = hd_df1[hd_df1$variable == input$variable1,]
    
    
    usmap::plot_usmap(data = hd_df1, values = "value.x", lines = "black") + 
      scale_fill_continuous(
        low = "white", high = "#00441b", name = "Prevalence", label = scales::comma
      ) + theme(legend.position = "right", plot.title = element_text(hjust = 0.5, size = 20, family = "Helvetica")) +
      ggtitle(gtitle) 
    
    #paste("You chose", input$sex, "You chose", input$x.race.g1 )
  })
  
  output$result2 <- renderPlotly({  
    
    if (input$variable1 == 1){
      gtitle <- " Percentage of adults with Coronary Heart Disease OR Myocardial Infarction"
      threshold <- Avgs[8,4]
      
    }
    else if (input$variable1 == 2){
      gtitle <- " Percentage of adults with Myocardial Infarction"
      threshold <- Avgs[5,4]
    }
    else if (input$variable1 == 3){
      gtitle <- " Percentage of adults with Stroke"
      threshold <- Avgs[7,4]
      
    } else {
      gtitle <- " Percentage of adults with Coronary Heart Disease"
      threshold <- Avgs[6,4]
      
    }
    # Plot
    hd_df1 = hd_df1[hd_df1$sex == input$sex1,]
    hd_df1 = hd_df1[hd_df1$x.race.g1 == input$x.race.g11,]
    hd_df1 = hd_df1[hd_df1$variable == input$variable1,]
    
    
    gheart = ggplot(hd_df1, aes(x = reorder(state,-value.x) , value.x, fill = value.x)) + theme_bw() + geom_bar(stat="identity")   +
      scale_fill_gradient(low="#41ae76", high="#00441b") +
      ggtitle(gtitle) + 
      xlab("Metrics") + ylab("Prevalence Values (in %)") + geom_hline(yintercept=threshold, linetype="dashed", color = "orange", size=0.5) + theme(legend.position="none")
    
    ggplotly(gheart)
  }) 
  
  output$resultO <- renderPlot({
    ob = ob[ob$x.race.g1==input$raceo ,]
    ob = ob[ob$sex==input$sexo ,]
    
    usmap::plot_usmap(data = ob, values = "x.rfbmi5", lines = "black") + 
      scale_fill_continuous(
        low = "white", high = "#00441b", name = "Prevalence", label = scales::comma
      ) + theme(legend.position = "right", plot.title = element_text(hjust = 0.5, size = 20, family = "Helvetica")) +
      ggtitle("Percentage of adults who are either overweight or obese")
    
    #paste("You chose", input$sex, "You chose", input$x.race.g1 )
  })   
  
  output$blankO <- renderText({
    paste('\n')
    paste('\n')
    paste('\n')
    paste('\n')
    
  })
  
  output$resultO2 <- renderPlotly({  
    # Plot
    
    
    
    ob = ob[ob$x.race.g1==input$raceo ,]
    ob = ob[ob$sex==input$sexo ,]
    
    g = ggplot(ob, aes(x = state, y = x.rfbmi5, label=x.rfbmi5, color=x.rfbmi5)) + 
      geom_point(stat='identity', size=8)  +
      scale_color_gradient(low="#00441b", high="#238b45") + 
      geom_text(color="white", size=3, fontface = "bold") + 
      ggtitle("Prevalence of population having BMI>25 in United States of America") + ylab("Net BMI (in %)") +
      theme(legend.position = "right", plot.title = element_text(hjust = 0.5, size = 17, family = "Helvetica")) +
      xlab("State")+ geom_hline(yintercept=Avgs[9,4], linetype="dashed", color = "orange", size=0.5)  + coord_flip() 
    ggplotly(g)
  }) 
  
  output$results <- renderPlot({
    sug = sug[sug$x.race.g1==input$races ,]
    sug = sug[sug$sex==input$sexs ,]
    sug = sug[sug$variable== input$variables]
    
    usmap::plot_usmap(data = sug, values = "value.x", lines = "black") + 
      scale_fill_continuous(
        low = "white", high = "#00441b", name = "Prevalence", label = scales::comma
      ) + theme(legend.position = "right", plot.title = element_text(hjust = 0.5, size = 20, family = "Helvetica")) +
      ggtitle("Percentage of adults who are either overweight or obese")
    
    #paste("You chose", input$sex, "You chose", input$x.race.g1 )
  })   
  output$results2 <- renderPlotly({  
    # Plot
    
    
    if (input$variables==1){threshold <- Avgs[12,4]}
    else{threshold <- Avgs[13,4]}
    
    sug = sug[sug$x.race.g1==input$races ,]
    sug = sug[sug$sex==input$sexs ,]
    sug = sug[sug$variable== input$variables]
    
    #sug <- mutate(sug, type = case_when(value.x<threshold)~1, value.x>=threshold~0)
    
    g = ggplot(sug, aes(x = state, y = value.x, label=value.x, color=value.x)) + 
      geom_point(stat='identity', size=8)  +
      scale_color_gradient(low="#00441b", high="#238b45") + 
      geom_text(color="white", size=3, fontface = "bold") + 
      ggtitle("Prevalence of population having BMI>25 in United States of America") + ylab("Net BMI (in %)") +
      theme(legend.position = "right", plot.title = element_text(hjust = 0.5, size = 17, family = "Helvetica")) +
      xlab("State")+ geom_hline(yintercept=threshold, linetype="dashed", color = "orange", size=0.5)  + coord_flip() 
    ggplotly(g)
  }) 
  
 
  
})
