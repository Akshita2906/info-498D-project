#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
theme_set(theme_bw())
library(plotly)


obesity_df = read.csv("obesity_race.csv")
state_table = read.csv("state_table.csv")
preval_df = read.csv("total_prevalence.csv")
prev_value = preval_df[preval_df$Variable=='x.rfbmi5','Value']
state_table$fips = as.numeric(state_table$value)
obesity_df$x.rfbmi5 = round(obesity_df$x.rfbmi5 * 100,0)
viz_df<- left_join(obesity_df,state_table, by = "fips")
viz_df <- mutate(viz_df,type = case_when( x.rfbmi5 > prev_value~'high', x.rfbmi5 <= prev_value~'low'))

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  output$result <- renderPlot({
    viz_df = viz_df[viz_df$x.race.g1==input$x.race.g1 ,]
    viz_df = viz_df[viz_df$sex==input$sex ,]
    
    usmap::plot_usmap(data = viz_df, values = "x.rfbmi5", lines = "black") + 
      scale_fill_continuous(
        low = "white", high = "#00441b", name = "Prevalence", label = scales::comma
      ) + theme(legend.position = "right", plot.title = element_text(hjust = 0.5, size = 20, family = "Helvetica")) +
      ggtitle("Prevalence of adults with higher in the United States")
    
    #paste("You chose", input$sex, "You chose", input$x.race.g1 )
  })   
  
  
  output$blank <- renderText({
    paste('\n')
    
    
  })
  output$blank1 <- renderText({
    paste('\n')
    
    
  })
  
  
  output$result2 <- renderPlotly({  
    # Plot
    viz_df = viz_df[viz_df$x.race.g1==input$x.race.g1 ,]
    viz_df = viz_df[viz_df$sex==input$sex ,]
    names(viz_df) <- c("x", "fips", "sex", "race", "BMI", "value", "State", "Threshold")
    g = ggplot(viz_df, aes(x = State, y = BMI , label=BMI)) + 
      geom_point(stat='identity', aes(fill=Threshold), size=8)  +
      scale_fill_manual(values = c( "#00441b", "#238b45")) +
      geom_text(color="white", size=3, fontface = "bold") + 
      ggtitle("Prevalence of population having BMI>25 in United States of America") + ylab("Net BMI (in %)") +
      theme(legend.position = "right", plot.title = element_text(hjust = 0.5, size = 17, family = "Helvetica")) +
      xlab("State") + coord_flip() 
    ggplotly(g)
  }) 
  
})
