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


obesity_df = read.csv("obesity_race.csv")
state_table = read.csv("state_table.csv")
preval_df = read.csv("total_prevalence.csv")
prev_value = preval_df[preval_df$Variable=='x.rfbmi5','Value']
state_table$fips = as.numeric(state_table$value)
obesity_df$x.rfbmi5 = round(obesity_df$x.rfbmi5 * 100,0)
viz_df<- left_join(obesity_df,state_table, by = "fips")
viz_df <- mutate(viz_df,type = case_when( x.rfbmi5 > prev_value~1, x.rfbmi5 <= prev_value~0))

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  output$result <- renderPlot({
    viz_df = viz_df[viz_df$x.race.g1==input$x.race.g1 ,]
    viz_df = viz_df[viz_df$sex==input$sex ,]
    
    usmap::plot_usmap(data = viz_df, values = "x.rfbmi5", lines = "black") + 
      scale_fill_continuous(
        low = "white", high = "#00441b", name = "Prevalence", label = scales::comma
      ) + theme(legend.position = "right") +
      labs(title = "Prevalence of adults with higher in the United States")
    
    #paste("You chose", input$sex, "You chose", input$x.race.g1 )
  })   

  
  output$blank <- renderText({
    paste('\n')
    paste('\n')
    paste('\n')
    paste('\n')
    
    
    
  })
    
    
    
  output$result2 <- renderPlot({  
    # Plot
    viz_df = viz_df[viz_df$x.race.g1==input$x.race.g1 ,]
    viz_df = viz_df[viz_df$sex==input$sex ,]
    ggplot(viz_df, aes(x = state, y = x.rfbmi5 , label=x.rfbmi5)) + 
      geom_point(stat='identity', aes(col=type), size=8)  +
      scale_fill_gradient(low="#238b45", high="#00441b") +
      geom_text(color="white", size=3, fontface = "bold") +
      labs(title="BMI Levels Across States in the United States of America") + 
      coord_flip()
  }) 
  
})
