
library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  pageWithSidebar(
    headerPanel('Health Risk: Obesity'),
    sidebarPanel(
      selectInput('sex', 'Sex',  list('Male' = 1,
                                      'Female' = 2, 
                                      'Both' = 3)
      ),
      selectInput('x.race.g1', 'Race',  list('White - Non-Hispanic' = 1,
                                             'Black - Non-Hispanic ' = 2, 
                                             'Hispanic' = 3, 
                                             'Other race only, Non-Hispanic' = 4, 
                                             'Multiracial, Non-Hispanic' = 5, 
                                             'All' = 6)
      )),
    
    mainPanel(
      plotOutput("result"),
      textOutput("blank"),
      plotOutput("result2")
    )
  )
)) 
