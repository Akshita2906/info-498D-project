#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(crosstalk)
library(reshape2)

navbarPage("Health in the United States",
           tabPanel("Overview",
                    sidebarLayout(sidebarPanel(
                      
                      
                      selectInput("overview",
                                  label = "Parameter:",
                                  list("Diabetes" = 1,
                                       "Prediabetes" = 10,
                                       "Personal Doctor" = 3,
                                       "Good Health" = 4,
                                       "Enrolled in Health Plan" = 2,
                                       "Net Heart Disease" = 5,
                                       "Obesity" = 9,
                                       "Non Soda Sugar Sweetened Beverage Consumption" = 11,
                                       "Soda Sugar Sweetened Beverage Consumption" = 12),
                                  selected = 1)
                    ),
                    
                    mainPanel(
                      plotOutput("resultOV", height = 800, width = 1200),
                      textOutput("blankOV"),
                      includeText("./textoverview/overview1.txt"),
                      br(),
                      br(),
                      includeText("./textoverview/overview2.txt"),
                      br(),
                      br(),
                      includeText("./textoverview/overview3.txt"),
                      br(),
                      br(),
                      includeText("./textoverview/overview4.txt")
                     

                    ))
                    
           ),
           
           
           tabPanel("Diabetes",
                    sidebarLayout(
                      sidebarPanel(
                        
                        
                        selectInput('diasex', 'Sex',  list('Male' = 1,
                                                           'Female' = 2, 
                                                           'Both' = 3)),
                        selectInput('diarace', 'Race',  list('White - Non-Hispanic' = 1,
                                                             'Black - Non-Hispanic ' = 2, 
                                                             'Hispanic' = 3, 
                                                             'Other races only, Non-Hispanic' = 4, 
                                                             'Multiracial, Non-Hispanic' = 5, 
                                                             'All' = 6)),
                        selectInput("diametric",
                                    label = "Parameter:",
                                    list("Diabetes" = 1,
                                         "Prediabetes" = 0),
                                    selected = 1)
                      ),
                      mainPanel(
                        plotOutput("result", height = 800, width = 1200),
                        br(),
                        br(),
                        includeText("./diabetestext/D1.txt"),
                        br(),
                        br(),
                        textOutput("blank"),
                        plotlyOutput("dresult2", height = 800, width = 1200),
                        br(),
                        br(),
                        includeText("./diabetestext/D2.txt"),
                        br(),
                        br(),
                        img(src = "Diabetes.png", height = 800, width = 1200),
                        br(),
                        br(),
                        includeText("./diabetestext/D1.txt")
                        
                      )
                    )
           ),
           
           tabPanel("Heart Disease",
                    sidebarLayout(
                      sidebarPanel(
                        
                        
                        selectInput('sex1', 'Sex',  list('Male' = 1,
                                                         'Female' = 2, 
                                                         'Both' = 3)),
                        selectInput('x.race.g11', 'Race',  list('White - Non-Hispanic' = 1,
                                                                'Black - Non-Hispanic ' = 2, 
                                                                'Hispanic' = 3, 
                                                                'Other races only, Non-Hispanic' = 4, 
                                                                'Multiracial, Non-Hispanic' = 5, 
                                                                'All' = 6)),
                        selectInput('variable1', 'Sugar Metric',  list('Respondents that have ever reported having coronary heart disease (CHD) or myocardial infarction (MI)' = 1,
                                                                       '(Ever told) you had a heart attack, also called a myocardial infarction?' = 2, 
                                                                       '(Ever told) you had a stroke?' = 3, 
                                                                       '(Ever told) you had angina or coronary heart disease?' = 4)
                        )
                      ),
                      mainPanel(
                        plotOutput("result1", height = 800, width = 1200),
                        br(),
                        br(),
                        includeText("./textheartdisease/H1.txt"),
                        br(),
                        br(),
                        textOutput("blank1"),
                        plotlyOutput("result2", height = 800, width = 1200),
                        br(),
                        br(),
                        includeText("./textheartdisease/H2.txt"),
                        br(),
                        br(),
                        img(src = "Heart.png", height = 800, width = 1200),
                        br(),
                        br(),
                        includeText("./textheartdisease/H2.txt")
                       
                        
                        
                      )
                    )
           ),
           tabPanel("Obesity",
                    sidebarLayout(
                      sidebarPanel(


                        selectInput('sexo', 'Sex',  list('Male' = 1,
                                                         'Female' = 2,
                                                         'Both' = 3)),
                        selectInput('raceo', 'Race',  list('White - Non-Hispanic' = 1,
                                                                'Black - Non-Hispanic ' = 2,
                                                                'Hispanic' = 3,
                                                                'Other races only, Non-Hispanic' = 4,
                                                                'Multiracial, Non-Hispanic' = 5,
                                                                'All' = 6))

                ),
                      mainPanel(
                        plotOutput("resultO", height = 800, width = 1200),
                        br(),
                        br(),
                        includeText("./obesitytext/O1.txt"),
                        br(),
                        br(),
                        textOutput("blankO"),
                        plotlyOutput("resultO2", height = 800, width = 1200),
                        br(),
                        br(),
                        includeText("./obesitytext/O2.txt"),
                        br(),
                        br(),
                        img(src = "Heart.png", height = 800, width = 1200),
                        br(),
                        br(),
                        includeText("./obesitytext/O3.txt"),
                        br(),
                        br(),
                        br(),
                        br(),
                        includeText("./obesitytext/O4.txt")
                      )
                    )
           ),
           tabPanel("Sugar Sweetened Beverage Consumption",
                    sidebarLayout(
                      sidebarPanel(
                        
                        
                        selectInput('sexs', 'Sex',  list('Male' = 1,
                                                         'Female' = 2, 
                                                         'Both' = 3)
                        ),
                        selectInput('races', 'Race',  list('White - Non-Hispanic' = 1,
                                                                'Black - Non-Hispanic ' = 2, 
                                                                'Hispanic' = 3, 
                                                                'Other race only, Non-Hispanic' = 4, 
                                                                'Multiracial, Non-Hispanic' = 5, 
                                                                'All' = 6)
                        ), 
                        selectInput('variables', 'Sugar Metric',  list('How often did you drink regular soda or pop that contains sugar?' = 1,
                                                                       'How often did you drink sugar-sweetened drinks?' = 2))),
                      mainPanel(
                        plotOutput("results", height = 800, width = 1200),
                        br(),
                        br(),
                        includeText("./ssbtexts/SSB1.txt"),
                        br(),
                        br(),
                        textOutput("blanks"),
                        plotlyOutput("results2", height = 800, width = 1200),
                        br(),
                        br(),
                        includeText("./ssbtexts/SSB2.txt"),
                        br(),
                        br(),
                        
                        textOutput("blanks2"),
                       
                        
                        img(src = "SSBs.png", height = 800, width = 1200),
                        br(),
                        br(),
                        includeText("./ssbtexts/SSB3.txt")
                        
                      )
                    )),
           
           tabPanel("Healthcare",
                    sidebarLayout(sidebarPanel(
                      
                      selectInput('hsex', 'Sex',  list('Male' = 1,
                                                       'Female' = 2, 
                                                       'Both' = 3)),
                      selectInput('hrace', 'Race',  list('White - Non-Hispanic' = 1,
                                                         'Black - Non-Hispanic ' = 2, 
                                                         'Hispanic' = 3, 
                                                         'Other race only, Non-Hispanic' = 4, 
                                                         'Multiracial, Non-Hispanic' = 5, 
                                                         'All' = 6)),
                      selectInput("hmetric",
                                  label = "Parameter:",
                                  list("Healthcare Coverage" = 0,
                                       "Availing the services of a personal doctor" = 1,
                                       "Adults who have good health (self reported):" = 2),
                                  selected = 1)),
                      
                      
                      
                      mainPanel(
                        plotOutput("hresult", height = 800, width = 1200),
                        br(),
                        br(),
                        includeText("./healthcaretexts/HC1.txt"),
                        br(),
                        br(),
                        textOutput("hblank"),
                        plotlyOutput("hresult2", height = 800, width = 1200),
                        br(),
                        br(),
                        includeText("./healthcaretexts/HC2.txt"),
                        br(),
                        br(),
                        
                        img(src = "Healthaccess.png", height = 800, width = 1200),
                        br(),
                        br(),
                        includeText("./healthcaretexts/HC3.txt")
                        
                        
                        
                      ))),
           tabPanel('The Team',
                    titlePanel("The Team"),
                    
                     
                      mainPanel(
                        img(src = "teamfinal.JPG", height = 800, width = 1200))))
                        