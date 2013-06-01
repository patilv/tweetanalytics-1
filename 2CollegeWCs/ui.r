library(shiny)

shinyUI(pageWithSidebar(
  
  headerPanel("Word clouds of Tweets from Different Universities Will be Displayed.Tweetanalytics-Twitter data was retrieved at 15:40, on 31 May 2013, to increase speed of analysis. This can also be done by retrieving tweets instantly --- makes it slower than it already is"),
  
  # selection of colleges 
  sidebarPanel(
    selectInput("College1", "College 1:",
                choices =c(
                  "Gonzaga University","Eastern Washington University","Washington State University",
                  "University of Washington","Seattle University")),
    
    selectInput("College2", "College 2:",
                choices =c(
                  "Eastern Washington University","Gonzaga University","Washington State University",
                  "University of Washington","Seattle University"))    
  ),
  
  mainPanel(
    tabsetPanel(
      tabPanel("College 1",plotOutput("college1wcplot")),
      tabPanel("College 2",plotOutput("college2wcplot")),
      tabPanel("Comparison Word Cloud",plotOutput("college1versus2wcplot")),
      tabPanel("Commonality Word Cloud", plotOutput("college1and2wcplot"))   
    )
    
  )
))

   