library(shiny)

shinyUI(pageWithSidebar(
  
  headerPanel("Tweetanalytics-Twitter data was retrieved at 15:40, on 31 May 2013, to increase speed of analysis. This can also be done by retrieving tweets instantly --- makes it slower than it already is"),
  
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
      tabPanel("Number of Tweets Retrieved",verbatimTextOutput("notweets")),
      tabPanel("Characters Per Tweet",plotOutput("charpertweet")),
      tabPanel("Words Per Tweet",plotOutput("wordpertweet")),
      tabPanel("Length of Words", plotOutput("lengthspertweet")),
      tabPanel("Unique Words",plotOutput("uniqspertweet")),
      tabPanel("# tags",plotOutput("hashspertweet")),
      tabPanel("@ Signs",plotOutput("atspertweet")),
      tabPanel("Web Links", plotOutput("linkspertweet")),
      tabPanel("All Variables Together",plotOutput("alltogether")),
      tabPanel("New Tweet Arrival Probability",plotOutput("arrivalprob"))
      
      )
    
    )
  ))
     
   