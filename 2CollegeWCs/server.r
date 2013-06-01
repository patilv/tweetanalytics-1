################## server.r for Generating word clouds for two universities' twitter accounts;menu of universities provided (data downloaded previously)
## functions based on the word cloud package, Jeff Stanton's book, and 
#Installing new packages

EnsurePackage<-function(x)
{x <- as.character(x)
 if (!require(x,character.only=TRUE))
 {
   install.packages(pkgs=x,repos="http://cran.r-project.org")
   require(x,character.only=TRUE)
 }
}

#loading packages

PrepareTwitter<-function()
{
  EnsurePackage("tm")
  EnsurePackage("wordcloud")
}
PrepareTwitter()


shinyServer(function(input, output) {
  
  #load data
  
  load("EWUdata.rda")
  load("GUdata.rda")
  load("SeattleUdata.rda")
  load("UWdata.rda")
  load("WSUdata.rda")
  
  # function for word cloud
  
  wordclouduniv<-function(myDatacleantext)
  {
    tweetCorpus<-Corpus(VectorSource(myDatacleantext))
    tweetTDM<-TermDocumentMatrix(tweetCorpus,control=list(removePunctuation=TRUE,
                                                          stopwords=c(stopwords('english'),"amp"),
                                                          removeNumbers=TRUE,tolower=TRUE))
    tdMatrix <- as.matrix(tweetTDM) # creating a data matrix
    sortedMatrix<-sort(rowSums(tdMatrix),decreasing=TRUE) # calculate row sum of each term and sort in descending order (high freq to low)
    cloudFrame<-data.frame(word=names(sortedMatrix),freq=sortedMatrix)#extracting names from named list in prev command and binding together into a dataframe with frequencies - called cloudFrame, names in separate columns
    
    wcuniv<-wordcloud(cloudFrame$word,cloudFrame$freq,colors=brewer.pal(8,"Dark2"),min.freq=2, max.words=100, scale=c(10,1), random.order=TRUE)
    print(wcuniv)
  }
  
  # function for comparative word cloud
  
  compwordcloud<-function(udata1,udata2){
    udata1vec<-paste(udata1, collapse=" ")
    udata2vec<-paste(udata2,collapse=" ")
    
    both<-c(udata1vec,udata2vec)
    tweetCorpus<-Corpus(VectorSource(both))
    tweetTDM<-TermDocumentMatrix(tweetCorpus,control=list(removePunctuation=TRUE,
                                                          stopwords=c(stopwords('english'),"amp"),
                                                          removeNumbers=TRUE,tolower=TRUE))
    tdMatrix <- as.matrix(tweetTDM) # creating a data matrix
    colnames(tdMatrix)<-c("college1","college2") #adding column names
    compuniv<-comparison.cloud(tdMatrix, random.order=FALSE, 
                               colors = c("blue", "red"),
                               min.freq=2, max.words=200, scale=c(10,1))
    
    print(compuniv)
    
  }
  
  #function for commonality word cloud
  
  commonwordcloud<-function(udata1,udata2){
    udata1vec<-paste(udata1, collapse=" ")
    udata2vec<-paste(udata2,collapse=" ")
    
    both<-c(udata1vec,udata2vec)
    tweetCorpus<-Corpus(VectorSource(both))
    tweetTDM<-TermDocumentMatrix(tweetCorpus,control=list(removePunctuation=TRUE,
                                                          stopwords=c(stopwords('english'),"amp"),
                                                          removeNumbers=TRUE,tolower=TRUE))
    tdMatrix <- as.matrix(tweetTDM) # creating a data matrix
    colnames(tdMatrix)<-c("college1","college2") #adding column names
    commonuniv<-commonality.cloud(tdMatrix, random.order=FALSE, 
                                  colors = brewer.pal(8,"Dark2"),
                                  scale=c(10,1))
    
    print(commonuniv)
    
  }
  
  udata1<-reactive({switch(input$College1,
                           "Gonzaga University" = GUdata,
                           "Eastern Washington University"= EWUdata, 
                           "Washington State University"=WSUdata,
                           "University of Washington" =UWdata,
                           "Seattle University"=SeattleUdata)} 
  )
  
  
  udata2<-reactive({switch(input$College2,
                           "Gonzaga University" = GUdata,
                           "Eastern Washington University"= EWUdata, 
                           "Washington State University"= WSUdata,
                           "University of Washington" =UWdata,
                           "Seattle University"=SeattleUdata)} 
  )
  
  output$college1wcplot <- renderPlot({wordclouduniv(udata1()$cleantext)})
  output$college2wcplot <- renderPlot({ wordclouduniv(udata2()$cleantext) })
  output$college1versus2wcplot<-renderPlot({compwordcloud(udata1()$cleantext,udata2()$cleantext)})
  output$college1and2wcplot<-renderPlot({commonwordcloud(udata1()$cleantext,udata2()$cleantext)})
  
})
