EnsurePackage<-function(x)
{x <- as.character(x)
 if (!require(x,character.only=TRUE))
 {
   install.packages(pkgs=x,repos="http://cran.r-project.org")
   require(x,character.only=TRUE)
 }
}
PrepareTwitter<-function()
{
  EnsurePackage("bitops")
  EnsurePackage("RCurl")
  EnsurePackage("RJSONIO")
  EnsurePackage("twitteR")
  EnsurePackage("stringr")
  EnsurePackage("ROAuth")
  EnsurePackage("RCurl")
  EnsurePackage("modeest")
  EnsurePackage("tm")
  EnsurePackage("wordcloud")
}
PrepareTwitter()

  load("EWUdata.rda")
  load("GUdata.rda")
  load("SeattleUdata.rda")
  load("UWdata.rda")
  load("WSUdata.rda")
  
  wordclouduniv<-function(myDatacleantext)
  {
    tweetCorpus<-Corpus(VectorSource(myDatacleantext))
    tweetTDM<-TermDocumentMatrix(tweetCorpus,control=list(removePunctuation=TRUE,
                                                          stopwords=c(stopwords('english'),"amp"),
                                                          removeNumbers=TRUE,tolower=TRUE))
    #removing sparse terms - 1 allows 100%
    #tweetTDM<-removeSparseTerms(tweetTDM,.99)
    tdMatrix <- as.matrix(tweetTDM) # creating a data matrix
    sortedMatrix<-sort(rowSums(tdMatrix),decreasing=TRUE) # calculate row sum of each term and sort in descending order (high freq to low)
    cloudFrame<-data.frame(word=names(sortedMatrix),freq=sortedMatrix)#extracting names from named list in prev command and binding together into a dataframe with frequencies - called cloudFrame, names in separate columns
    
    wordcloud(cloudFrame$word,cloudFrame$freq,colors=brewer.pal(8,"Dark2"),min.freq=2, max.words=100, scale=c(10,1), random.order=TRUE)
    
  }
  
  compwordcloud<-function(udata1,udata2){
    udata1vec<-paste(udata1, collapse=" ")
    udata2vec<-paste(udata2,collapse=" ")
    
    both<-c(udata1vec,udata2vec)
    tweetCorpus<-Corpus(VectorSource(both))
    tweetTDM<-TermDocumentMatrix(tweetCorpus,control=list(removePunctuation=TRUE,
                                                          stopwords=c(stopwords('english'),"amp"),
                                                          removeNumbers=TRUE,tolower=TRUE))
    #removing sparse terms - 1 allows 100%
    #tweetTDM<-removeSparseTerms(tweetTDM,.99)
    tdMatrix <- as.matrix(tweetTDM) # creating a data matrix
    #sortedMatrix<-sort(rowSums(tdMatrix),decreasing=TRUE) # calculate row sum of each term and sort in descending order (high freq to low)
    colnames(tdMatrix)<-c("college1","college2") #adding column names
    comparison.cloud(tdMatrix, random.order=FALSE, 
                               colors = c("blue", "red"),
                     min.freq=2, max.words=200, scale=c(10,1))
    
    
    
  }
  
    
  #Tab 1 requirement
  
  wordclouduniv(GUdata$cleantext)
  wordclouduniv(EWUdata$cleantext)
  compwordcloud(GUdata$cleantext,EWUdata$cleantext)

  