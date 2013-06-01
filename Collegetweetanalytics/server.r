# Ensuring packages are installed
EnsurePackage<-function(x)
{x <- as.character(x)
 if (!require(x,character.only=TRUE))
 {
   install.packages(pkgs=x,repos="http://cran.r-project.org")
   require(x,character.only=TRUE)
 }
}
PrepareTwitter<-function() # calling necessary packages
{
  EnsurePackage("ggplot2")
  EnsurePackage("reshape")
}
PrepareTwitter()

shinyServer(function(input, output) {
  
  #loading stored data files
  load("EWUdata.rda")
  load("GUdata.rda")
  load("SeattleUdata.rda")
  load("UWdata.rda")
  load("WSUdata.rda")
  
  # function to calculate number of tweets
  
  numoftweets<-function(udata1text,udata2text){
    u1tweets<-length(udata1text)
    u2tweets<-length(udata2text)
    notweets<-c(u1tweets,u2tweets)
    names(notweets)<-c("College 1","College 2")
    notweets
  } 
  
  #function to compute data frame of all vars of interest for the two universities selected - Gaston Sanchez's twitter mining project provide these separately
  
  tweetsboth<-function(udata1text,udata2text){
    
    charpertweetcoll1<-sapply(udata1text,nchar)
    charpertweetcoll2<-sapply(udata2text,nchar)
    
    # Words per Tweet - splitting word and counting # of words
    words_list1 = strsplit(udata1text, " ")
    words_list2 = strsplit(udata2text, " ")
    
    wordpertweetcoll1 = sapply(words_list1,length)
    wordpertweetcoll2 = sapply(words_list2,length)
    
    # length of words per tweet
    wsize_per_tweet1 = sapply(words_list1, function(x) mean(nchar(x)))
    wsize_per_tweet2 = sapply(words_list2, function(x) mean(nchar(x)))
    
    # how many unique words per tweet
    uniq_words_per_tweet1 = sapply(words_list1, function(x) length(unique(x)))
    uniq_words_per_tweet2 = sapply(words_list2, function(x) length(unique(x)))
    
    # how many hashtags per tweet
    hash_per_tweet1 = sapply(words_list1, function(x) length(grep("#", x)))
    hash_per_tweet2 = sapply(words_list2, function(x) length(grep("#", x)))
    
    # how many @mentions per tweet
    ats_per_tweet1 = sapply(words_list1, function(x) length(grep("@", x)))
    ats_per_tweet2 = sapply(words_list2, function(x) length(grep("@", x)))
    
    # how many http links per tweet
    links_per_tweet1 = sapply(words_list1, function(x) length(grep("http", x)))
    links_per_tweet2 = sapply(words_list2, function(x) length(grep("http", x)))
    
    #a data frame with all the calculated stuff 
    tweetsdf1 = data.frame(
      characters=charpertweetcoll1,
      words = wordpertweetcoll1,
      lengthOfWords = wsize_per_tweet1,
      uniqueWords = uniq_words_per_tweet1,
      hashTags = hash_per_tweet1,
      atSigns = ats_per_tweet1,
      Weblinks = links_per_tweet1
    )
    
    tweetsdf2 = data.frame(
      characters=charpertweetcoll2,
      words = wordpertweetcoll2,
      lengthOfWords = wsize_per_tweet2,
      uniqueWords = uniq_words_per_tweet2,
      hashTags = hash_per_tweet2,
      atSigns = ats_per_tweet2,
      Weblinks = links_per_tweet2
    )
    tweetsdf1$College<-c("College 1") #creating a factor variable for College
    tweetsdf2$College<-c("College 2")#creating a factor variable for College
    
    tweetsboth<-rbind(tweetsdf1,tweetsdf2) #joining the two data frames vertically (same column names, adding rows)
  }
  
  ArrivalProbability<-function(times, increment, max) # function from Jeffrey Stanton's book (2013)
  {
    # Initialize an empty vector
    plist <- NULL
    # Probability is defined over the size of this sample
    # of arrival times
    timeLen <- length(times)
    # May not be necessary, but checks for input mistake
    if (increment>max) {return(NULL)}
    for (i in seq(increment, max, by=increment))
    {
      # diff() requires a sorted list of times
      # diff() calculates the delays between neighboring times
      # the logical test <i provides a list of TRUEs and FALSEs
      # of length = timeLen, then sum() counts the TRUEs.
      # Divide by timeLen to calculate a proportion
      plist<-c(plist,(sum(as.integer(diff(times))<i))/timeLen)
    }
    return(plist)
  }      
  
  # function which calls the above arrival probability function by reordering arrival times
  arrivalprob<-function(udata1,udata2){
    
    u1sort<-udata1[order(as.integer(udata1$created)),]  #Ordering tweets based on arrival time
    u2sort<-udata2[order(as.integer(udata2$created)),]  #Ordering tweets based on arrival time
    
    u1diffcreated<-as.integer(diff(u1sort$created)) #calculate the difference in seconds between each pair of neighboring values. 
    u2diffcreated<-as.integer(diff(u2sort$created))
    
    #creating a data frame with arrival probabilities for both univs
    arriveprobu1<-ArrivalProbability(u1diffcreated,100,100000)
    arriveprobu2<-ArrivalProbability(u2diffcreated,100,100000)
    bothdatatimes<-data.frame(College1=arriveprobu1, College2=arriveprobu2)
    bothdatatimes$time<-c(1:nrow(bothdatatimes)) # creating an index variable
    arrivaldata<-melt(bothdatatimes,id.vars="time") #manipulating data frame to have it ready for ggplot2
  }
  
  # Reading in values for the two selected universities
  
  udata1<-reactive({udata1<-switch(input$College1,
                                   "Gonzaga University" = GUdata,
                                   "Eastern Washington University"= EWUdata, 
                                   "Washington State University"=WSUdata,
                                   "University of Washington" =UWdata,
                                   "Seattle University"=SeattleUdata)} 
  )
  
  
  udata2<-reactive({udata2<-switch(input$College2,
                                   "Gonzaga University" = GUdata,
                                   "Eastern Washington University"= EWUdata, 
                                   "Washington State University"= WSUdata,
                                   "University of Washington" =UWdata,
                                   "Seattle University"=SeattleUdata)} 
  )
  
  
  #creating dataframe of vars analyzed for the two university by calling the tweetsboth function - Note, raw tweets are used, not cleaned tweets
  tweetsbothdata<-reactive({tweetsboth(udata1()$text,udata2()$text)})
  
  # creating dataframe of arrival probabilities by calling the arrivalprob function
  arrivaldata<-reactive({arrivalprob(udata1(),udata2())})
  
  #tab 1 output - number of tweets for the two universities
  output$notweets<-renderPrint({numoftweets(udata1()$text,udata2()$text)})
  
  
  #tab 2 output -see ggtitle
  output$charpertweet<-renderPlot({cptplot<-ggplot(tweetsbothdata(),aes(x=College,y=characters,fill=College))+
                                     geom_bar(stat="identity", width=.5,position="dodge")+ggtitle("Number of characters per tweet")+
                                     labs(y= "Characters per tweet", x="College")+ 
                                     theme(axis.text.y = element_text(color="black"))+theme(axis.text.x = element_text(color="black"))+
                                     theme(legend.position="none")
                                   print(cptplot)})
  
  #tab3 output  -see ggtitle
  output$wordpertweet<-renderPlot({cptplot<-ggplot(tweetsbothdata(),aes(x=College,y=words,fill=College))+
                                     geom_bar(stat="identity", width=.5,position="dodge")+ggtitle("Number of words per tweet")+
                                     labs(y= "Words per tweet", x="College")+ 
                                     theme(axis.text.y = element_text(color="black"))+theme(axis.text.x = element_text(color="black"))+
                                     theme(legend.position="none")
                                   print(cptplot)})
  
  #tab4  -see ggtitle
  output$lengthspertweet<-renderPlot({cptplot<-ggplot(tweetsbothdata(),aes(x=College,y=lengthOfWords,fill=College))+
                                        geom_bar(stat="identity", width=.5,position="dodge")+ggtitle("Length of words per tweet")+
                                        labs(y= "Word length per tweet", x="College")+ 
                                        theme(axis.text.y = element_text(color="black"))+theme(axis.text.x = element_text(color="black"))+
                                        theme(legend.position="none")
                                      print(cptplot)})
  #tab5  -see ggtitle
  output$uniqspertweet<-renderPlot({cptplot<-ggplot(tweetsbothdata(),aes(x=College,y=uniqueWords,fill=College))+
                                      geom_bar(stat="identity", width=.5,position="dodge")+ggtitle("Unique words per tweet")+
                                      labs(y= "Unique words per tweet", x="College")+ 
                                      theme(axis.text.y = element_text(color="black"))+theme(axis.text.x = element_text(color="black"))+
                                      theme(legend.position="none")
                                    print(cptplot)})
  #tab 6  -see ggtitle
  output$hashspertweet<-renderPlot({cptplot<-ggplot(tweetsbothdata(),aes(x=College,y=hashTags,fill=College))+
                                      geom_bar(stat="identity", width=.5,position="dodge")+ggtitle("Hash tags per tweet")+
                                      labs(y= "# tags per tweet", x="College")+ 
                                      theme(axis.text.y = element_text(color="black"))+theme(axis.text.x = element_text(color="black"))+
                                      theme(legend.position="none")
                                    print(cptplot)})
  #tab 7  -see ggtitle
  output$atspertweet<-renderPlot({cptplot<-ggplot(tweetsbothdata(),aes(x=College,y=atSigns,fill=College))+
                                    geom_bar(stat="identity", width=.5,position="dodge")+ggtitle("Number of ats(@) per tweet")+
                                    labs(y= "@ signs per tweet", x="College")+ 
                                    theme(axis.text.y = element_text(color="black"))+theme(axis.text.x = element_text(color="black"))+
                                    theme(legend.position="none")
                                  print(cptplot)})
  #tab 8  -see ggtitle
  output$linkspertweet<-renderPlot({cptplot<-ggplot(tweetsbothdata(),aes(x=College,y=Weblinks,fill=College))+
                                      geom_bar(stat="identity", width=.5,position="dodge")+ggtitle("Number of web links per tweet")+
                                      labs(y= "Web links per tweet", x="College")+ 
                                      theme(axis.text.y = element_text(color="black"))+theme(axis.text.x = element_text(color="black"))+
                                      theme(legend.position="none")
                                    print(cptplot)})
  
  #tab 9  -see ggtitle
  output$alltogether<-renderPlot({moltendata<-melt(tweetsbothdata(),id.vars="College")
                                  cptplot<-ggplot(moltendata,aes(x=variable,y=value,fill=College))+
                                    geom_bar(stat="identity",position="dodge")+ggtitle("All Variables Together")+
                                    labs(y= "", x="All Variables")+ 
                                    theme(axis.text.y = element_text(color="black"))+theme(axis.text.x = element_text(color="black"))
                                  print(cptplot)
                                  
  })
  
  #tab 10  -see ggtitle
  output$arrivalprob<-renderPlot({cptplot<-ggplot(arrivaldata(),aes(x=time,y=value,color=variable))+geom_point()+ 
                                    labs(y= "Probability", x="Time (t)------>")+ theme(axis.text.y = element_text(color="black"))+
                                    theme(axis.text.x = element_text(color="black"))+ ggtitle("Probability of a new tweet arriving within a particular time, t")
                                  print(cptplot)})
  
})
