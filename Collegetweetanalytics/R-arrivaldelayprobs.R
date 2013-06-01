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
  Ensure Package("ggplot2")
}
PrepareTwitter()

ArrivalProbability<-function(times, increment, max)
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

#R Script - Create Vector of Probabilities From Delay Times
# Like ArrivalProbability, but works with unsorted list
# of delay times
DelayProbability<-function(delays, increment, max)
{
  # Initialize an empty vector
  plist <- NULL
  # Probability is defined over the size of this sample
  # of arrival times
  delayLen <- length(delays)
  # May not be necessary, but checks for input mistake
  if (increment>max) {return(NULL)}
  for (i in seq(increment, max, by=increment))
  {
    # logical test <=i provides list of TRUEs and FALSEs
    # of length = timeLen, then sum() counts the TRUEs
    plist<-c(plist,(sum(delays<=i)/delayLen))
  }
  return(plist)
}

load("EWUdata.rda")
load("GUdata.rda")

#Sorting based on Arrival Time
ewusort<-EWUdata[order(as.integer(EWUdata$created)), ] #Ordering tweets based on arrival time
gusort<-GUdata[order(as.integer(GUdata$created)), ] #Ordering tweets based on arrival time

ewudiffcreated<-as.integer(diff(ewusort$created)) #calculate the difference in seconds between each pair of neighboring values. 
gudiffcreated<-as.integer(diff(gusort$created))

arriveprobewu<-ArrivalProbability(ewudiffcreated,100,100000)
arriveprobgu<-ArrivalProbability(gudiffcreated,100,100000)
bothdatatimes<-data.frame(EWU=arriveprobewu, GU=arriveprobgu)
bothdatatimes$time<-c(1:nrow(bothdatatimes))

m<-melt(bothdatatimes,id.vars="time")
ggplot(m,aes(x=time,y=value,color=variable))+geom_jitter()

delayprobewu<-DelayProbability(ewudiffcreated,10,1000)
delayprobegu<-DelayProbability(gudiffcreated,10,1000)
bothdatadelay<-data.frame(EWU=delayprobewu, GU=delayprobegu)
bothdatadelay$time<-c(1:nrow(bothdatadelay))

molten<-melt(bothdatadelay,id.vars="time")
ggplot(molten,aes(x=time,y=value,color=variable))+geom_jitter()

