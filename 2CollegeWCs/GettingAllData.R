# Installing package if not already installed (Stanton 2013)
EnsurePackage<-function(x)
{x <- as.character(x)
 if (!require(x,character.only=TRUE))
 {
   install.packages(pkgs=x,repos="http://cran.r-project.org")
   require(x,character.only=TRUE)
 }
}

#Identifying packages required  (Stanton 2013)
PrepareTwitter<-function()
{
  EnsurePackage("twitteR")
  EnsurePackage("stringr")
  EnsurePackage("ROAuth")
  EnsurePackage("RCurl")
}
PrepareTwitter()

load("credential") # A credential obtained from twitter permitting access to their data - A user will need this to proceed
# Please see http://cran.r-project.org/web/packages/twitteR/vignettes/twitteR.pdf for more info on this.

registerTwitterOAuth(credential)


# Function to create a data frame from tweets, Stanton 2013
TweetFrame<-function(userhandle, maxTweets)
{
  twtList<-userTimeline(userhandle,n=maxTweets,cainfo="cacert.pem")
  return(do.call("rbind",lapply(twtList,as.data.frame)))
}

# Function to clean tweets, Stanton 2013
CleanTweets<-function(tweets)
{
  # Remove redundant spaces
  tweets <- str_replace_all(tweets," "," ")
  # Get rid of URLs
  tweets <- str_replace_all(tweets, "http://t.co/[a-z,A-Z,0-9]*{8}","")
  # Take out retweet header, there is only one
  tweets <- str_replace(tweets,"RT @[a-z,A-Z]*: ","")
  # Get rid of hashtags
  tweets <- str_replace_all(tweets,"#[a-z,A-Z]*","")
  # Get rid of references to other screennames
  tweets <- str_replace_all(tweets,"@[a-z,A-Z]*","")
  return(tweets)
}

#Getting Raw Data #retrieved at 15:40, on 31 May 2013 

GUdata<-TweetFrame("GonzagaU",500)
SeattleUdata<-TweetFrame("seattleu",500)
WSUdata<-TweetFrame("WSUPullman",500)
UWdata<-TweetFrame("UW",500)
EWUdata<-TweetFrame("EWUEagles",500)

#cleaning tweets and assigning to new column

GUdata$cleantext<-CleanTweets(GUdata$text)
SeattleUdata$cleantext<-CleanTweets(SeattleUdata$text)
WSUdata$cleantext<-CleanTweets(WSUdata$text)
UWdata$cleantext<-CleanTweets(UWdata$text)
EWUdata$cleantext<-CleanTweets(EWUdata$text)

# saving files to local directory

save(GUdata,file="GUdata.rda")
save(SeattleUdata,file="SeattleUdata.rda")
save(WSUdata,file="WSUdata.rda")
save(UWdata,file="UWdata.rda")
save(EWUdata,file="EWUdata.rda")