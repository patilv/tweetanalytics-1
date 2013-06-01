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

  load("EWUdata.rda")
  load("GUdata.rda")
  load("SeattleUdata.rda")
  load("UWdata.rda")
  load("WSUdata.rda")
  
# Number of tweets retrieved

gulen<-length (GUdata$cleantext)
ewulen<-length(EWUdata$cleantext)
guewulen<-c(gulen,ewulen)

names(guewulen) = c("GU", "EWU")
guewulen
# Chars per tweet (on not cleaned data)

chars_per_tweet<-sapply(EWUdata$text,nchar)
summary(charspertweet)
mean(charspertweet)
min(charspertweet)
max(charspertweet)
median(charspertweet)
qplot(charspertweet)
# Words per tweet
# split words
words_list = strsplit(EWUdata$text, " ")

# words per tweet
words_per_tweet = sapply(words_list, length)
# barplot
barplot(table(words_per_tweet), border=NA,
        main="Distribution of words per tweet", cex.main=1)
qplot(words_per_tweet)  
# length of words per tweet
wsize_per_tweet = sapply(words_list, function(x) mean(nchar(x)))
# barplot
barplot(table(round(wsize_per_tweet)), border=NA,
        xlab = "word length in number of characters",
        main="Distribution of words length per tweet", cex.main=1)
qplot(wsize_per_tweet)
# how many unique words per tweet
uniq_words_per_tweet = sapply(words_list, function(x) length(unique(x)))
# barplot
barplot(table(uniq_words_per_tweet), border=NA,
        main="Distribution of unique words per tweet", cex.main=1)
qplot(uniq_words_per_tweet)
# how many hashtags per tweet
hash_per_tweet = sapply(words_list, function(x) length(grep("#", x)))
table(hash_per_tweet)
prop.table(table(hash_per_tweet))

# how many @mentions per tweet
ats_per_tweet = sapply(words_list, function(x) length(grep("@", x)))
table(ats_per_tweet)
prop.table(table(ats_per_tweet))

# how many http links per tweet
links_per_tweet = sapply(words_list, function(x) length(grep("http", x)))
table(links_per_tweet)
prop.table(table(links_per_tweet))

#let's create a data frame with all the calculated stuff and make some plots
tweetsdf = data.frame(
  chars=chars_per_tweet,
  words = words_per_tweet,
  lengths = wsize_per_tweet,
  uniqs = uniq_words_per_tweet,
  hashs = hash_per_tweet,
  ats = ats_per_tweet,
  links = links_per_tweet
)

#The more words in a tweet, the more characters per word
# words -vs- chars
ggplot(tweetsdf, aes(x=words, y=chars)) +
  geom_point(colour="gray20", alpha=0.2) +
  stat_smooth(method="lm") +
  labs(x="number of words per tweet", y="number of characters per tweet") +
  ggtitle("Tweets from Univ \nNumber of words -vs- Number of characters")

#The more words in a tweet, the shorter the words
# words -vs- word length
ggplot(tweetsdf, aes(x=words, y=lengths)) +
  geom_point(colour="gray20", alpha=0.2) +
  stat_smooth(method="lm") +
  labs(x="number of words per tweet", y="size of words per tweet") +
  ggtitle("Tweets from Univ \nNumber of words -vs- Length of words")

#Lexical diversity: number of unique tokens / number of total tokens The lexical diversity reflects the range of diversity in vocabulary.

# unique words in total
uniq_words = unique(unlist(words_list))

# lexical diversity
length(uniq_words) / length(unlist(words_list))

#What are the most frequent words
# most frequent words
mfw = sort(table(unlist(words_list)), decreasing=TRUE)

# top-20 most frequent
top20 = head(mfw, 20)
qplot(top20)
# barplot
barplot(top20, border=NA, las=2, main="Top 20 most frequent terms", cex.main=1)
